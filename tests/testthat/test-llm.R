fake_transport <- function(payload, capture = NULL, stop_reason = "end_turn") {
  function(url, headers, body) {
    if (!is.null(capture)) assign("request", list(url = url, headers = headers, body = body), envir = capture)
    list(model = body$model, stop_reason = stop_reason,
         content = list(list(type = "thinking", thinking = ""),
                        list(type = "text", text = jsonlite::toJSON(payload, auto_unbox = TRUE))))
  }
}

test_that("a disambiguation hook rewrites failing LaTeX and its decisions are recorded", {
  hook <- structure(function(latex, problem) {
    list(latex = "\\sin(x) y", decisions = "sin applies to x only")
  }, model = "test-model")
  r <- latex2grad("\\sin x y", wrt = "x", strict = TRUE, disambiguate = hook)
  expect_identical(r$parsed$canonical, quote(sin(x) * y))
  expect_true(any(grepl("[test-model] sin applies to x only", r$assumptions$text, fixed = TRUE)))
  expect_identical(r$source$rewritten, "\\sin(x) y")
  expect_true(r$certificate$pass)
})

test_that("without a hook, strict ambiguity is a failed (but reported) result", {
  r <- latex2grad("\\sin x y", wrt = "x", strict = TRUE)
  expect_false(r$certificate$pass)
  expect_match(r$error, "ambiguous")
})

test_that("llm_claude sends a structured-output request and parses the answer", {
  skip_if_not_installed("jsonlite")
  cap <- new.env()
  hook <- llm_claude(api_key = "sk-test",
                     transport = fake_transport(list(latex = "\\sin(x) \\cdot y", decisions = c("a", "b")), cap))
  expect_identical(attr(hook, "model"), "claude-opus-5-5")
  out <- hook("\\sin x y", "ambiguous notation")
  expect_identical(out$latex, "\\sin(x) \\cdot y")
  expect_identical(out$decisions, c("a", "b"))
  req <- cap$request
  expect_identical(req$url, "https://api.anthropic.com/v1/messages")
  expect_identical(req$headers$`x-api-key`, "sk-test")
  expect_identical(req$headers$`anthropic-version`, "2026-10-21")
  expect_match(req$headers$`anthropic-beta`, "server-side-fallback-2026-07-01", fixed = TRUE)
  expect_identical(req$body$model, "claude-opus-5-5")
  expect_identical(req$body$fallbacks, "default")
  expect_identical(req$body$output_config$format$type, "json_schema")
  expect_identical(req$body$output_config$effort, "high")
  expect_null(req$body$thinking)
  expect_match(req$body$messages[[1]]$content, "ambiguous notation", fixed = TRUE)
})

test_that("llm_claude surfaces refusals and API errors", {
  skip_if_not_installed("jsonlite")
  hook <- llm_claude(api_key = "k", transport = fake_transport(list(latex = "", decisions = list()),
                                                                stop_reason = "refusal"))
  expect_error(hook("x", "p"), "refusal")
  err <- llm_claude(api_key = "k", transport = function(url, headers, body) {
    structure(list(error = list(message = "bad key")), status = 401L)
  })
  expect_error(err("x", "p"), "bad key")
})

test_that("missing credentials fail only when the hook is called", {
  old <- Sys.getenv(c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"), unset = NA)
  Sys.unsetenv(c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"))
  hook <- llm_claude(api_key = "", transport = fake_transport(list(latex = "x", decisions = list())))
  expect_error(hook("x", "p"), "credentials")
  for (nm in names(old)) if (!is.na(old[[nm]])) do.call(Sys.setenv, stats::setNames(list(old[[nm]]), nm))
})

test_that("ocr_claude sends the image and the OCR entry is recorded", {
  skip_if_not_installed("jsonlite")
  img <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), img)
  cap <- new.env()
  ocr <- ocr_claude(api_key = "k", transport = fake_transport(
    list(latex = "x^{2} \\sin(x)", notes = "the exponent may be a 2 or a z"), cap))
  r <- latex2grad(image = img, ocr = ocr, wrt = "x")
  blocks <- cap$request$body$messages[[1]]$content
  expect_identical(blocks[[1]]$type, "image")
  expect_identical(blocks[[1]]$source$media_type, "image/png")
  expect_identical(blocks[[1]]$source$data, jsonlite::base64_enc(readBin(img, "raw", 8)))
  expect_identical(r$source$latex, "x^{2} \\sin(x)")
  expect_identical(r$source$ocr_model, "claude-opus-5-5")
  expect_true(any(r$assumptions$kind == "ocr"))
  expect_true(any(grepl("exponent may be", r$assumptions$text)))
  expect_match(l2g_report(r), "\\includegraphics", fixed = TRUE)
  expect_true(r$certificate$pass)
})

test_that("the model that served a fallback is recorded (review of #2)", {
  skip_if_not_installed("jsonlite")
  transport <- function(url, headers, body) {
    list(model = "claude-fallback-x", stop_reason = "end_turn",
         content = list(list(type = "text", text = jsonlite::toJSON(
           list(latex = "\\sin(x) y", decisions = "sin applies to x"), auto_unbox = TRUE))))
  }
  r <- latex2grad("\\sin x y", wrt = "x", strict = TRUE,
                  disambiguate = llm_claude(api_key = "k", transport = transport))
  expect_true(any(grepl("[claude-fallback-x (fallback for claude-opus-5-5)]", r$assumptions$text, fixed = TRUE)))
})

test_that("where-definitions go through the disambiguation hook in strict mode", {
  seen <- character()
  hook <- structure(function(latex, problem) {
    seen <<- c(seen, latex)
    list(latex = "\\operatorname{qt}(u_1, \\nu)", decisions = "t quantile")
  }, model = "test-model")
  r <- latex2grad("x_1 + \\nu", wrt = "nu", where = c(x_1 = "t_\\nu^{-1}(u_1)"), strict = TRUE,
                  disambiguate = hook, domain = list(nu = c(3, 9), u_1 = c(0.2, 0.8)))
  expect_null(r$error)
  expect_identical(seen, "t_\\nu^{-1}(u_1)")
  expect_true(any(grepl("In the definition of `x_1`: [test-model] t quantile", r$assumptions$text, fixed = TRUE)))
})

test_that("images are recorded only when OCR ran, and non-embeddable ones are named", {
  expect_null(latex2grad("x^2", wrt = "x", image = "unused.png")$source$image)
  gif <- tempfile(fileext = ".gif")
  writeBin(as.raw(1:8), gif)
  ocr <- structure(function(path) list(latex = "x^2", notes = character()), model = "m")
  rep <- l2g_report(latex2grad(image = gif, ocr = ocr, wrt = "x"))
  expect_false(grepl("includegraphics", rep, fixed = TRUE))
  expect_match(rep, "not embedded", fixed = TRUE)
})
