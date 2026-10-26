# The entry assistant is exercised with a fake transport: no network.
fake_post <- function(reply, log = NULL) {
  function(body) {
    if (!is.null(log)) log$bodies <- c(log$bodies, list(body))
    if (is.character(reply)) {
      return(list(stop_reason = "end_turn", content = list(list(type = "text", text = reply))))
    }
    reply
  }
}

test_that("requests use structured output, effort and fallbacks", {
  log <- new.env(); log$bodies <- list()
  llm <- claude_entry(post = fake_post('{"latex": "x^{2}"}', log))
  expect_identical(llm$rewrite("x^^2", "bad"), "x^{2}")
  b <- log$bodies[[1L]]
  expect_identical(b$model, "claude-opus-5-5")
  expect_identical(b$output_config$format$type, "json_schema")
  expect_identical(b$fallbacks, "default")
  expect_null(b$temperature)
  expect_null(b$thinking)
})

test_that("resolve keeps only valid options", {
  reply <- '{"choices": [{"id": "letters@1", "option": "single name"}, {"id": "bogus", "option": "x"}]}'
  llm <- claude_entry(post = fake_post(reply))
  amb <- tex_parse("xy")$ambiguities
  expect_identical(llm$resolve("xy", amb), list(`letters@1` = "single name"))
})

test_that("the pipeline records the assistant's decisions", {
  reply <- '{"choices": [{"id": "letters@1", "option": "single name"}]}'
  llm <- claude_entry(post = fake_post(reply))
  r <- latex2grad("xy^2", llm = llm, n_points = 3)
  expect_identical(r$parsed$expr, quote(xy^2))
  expect_identical(r$assumptions[[1L]]$source, "LLM (claude-opus-5-5)")
})

test_that("a parse error is rewritten once by the assistant", {
  llm <- list(name = "stub", rewrite = function(latex, error) "x^{2}")
  r <- latex2grad("x^^2", llm = llm, n_points = 3)
  expect_identical(r$source$used_latex, "x^{2}")
  expect_true(any(grepl("rewritten", vapply(r$assumptions, `[[`, "", "text"))))
})

test_that("OCR feeds the pipeline", {
  img <- tempfile(fileext = ".png"); writeBin(as.raw(1:10), img); on.exit(unlink(img))
  log <- new.env(); log$bodies <- list()
  llm <- claude_entry(post = fake_post('{"latex": "$\\\\sin x$"}', log))
  r <- latex2grad(image = img, llm = llm, n_points = 3)
  expect_identical(r$source$latex, "\\sin x")
  expect_identical(log$bodies[[1L]]$messages[[1L]]$content[[1L]]$type, "image")
})

test_that("refusals and truncation are errors, not guesses", {
  refused <- list(stop_reason = "refusal", stop_details = list(category = "cyber"), content = list())
  expect_error(claude_entry(post = fake_post(refused))$rewrite("x", "e"), "declined",
               class = "latex2grad_entry_error")
  cut <- list(stop_reason = "max_tokens", content = list(list(type = "text", text = "{")))
  expect_error(claude_entry(post = fake_post(cut))$rewrite("x", "e"), "max_tokens",
               class = "latex2grad_entry_error")
})

test_that("a missing key is reported", {
  skip_if_not_installed("httr2")
  expect_error(claude_entry(api_key = ""), "API key", class = "latex2grad_entry_error")
})
