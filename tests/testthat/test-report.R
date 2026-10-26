sections <- c("Source", "Parsed form", "Assumptions", "Derivative", "Verification",
              "Out of scope", "Conclusion")

section_positions <- function(txt) {
  vapply(sections, function(s) regexpr(paste0("\\section*{", s, "}"), txt, fixed = TRUE)[1], 0)
}

test_that("sections are present and ordered", {
  txt <- l2g_report(latex2grad("x^2", wrt = "x"))
  pos <- section_positions(txt)
  expect_true(all(pos > 0))
  expect_false(is.unsorted(pos))
})

test_that("source is reproduced verbatim and empty sections are explicit", {
  tex <- "x^{2} + \\sin\\left(x\\right)"
  txt <- l2g_report(latex2grad(tex, wrt = "x"))
  expect_match(txt, paste0("\\begin{verbatim}\n", tex, "\n\\end{verbatim}"), fixed = TRUE)
  expect_match(txt, "None: the input was read without applying any notational convention", fixed = TRUE)
  expect_match(txt, "None: every sub-term is in DefDiff's language.", fixed = TRUE)
  expect_match(txt, "\\textbf{CERTIFIED}", fixed = TRUE)
})

test_that("a parse failure still renders a complete report", {
  txt <- l2g_report(latex2grad("\\frac{1}{x", wrt = "x"))
  expect_true(all(section_positions(txt) > 0))
  expect_match(txt, "\\textbf{NOT CERTIFIED}", fixed = TRUE)
})

test_that("assumptions and out-of-scope terms are listed", {
  r <- latex2grad("2xy \\Gamma(x)", wrt = "x", domain = list(x = c(1, 2)))
  txt <- l2g_report(r)
  expect_match(txt, "\\begin{enumerate}", fixed = TRUE)
  expect_match(txt, "\\texttt{gamma}", fixed = TRUE)
})

test_that("the report is written to a file", {
  f <- tempfile(fileext = ".tex")
  out <- l2g_report(latex2grad("x^2", wrt = "x"), file = f)
  expect_true(file.exists(f))
  expect_identical(paste(readLines(f), collapse = "\n"), out)
})

test_that("reports compile with pdflatex", {
  skip_if(Sys.which("pdflatex") == "", "pdflatex not available")
  skip_on_cran()
  dir <- tempfile("l2g-report")
  dir.create(dir)
  hook <- structure(function(latex, problem) {
    list(latex = "\\sin(x) y", decisions = "read \u03bd as nu \u2264 \u2713")
  }, model = "m")
  r <- latex2grad("x^2 \\Gamma(x) + \\frac{y}{1 + x^2} + \\sin x y", wrt = c("x", "y"), type = "hessian",
                  domain = list(x = c(1, 2)), strict = TRUE, disambiguate = hook)
  expect_null(r$error)
  l2g_report(r, file = file.path(dir, "r.tex"))
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "r.tex"),
                    stdout = FALSE, stderr = FALSE)
  expect_identical(status, 0L)
  expect_true(file.exists("r.pdf"))
})

test_that("non-ASCII text is converted for LaTeX", {
  expect_identical(latex2grad:::.tex_text("read \u03bd \u2264 1"), "read $\\nu$ $\\le$ 1")
  expect_identical(latex2grad:::.tex_text("\u2713"), "[U+2713]")
  expect_true(any(grepl("[U+03BD]", latex2grad:::.verbatim("x \u03bd"), fixed = TRUE)))
})
