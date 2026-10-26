test_that("gradient report passes and renders", {
  r <- latex2grad("f(x, y) = x^2 \\sin y", n_points = 5)
  expect_s3_class(r, "latex2grad_report")
  expect_true(r$pass)
  expect_identical(r$wrt, c("x", "y"))
  expect_identical(r$derivatives[[1L]]$expr, quote(2 * sin(y) * x))
  expect_true(r$verification$round_trip$ok)
  expect_match(r$conclusion, "^Verified")
  tex <- format(r, "latex")
  for (sec in c("Source", "Parsed form", "Assumptions", "Derivative", "Verification",
                "Out of scope", "Conclusion")) {
    expect_match(tex, paste0("\\\\section\\*\\{", sec, "\\}"))
  }
  expect_match(format(r, "markdown"), "## Verification")
  expect_output(print(r), "Conclusion")
  f <- tempfile(fileext = ".tex"); on.exit(unlink(f))
  write_report(r, f)
  expect_true(file.exists(f))
})

test_that("Hessian and Jacobian", {
  r <- latex2grad("x^2 y + \\sin(x y)", op = "hessian", n_points = 5)
  expect_true(r$pass)
  expect_length(r$derivatives, 4L)
  r <- latex2grad("\\begin{pmatrix} r\\cos\\theta \\\\ r \\sin\\theta \\end{pmatrix}",
                  wrt = c("r", "\\theta"), n_points = 5)
  expect_identical(r$op, "jacobian")
  expect_true(r$pass)
  expect_canon(r$derivatives[[2L]]$expr, quote(-(r * sin(theta))))
})

test_that("assumptions record grammar choices and defaults", {
  r <- latex2grad("\\sin x y", n_points = 3)
  texts <- vapply(r$assumptions, `[[`, "", "text")
  expect_true(any(grepl("whole product", texts)))
  expect_true(any(grepl("every free symbol", texts)))
  r <- latex2grad("\\sin x y", wrt = "x", n_points = 3,
                  choices = list(`fnarg@1` = "first factor only"))
  expect_identical(r$parsed$expr, quote(sin(x) * y))
  expect_identical(r$assumptions[[1L]]$source, "user choice")
})

test_that("out-of-scope terms are reported, the worked example verifies", {
  copula <- paste0(
    "c(u_1, u_2; \\rho, \\nu) = ",
    "\\frac{\\Gamma\\left(\\frac{\\nu+2}{2}\\right) \\Gamma\\left(\\frac{\\nu}{2}\\right)}",
    "{\\Gamma\\left(\\frac{\\nu+1}{2}\\right)^{2} \\sqrt{1-\\rho^{2}}}",
    "\\frac{\\left(1 + \\frac{t_{\\nu}^{-1}(u_1)^{2} + t_{\\nu}^{-1}(u_2)^{2}",
    " - 2\\rho\\, t_{\\nu}^{-1}(u_1)\\, t_{\\nu}^{-1}(u_2)}{\\nu (1-\\rho^{2})}\\right)^{-\\frac{\\nu+2}{2}}}",
    "{\\left(1 + \\frac{t_{\\nu}^{-1}(u_1)^{2}}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}",
    " \\left(1 + \\frac{t_{\\nu}^{-1}(u_2)^{2}}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}}")
  r <- latex2grad(copula, wrt = "nu", n_points = 5,
                  domain = list(u_1 = c(0.05, 0.95), u_2 = c(0.05, 0.95),
                                rho = c(-0.8, 0.8), nu = c(2, 30)))
  expect_true(r$pass)
  funs <- vapply(r$boundaries, `[[`, "", "fun")
  expect_setequal(funs, c("gamma", "qt", "^"))
  qt_b <- r$boundaries[[which(funs == "qt")]]
  expect_identical(qt_b$kind, "numeric")
  expect_true(r$derivatives[[1L]]$numeric_node)
  expect_match(r$conclusion, "numeric derivative nodes")
  expect_match(format(r, "latex"), "where")  # long result is abbreviated for display
})

test_that("a failed check still produces a report", {
  r <- latex2grad("\\ln x", domain = list(x = c(-2, -1)), n_points = 3)
  expect_false(r$pass)
  expect_identical(r$verification$numeric$n_points, 0L)
  expect_match(r$conclusion, "^NOT verified")
  expect_match(format(r, "latex"), "FAIL")
})

test_that("the global RNG state is restored", {
  set.seed(42); a <- runif(1)
  set.seed(42); latex2grad("x^2", n_points = 3); b <- runif(1)
  expect_identical(a, b)
})

test_that("input errors", {
  expect_error(latex2grad("x^2", op = "jacobian", wrt = "x", n_points = 2), NA)
  expect_error(latex2grad("\\begin{pmatrix} x \\\\ y \\end{pmatrix}", op = "grad"),
               class = "latex2grad_entry_error")
  expect_error(latex2grad("3 + 4"), class = "latex2grad_entry_error")
  expect_error(latex2grad(image = "nope.png"), class = "latex2grad_entry_error")
})
