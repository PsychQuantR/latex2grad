test_that("out-of-scope term that depends on the variable is numeric", {
  r <- latex2grad("x^2 \\Gamma(x)", wrt = "x", domain = list(x = c(1, 3)))
  e <- r$derivative$entries$x
  expect_identical(e, quote(gamma(x) * 2 * x + x^2 * numeric_partial(gamma(x), "x")))
  expect_equal(nrow(r$out_of_scope), 1L)
  expect_identical(r$out_of_scope$head, "gamma")
  expect_identical(r$out_of_scope$treatment, "numeric")
  expect_match(r$out_of_scope$reason, "digamma")
  x <- 1.7
  expect_equal(unname(as_function(r)(x = x)), 2 * x * gamma(x) + x^2 * gamma(x) * digamma(x),
               tolerance = 1e-8)
  expect_true(r$certificate$pass)
})

test_that("out-of-scope term that does not depend on the variable is constant", {
  r <- latex2grad("\\Gamma(a) x^2", wrt = "x")
  expect_identical(r$out_of_scope$treatment, "constant w.r.t. x")
  expect_false(latex2grad:::.has_numeric_partial(r$derivative$entries$x))
})

test_that("the Student-t quantile is named with its reason", {
  r <- latex2grad("t_\\nu^{-1}(u)^2", wrt = "nu", domain = list(nu = c(3, 10), u = c(0.2, 0.8)))
  oos <- r$out_of_scope
  expect_identical(oos$head, "qt")
  expect_identical(oos$variable, "nu")
  expect_identical(oos$treatment, "numeric")
  expect_match(oos$reason, "no closed form")
  expect_true(r$certificate$pass)
})

test_that("numeric-partial nodes are differentiable again (Hessian)", {
  r <- latex2grad("x^2 \\Gamma(x)", wrt = "x", type = "hessian", domain = list(x = c(1, 2)))
  expect_true(latex2grad:::.contains_subtree(r$derivative$entries[[1]],
                                             quote(numeric_partial(gamma(x), "x", "x"))))
  x <- 1.4
  ref <- gamma(x) * (2 + 4 * x * digamma(x) + x^2 * (digamma(x)^2 + trigamma(x)))
  expect_equal(unname(as_function(r)(x = x))[1], ref, tolerance = 1e-6)
  expect_true(r$certificate$pass)
})

test_that("a formula DefDiff refuses becomes one numeric term and is not certified", {
  local_mocked_bindings(.defdiff_grad = function(f, v) stop("refused for the test"))
  r <- latex2grad("x^2", wrt = "x")
  expect_identical(r$derivative$entries$x, quote(numeric_partial(x^2, "x")))
  expect_identical(r$out_of_scope$head, "(whole formula)")
  expect_match(r$out_of_scope$reason, "refused for the test")
  expect_false(r$certificate$pass)
  expect_match(paste(r$certificate$failures, collapse = " "), "circular")
})

test_that("numeric_partial evaluates in the calling environment", {
  nu <- 3
  expect_equal(numeric_partial(gamma(nu), "nu"), gamma(3) * digamma(3), tolerance = 1e-9)
  a <- 0.7
  b <- 1.1
  expect_equal(numeric_partial(sin(a * b), "a", "b"), cos(a * b) - a * b * sin(a * b), tolerance = 1e-6)
})
