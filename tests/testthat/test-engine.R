num_grad <- function(expr, values, var) {
  env <- latex2grad:::.l2g_eval_env()
  g <- function(t) { v <- values; v[[var]] <- t; latex2grad:::.l2g_eval(expr, v, env) }
  latex2grad:::.l2g_richardson(g, values[[var]])
}
check_grad <- function(expr, values, tol = 1e-7) {
  r <- l2g_differentiate(expr, names(values))
  env <- latex2grad:::.l2g_eval_env()
  for (v in names(values)) {
    sym <- latex2grad:::.l2g_eval(r$derivatives[[v]], values, env)
    expect_equal(sym, num_grad(expr, values, v), tolerance = tol, info = deparse1(expr))
  }
  r
}

test_that("DefDiff differentiates in-language formulas without boundaries", {
  r <- check_grad(quote(x^2 * sin(y) + exp(x * y) / (1 + y^2)), list(x = 0.7, y = -0.4))
  expect_length(r$boundaries, 0L)
  expect_identical(r$derivatives$y, tex_normalize(r$derivatives$y))
  r <- check_grad(quote(atan(x / y) + sqrt(x) * log(y)), list(x = 1.3, y = 0.8))
  expect_length(r$boundaries, 0L)
  r <- check_grad(quote(x^y), list(x = 1.3, y = 0.8))  # symbolic exponent
  expect_identical(r$boundaries[[1L]]$fun, "^")
})

test_that("out-of-catalog functions get closed-form rules and are reported", {
  r <- check_grad(quote(lgamma((nu + 1) / 2) - lgamma(nu / 2)), list(nu = 4.5))
  expect_canon(r$derivatives$nu, quote(digamma((nu + 1) / 2) / 2 - digamma(nu / 2) / 2))
  b <- r$boundaries[[1L]]
  expect_identical(b$fun, "lgamma")
  expect_identical(b$kind, "closed form")
  expect_match(b$reason, "not in DefDiff")
  check_grad(quote(tan(x) + abs(x) + gamma(x) + beta(x, 2)), list(x = 0.6))
  check_grad(quote(pnorm(x) * dnorm(x) + qnorm(p) + erf(x)), list(x = 0.3, p = 0.8))
  check_grad(quote(pt(x, nu) + dt(x, nu)), list(x = 0.3, nu = 5.5), tol = 1e-6)
})

test_that("a node with no closed form becomes a numeric derivative node", {
  r <- check_grad(quote(qt(u, nu)), list(u = 0.8, nu = 6), tol = 1e-6)
  expect_identical(r$derivatives$u, quote(1 / dt(qt(u, nu), nu)))
  expect_identical(r$derivatives$nu, quote(numd(qt(u, s), s, nu)))
  kinds <- vapply(r$boundaries, `[[`, "", "kind")
  expect_setequal(kinds, c("closed form", "numeric"))
})

test_that("integrals: Leibniz rule from DefDiff, numeric fallback otherwise", {
  r <- check_grad(quote(integral(exp(-a * t^2), t, 0, x)), list(a = 0.7, x = 1.2), tol = 1e-6)
  expect_canon(r$derivatives$x, quote(exp(-(a * x^2))))
  expect_length(r$boundaries, 0L)
  r <- check_grad(quote(integral(gamma(t + a), t, 0, 1)), list(a = 1.5), tol = 1e-5)
  expect_identical(r$boundaries[[1L]]$fun, "integral")
  expect_match(r$boundaries[[1L]]$reason, "gamma")
})

test_that("variables that do not occur give 0 and nested numeric nodes do not shadow", {
  expect_identical(l2g_differentiate(quote(sin(y)), "x")$derivatives$x, 0)
  g <- l2g_differentiate(quote(qt(u, nu)), "nu")$derivatives$nu
  h <- l2g_differentiate(g, "nu")$derivatives$nu
  expect_false(identical(h[[3L]], g[[3L]]))
})

test_that("numd and erf evaluate", {
  expect_equal(numd(sin(s), s, 0), 1, tolerance = 1e-10)
  expect_equal(erf(0.5), 0.5204998778130465, tolerance = 1e-12)
  expect_equal(erfc(0.5), 1 - erf(0.5), tolerance = 1e-12)
})
