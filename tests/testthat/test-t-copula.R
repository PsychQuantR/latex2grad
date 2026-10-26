t_copula <- paste0(
  "c(u_1, u_2) = \\frac{\\Gamma\\left(\\frac{\\nu+2}{2}\\right)\\Gamma\\left(\\frac{\\nu}{2}\\right)}",
  "{\\sqrt{1-\\rho^2}\\,\\Gamma\\left(\\frac{\\nu+1}{2}\\right)^2}\\,",
  "\\frac{\\left(1+\\frac{x_1^2-2\\rho x_1 x_2+x_2^2}{\\nu(1-\\rho^2)}\\right)^{-\\frac{\\nu+2}{2}}}",
  "{\\left(1+\\frac{x_1^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}\\left(1+\\frac{x_2^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}}"
)

# Independent reference: bivariate t density over the product of the margins.
copula_ref <- function(u1, u2, rho, nu) {
  x1 <- qt(u1, nu)
  x2 <- qt(u2, nu)
  q <- (x1^2 - 2 * rho * x1 * x2 + x2^2) / (1 - rho^2)
  joint <- gamma((nu + 2) / 2) / (gamma(nu / 2) * nu * pi * sqrt(1 - rho^2)) * (1 + q / nu)^(-(nu + 2) / 2)
  joint / (dt(x1, nu) * dt(x2, nu))
}

fit <- function() {
  latex2grad(t_copula, wrt = "nu",
             where = c(x_1 = "t_\\nu^{-1}(u_1)", x_2 = "t_\\nu^{-1}(u_2)"),
             domain = list(nu = c(3, 12), rho = c(-0.8, 0.8), u_1 = c(0.1, 0.9), u_2 = c(0.1, 0.9)))
}

test_that("the parsed density equals the reference t-copula density", {
  r <- fit()
  for (pt in list(c(0.2, 0.7, 0.3, 5), c(0.9, 0.15, -0.5, 9))) {
    vals <- list(u_1 = pt[1], u_2 = pt[2], rho = pt[3], nu = pt[4])
    expect_equal(latex2grad:::.eval_tree(r$parsed$canonical, vals),
                 copula_ref(pt[1], pt[2], pt[3], pt[4]), tolerance = 1e-12)
  }
})

test_that("d/d nu of the t-copula density is certified, with the boundary named", {
  r <- fit()
  expect_null(r$error)
  expect_true(r$certificate$pass)
  expect_lte(r$certificate$numeric$max_rel_error, 1e-6)
  oos <- r$out_of_scope
  expect_identical(sort(oos$head), c("gamma", "gamma", "gamma", "qt", "qt"))
  expect_true(all(oos$treatment == "numeric"))
  expect_identical(r$parameters, c("rho", "u_1", "u_2"))
  # against an independent numeric derivative of the reference density
  for (pt in list(c(0.2, 0.7, 0.3, 5), c(0.6, 0.4, -0.2, 8))) {
    g <- function(nu) copula_ref(pt[1], pt[2], pt[3], nu)
    ref <- latex2grad:::.richardson(g, pt[4])
    got <- as_function(r)(u_1 = pt[1], u_2 = pt[2], rho = pt[3], nu = pt[4])
    expect_equal(unname(got), ref, tolerance = 1e-7)
  }
})

test_that("the t-copula report shows folded definitions and abbreviations", {
  r <- fit()
  txt <- l2g_report(r)
  expect_match(txt, "x_{1} &= t_{\\nu}^{-1}\\left(u_{1}\\right)", fixed = TRUE)
  expect_match(txt, "A_{1} &=", fixed = TRUE)
  expect_match(txt, "\\textbf{CERTIFIED}", fixed = TRUE)
})
