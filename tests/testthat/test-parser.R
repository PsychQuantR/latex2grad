p <- function(s, ...) tex_parse(s, ...)$expr
pn <- function(s, ...) tex_normalize(tex_parse(s, ...)$expr)

test_that("arithmetic, fractions, roots and powers", {
  expect_canon(pn("x^2 + 2x - 1"), quote(x^2 + 2 * x - 1))
  expect_canon(pn("\\frac{a}{b}"), quote(a / b))
  expect_canon(pn("\\frac12 x"), quote(x / 2))
  expect_canon(pn("\\sqrt{x}"), quote(sqrt(x)))
  expect_canon(pn("\\sqrt[3]{x}"), quote(x^(1 / 3)))
  expect_canon(pn("a \\cdot b \\times c"), quote(a * b * c))
  expect_canon(pn("x^{a+b}"), quote(x^(a + b)))
  expect_canon(pn("-x^2"), quote(-x^2))
})

test_that("LaTeX takes one character as an unbraced script", {
  expect_canon(pn("x^23"), quote(3 * x^2))
  expect_canon(pn("x_12"), quote(2 * x_1))
})

test_that("delimiters and absolute values", {
  expect_canon(pn("\\left(1 + x\\right)^{2}"), quote((1 + x)^2))
  expect_canon(pn("|x - 1| + \\left|y\\right|"), quote(abs(x - 1) + abs(y)))
  expect_canon(pn("\\{a + b\\} c"), quote((a + b) * c))
})

test_that("Greek letters, subscripts and named symbols", {
  expect_canon(pn("\\alpha_1 + \\sigma_{xy} + x_{ij}"), quote(alpha_1 + sigma_xy + x_ij))
  expect_canon(pn("\\mathrm{rate} \\cdot t"), quote(rate * t))
  expect_canon(pn("\\pi r^2"), quote(pi * r^2))
})

test_that("elementary functions with and without parentheses", {
  expect_canon(pn("\\sin x + \\cos(y)"), quote(sin(x) + cos(y)))
  expect_canon(pn("\\sin^2 x"), quote(sin(x)^2))
  expect_canon(pn("\\sin^{-1} x"), quote(asin(x)))
  expect_canon(pn("\\ln x + \\exp(x)"), quote(log(x) + exp(x)))
  expect_canon(pn("\\log_2 x"), quote(log(x) / log(2)))
  expect_canon(pn("\\cot x"), quote(1 / tan(x)))
  expect_canon(pn("\\sin x \\cos x"), quote(sin(x) * cos(x)))
})

test_that("special functions", {
  expect_canon(pn("\\Gamma(x) + \\ln\\Gamma(y)"), quote(gamma(x) + lgamma(y)))
  expect_canon(pn("\\psi(x) + \\psi_1(x)"), quote(digamma(x) + trigamma(x)))
  expect_canon(pn("\\Phi(x) \\phi(x)"), quote(pnorm(x) * dnorm(x)))
  expect_canon(pn("\\Phi^{-1}(p)"), quote(qnorm(p)))
  expect_canon(pn("t_{\\nu}^{-1}(u)"), quote(qt(u, nu)))
  expect_canon(pn("\\mathrm{B}(a, b) + \\operatorname{erf}(x)"), quote(beta(a, b) + erf(x)))
  expect_canon(pn("\\operatorname{pt}(x, \\nu)"), quote(pt(x, nu)))
  expect_canon(pn("n!"), quote(gamma(n + 1)))
  expect_canon(pn("\\binom{n}{k}"), quote(choose(n, k)))
})

test_that("e is Euler's number unless declared", {
  expect_canon(pn("e^{-x^2}"), quote(exp(-x^2)))
  expect_canon(pn("\\mathrm{e}^{x}"), quote(exp(x)))
  expect_canon(pn("f(e) = e^2"), quote(e^2))
})

test_that("integrals, sums and products", {
  expect_canon(pn("\\int_0^x e^{-t^2} \\, dt"), quote(integral(exp(-t^2), t, 0, x)))
  expect_canon(pn("\\int_{0}^{1} \\mathrm{d}t \\, t^a"), quote(integral(t^a, t, 0, 1)))
  expect_canon(pn("\\int_{-\\infty}^{x} \\phi(t) dt"), quote(integral(dnorm(t), t, -Inf, x)))
  expect_canon(pn("\\sum_{i=1}^{3} x_i^2"), quote(x_1^2 + x_2^2 + x_3^2))
  expect_canon(pn("\\prod_{k=1}^{2} (1 + a_k)"), quote((1 + a_1) * (1 + a_2)))
  expect_error(tex_parse("\\sum_{i=1}^{n} x_i"), class = "latex2grad_parse_error")
  expect_error(tex_parse("\\int x dx"), class = "latex2grad_parse_error")
})

test_that("evaluation bars become numeric derivative nodes", {
  expect_identical(
    pn("\\left.\\frac{\\partial}{\\partial s} \\operatorname{qt}(u, s)\\right|_{s=\\nu}"),
    quote(numd(qt(u, s), s, nu)))
})

test_that("vectors parse to c(...)", {
  expect_canon(pn("\\begin{pmatrix} x \\\\ y^2 \\end{pmatrix}"), quote(c(x, y^2)))
  expect_canon(pn("\\begin{bmatrix} x & y \\end{bmatrix}"), quote(c(x, y)))
})

test_that("equations give a signature", {
  r <- tex_parse("c(u_1, u_2; \\rho, \\nu) = \\rho u_1")
  expect_identical(r$signature$name, "c")
  expect_identical(r$signature$vars, c("u_1", "u_2"))
  expect_identical(r$signature$params, c("rho", "nu"))
  r <- tex_parse("\\log p(x) = -x")
  expect_null(r$signature$name)
  expect_length(r$notes, 1L)
  expect_error(tex_parse("a = b = c"), class = "latex2grad_parse_error")
})

test_that("ambiguities are recorded with options and can be overridden", {
  r <- tex_parse("\\sin x y")
  expect_identical(r$expr, quote(sin(x * y)))
  a <- r$ambiguities[[1L]]
  expect_identical(a$kind, "function-scope")
  expect_identical(a$chosen, "whole product")
  expect_true("first factor only" %in% a$options)
  r2 <- tex_parse("\\sin x y", choices = setNames(list("first factor only"), a$id))
  expect_identical(tex_normalize(r2$expr), quote(sin(x) * y))
  expect_identical(r2$ambiguities[[1L]]$source, "resolver")

  r <- tex_parse("xy")
  expect_identical(r$expr, quote(x * y))
  id <- r$ambiguities[[1L]]$id
  expect_identical(tex_parse("xy", choices = setNames(list("single name"), id))$expr, quote(xy))
  # one record for a run of letters, not one per letter
  expect_length(tex_parse("xyz")$ambiguities, 1L)

  r <- tex_parse("\\log x")
  id <- r$ambiguities[[1L]]$id
  expect_identical(tex_normalize(tex_parse("\\log x", choices = setNames(list("base-10 logarithm"), id))$expr),
                   quote(log(x) / log(10)))
})

test_that("plain function words are recognised", {
  expect_canon(pn("sin(x) + exp(2 x)"), quote(sin(x) + exp(2 * x)))
})

test_that("parse errors are typed", {
  expect_error(tex_parse("x +"), class = "latex2grad_parse_error")
  expect_error(tex_parse("\\foo x"), class = "latex2grad_parse_error")
  expect_error(tex_parse("f'(x)"), class = "latex2grad_parse_error")
  expect_error(tex_parse("(x"), class = "latex2grad_parse_error")
})
