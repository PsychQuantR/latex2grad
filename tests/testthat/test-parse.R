tree <- function(tex, ...) normalize_tree(parse_latex(tex, ...)$expr)

test_that("explicit formula parses without assumptions", {
  p <- parse_latex("\\frac{1}{2} \\cdot x^{2} + \\sin\\left(y\\right)")
  expect_identical(normalize_tree(p$expr), normalize_tree(quote((1/2) * x^2 + sin(y))))
  expect_equal(nrow(p$assumptions), 0L)
  expect_setequal(p$symbols, c("x", "y"))
})

test_that("Greek letters and subscripts become symbol names", {
  expect_identical(all.vars(tree("x_1 + x_{12} + \\sigma_{x} + \\nu")),
                   c("x_1", "x_12", "sigma_x", "nu"))
  expect_identical(tree("\\pi x"), quote(pi * x))
})

test_that("unsupported constructs raise latex2grad_parse_error", {
  expect_error(parse_latex("\\sum_{i=1}^{n} x_i"), class = "latex2grad_parse_error")
  expect_error(parse_latex("\\sum_{i=1}^{n} x_i"), "symbolic bounds")
  expect_error(parse_latex("\\infty"), class = "latex2grad_parse_error")
  expect_error(parse_latex("f'"), class = "latex2grad_parse_error")
  expect_error(parse_latex("\\frac{\\partial f}{\\partial x}"), class = "latex2grad_parse_error")
  expect_error(parse_latex("x^2^3"), "double superscript")
  expect_error(parse_latex("\\frac{1}{x"), class = "latex2grad_parse_error")
  expect_error(parse_latex(""), class = "latex2grad_parse_error")
  expect_error(parse_latex("\\operatorname{system}(x)"), "unknown function")
})

test_that("implicit multiplication is recorded", {
  p <- parse_latex("2xy")
  expect_identical(normalize_tree(p$expr), quote(2 * x * y))
  expect_true(any(p$assumptions$kind == "letters"))
  expect_match(p$assumptions$text[p$assumptions$kind == "letters"], "`xy`")
})

test_that("an unbraced function argument absorbs the implicit product", {
  p <- parse_latex("\\sin x y")
  expect_identical(p$expr, quote(sin(x * y)))
  expect_true("arg_scope" %in% p$assumptions$kind)
  expect_identical(tree("\\sin x \\cos y"), quote(sin(x) * cos(y)))
  expect_identical(tree("\\sin(x) y"), quote(sin(x) * y))
  expect_identical(tree("\\sin 2x"), quote(sin(2 * x)))
})

test_that("strict mode turns conventions into ambiguity errors", {
  expect_error(parse_latex("\\sin x y", strict = TRUE), class = "latex2grad_ambiguity")
  expect_error(parse_latex("xy", strict = TRUE), class = "latex2grad_ambiguity")
  expect_error(parse_latex("e^x", strict = TRUE), class = "latex2grad_ambiguity")
  expect_silent(parse_latex("2 x \\cdot \\sin\\left(y\\right)", strict = TRUE))
})

test_that("documented conventions", {
  expect_identical(tree("a/bc"), call("/", quote(a), quote(b * c)))
  expect_identical(tree("\\sin^2 x"), quote(sin(x)^2))
  expect_identical(tree("\\sin^{-1} x"), quote(asin(x)))
  expect_identical(tree("e^{x}"), quote(exp(x)))
  expect_identical(tree("e^{x}", vars = "e"), quote(e^x))
  expect_identical(tree("\\Phi(x)"), quote(pnorm(x)))
  expect_identical(tree("t_\\nu^{-1}(u_1)"), quote(qt(u_1, nu)))
  expect_identical(tree("t_{3}^{-1}(u)"), quote(qt(u, 3)))
  expect_identical(tree("sin x"), quote(sin(x)))
  expect_identical(tree("f(x)", functions = c(f = "dnorm")), quote(dnorm(x)))
  p <- parse_latex("\\nu(1-\\rho^2)")
  expect_identical(normalize_tree(p$expr), call("*", quote(nu), quote(1 - rho^2)))
  expect_true("symbol_paren" %in% p$assumptions$kind)
})

test_that("finite sums and products expand", {
  expect_identical(tree("\\sum_{i=1}^{3} x_i^2"), quote(x_1^2 + x_2^2 + x_3^2))
  expect_identical(tree("\\prod_{k=1}^{2} (1 + k y)"), call("*", quote(1 + y), quote(1 + 2 * y)))
  expect_identical(tree("\\sum_{i=1}^{2} \\sum_{j=1}^{2} a_{ij}"), quote(a_11 + a_12 + a_21 + a_22))
})

test_that("functions, roots, absolute values, binomials", {
  expect_identical(tree("\\sqrt[3]{x}"), normalize_tree(quote(x^(1/3))))
  expect_identical(tree("\\sqrt{x}"), quote(sqrt(x)))
  expect_identical(tree("|x - 1| + \\left|y\\right|"), quote(abs(x - 1) + abs(y)))
  expect_identical(tree("\\log_2 x"), quote(log(x) / log(2)))
  expect_identical(tree("\\ln x"), quote(log(x)))
  expect_identical(tree("\\binom{n}{2}"), quote(choose(n, 2)))
  expect_identical(tree("n!"), quote(factorial(n)))
  expect_identical(tree("\\Gamma\\left(\\frac{\\nu}{2}\\right)"), quote(gamma(nu / 2)))
  expect_identical(tree("\\operatorname{qt}(u, \\nu)"), quote(qt(u, nu)))
  expect_identical(tree("\\mathit{df} + 1"), quote(df + 1))
})

test_that("LaTeX scripts take one character when unbraced", {
  expect_identical(tree("x^23"), quote(x^2 * 3))
  expect_identical(tree("\\frac12 x"), quote(0.5 * x))
})

test_that("left-hand side and vectors", {
  p <- parse_latex("f(x, y) = x y")
  expect_identical(normalize_tree(p$expr), quote(x * y))
  expect_true("lhs" %in% p$assumptions$kind)
  expect_identical(tree("\\begin{pmatrix} x y \\\\ x^2 \\end{pmatrix}"), quote(c(x * y, x^2)))
  expect_identical(tree("\\begin{bmatrix} x & y \\end{bmatrix}"), quote(c(x, y)))
  expect_error(parse_latex("\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}"), "matrices")
})

test_that("numeric-partial notation parses", {
  e <- tree("\\left.\\frac{\\partial^{2}}{\\partial \\nu \\, \\partial x}\\left[\\Gamma\\left(\\nu x\\right)\\right]\\right|_{\\mathrm{num}}")
  expect_identical(e, quote(numeric_partial(gamma(nu * x), "nu", "x")))
})

test_that("superscripted \\Phi and \\Gamma are still functions (review of #2)", {
  p <- parse_latex("\\Phi^{-1}(u)")
  expect_identical(p$expr, quote(qnorm(u)))
  expect_true("inverse_fn" %in% p$assumptions$kind)
  expect_error(parse_latex("\\Phi^{-1}(u)", strict = TRUE), class = "latex2grad_ambiguity")
  expect_identical(tree("\\Gamma^2(x)"), quote(gamma(x)^2))
  expect_identical(tree("\\Phi^{-1}\\left(u\\right)"), quote(qnorm(u)))
})

test_that("an unbraced script e is Euler's number, as in the braced form", {
  p <- parse_latex("x^e")
  expect_identical(p$expr, quote(x^exp(1)))
  expect_true("euler" %in% p$assumptions$kind)
  expect_identical(tree("x^e", vars = "e"), quote(x^e))
  expect_identical(tree("\\log_e x"), tree("\\log_{e} x"))
})

test_that("Leibniz notation with d or \\mathrm{d} is rejected", {
  expect_error(parse_latex("\\frac{\\mathrm{d}}{\\mathrm{d}x}\\left(x^2\\right)"), "Leibniz")
  expect_error(parse_latex("\\frac{d}{dx} x^2"), "Leibniz")
  expect_error(parse_latex("\\frac{d^2}{dx^2} x^3"), "Leibniz")
  expect_identical(tree("\\frac{d}{2}"), quote(d / 2))
})

test_that("\\left\\{ and \\left| delimit function arguments", {
  expect_identical(tree("\\exp\\left\\{-x^2\\right\\}"), call("exp", call("-", quote(x^2))))
  expect_identical(tree("\\log\\left|x\\right|"), quote(log(abs(x))))
})

test_that("t_i^{-1} without an argument is a reciprocal", {
  expect_identical(tree("t_1^{-1}"), call("^", quote(t_1), call("-", 1)))
  expect_identical(tree("\\frac{1}{2} t_0^{-1}"), call("*", 0.5, call("^", quote(t_0), call("-", 1))))
})
