test_that("normalization folds identities and is idempotent", {
  n <- tex_normalize
  expect_identical(n(quote(x + 0)), quote(x))
  expect_identical(n(quote(1 * x * 1)), quote(x))
  expect_identical(n(quote(0 * sin(x))), 0)
  expect_identical(n(quote(x^1)), quote(x))
  expect_identical(n(quote(-(-x))), quote(x))
  expect_identical(n(quote(a + -b)), quote(a - b))
  expect_identical(n(quote(sin(y) * 2 * x)), quote(2 * sin(y) * x))
  expect_identical(n(quote(2 * (a / 2) * x)), quote(a * x))
  expect_identical(n(quote(y * e * y)), quote(y^2 * e))
  expect_identical(n(quote(x^3 / x)), quote(x^2))
  expect_identical(n(quote((x^2)^3)), quote(x^6))
  expect_identical(n(quote(a * (b * c))), quote(a * b * c))
  expect_identical(n(quote(a + (b + c))), quote(a + b + c))
  expect_identical(n(quote(exp(1)^x)), quote(exp(x)))
  expect_identical(n(-2), quote(-2))
  e <- quote(-(2 * (exp(-(x^2 / 2)) / sqrt(2 * pi) / 2) * x))
  expect_identical(n(n(e)), n(e))
})

test_that("DefDiff fast-path kernels and shape helpers are removed", {
  n <- tex_normalize
  expect_identical(n(quote(fast_vec_add(fast_vv_sin(x), fast_vec_mul(x, fast_vv_cos(x))))),
                   quote(sin(x) + x * cos(x)))
  expect_identical(n(quote({ kv <- fast_scalar_mul(3, x); fast_scalar_mul(3, fast_vv_cos(kv)) })),
                   quote(3 * cos(3 * x)))
  expect_identical(n(quote(rep(1, length(x^2)) * cos(y) + rep(0, length(x)))), quote(cos(y)))
  expect_identical(n(quote(DefDiff::integral(t, t, 0, x))), quote(integral(t, t, 0, x)))
})

test_that("binder slots are left alone", {
  e <- quote(integral(-(t^2), t, 0, x))
  expect_identical(tex_normalize(e), quote(-integral(t^2, t, 0, x)))
})

test_that("printing", {
  expect_identical(tex_print(quote(x^2 / 2)), "\\frac{x^{2}}{2}")
  expect_identical(tex_print(quote(2 * x * y)), "2 x y")
  expect_identical(tex_print(quote(x * 2)), "x \\cdot 2")
  expect_identical(tex_print(quote(exp(-x))), "e^{-x}")
  expect_identical(tex_print(quote(sin(x)^2)), "\\sin^{2}\\left(x\\right)")
  expect_identical(tex_print(quote(qt(u_1, nu))), "t_{\\nu}^{-1}\\left(u_{1}\\right)")
  expect_identical(tex_print(quote(lgamma(x))), "\\ln\\Gamma\\left(x\\right)")
  expect_identical(tex_print(quote(-(a + b))), "-\\left(a + b\\right)")
  expect_identical(tex_print(quote(a - (b - c))), "a - \\left(b - c\\right)")
  expect_identical(tex_print(quote(c(x, y))), "\\begin{pmatrix} x \\\\ y \\end{pmatrix}")
  expect_identical(tex_print(0.1), "0.1")
  expect_identical(tex_print(Inf), "\\infty")
})

test_that("parse(print(e)) == e on a corpus of canonical expressions", {
  corpus <- c(
    "\\frac{1}{\\sqrt{2\\pi}} e^{-x^2/2}",
    "\\sin^2 x + \\cos^{2}(x)",
    "\\ln\\Gamma(x) + \\Gamma\\left(\\frac{\\nu+1}{2}\\right) - \\psi_1(x)",
    "t_{\\nu}^{-1}(u_1) \\cdot \\Phi^{-1}(p)",
    "\\int_0^x e^{-t^2} \\, dt",
    "\\left| x \\right| + |y - 1|",
    "\\begin{pmatrix} x^2 y \\\\ \\sin x \\end{pmatrix}",
    "\\left.\\frac{\\partial}{\\partial s} \\operatorname{qt}(u, s)\\right|_{s=\\nu}",
    "-\\frac{1}{2}\\log(2\\pi\\sigma^2) - \\frac{(x-\\mu)^2}{2\\sigma^2}",
    "x^{-1} + 2^{x} + 0.1 x - 3 \\cdot 4",
    "(1 + x^2/\\nu)^{-(\\nu+1)/2}",
    "\\operatorname{erf}(x)^2 + \\mathrm{B}(a, b) + \\mathrm{rate} \\cdot \\sigma_{xy} + \\operatorname{arsinh}(x)",
    "a - (b - c) + (d + e_1)",
    "\\binom{n}{k} + \\max(a, b)",
    "\\log_{10} x + \\lg x",
    "\\int_{-\\infty}^{x} \\phi(t) \\mathrm{d}t",
    "x \\cdot 10^{-3} + 1.5e-4",
    "\\frac{a}{b} \\frac{c}{d} + \\frac{\\frac{a}{b}}{c}",
    "\\sqrt{x}^{3} + \\left(x^{2}\\right)^{y}"
  )
  for (s in corpus) {
    e <- tex_normalize(tex_parse(s)$expr)
    rt <- l2g_round_trip(e)
    expect_true(rt$ok, info = paste(s, "->", rt$tex))
  }
})
