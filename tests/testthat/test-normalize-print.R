rt <- function(e) {
  e <- normalize_tree(e)
  identical(normalize_tree(parse_latex(to_latex(e), vars = all.vars(e))$expr), e)
}

test_that("identities and constants fold", {
  expect_identical(normalize_tree(quote(0 + 1 * x^1 + (2 - 2) * y)), quote(x))
  expect_identical(normalize_tree(quote(1/3 - 1)), normalize_tree(quote(-(2/3))))
  expect_identical(normalize_tree(quote(2^3 * x)), quote(8 * x))
  expect_identical(normalize_tree(quote(2^(1/3))), normalize_tree(quote(2^(1/3))))
  expect_true(is.call(normalize_tree(quote(2^(1/3)))))   # non-integer powers stay symbolic
  expect_identical(normalize_tree(quote(sqrt(2) * x)), quote(sqrt(2) * x))
  expect_identical(normalize_tree(quote(x - x)), 0)
})

test_that("signs and associativity are normalized", {
  expect_identical(normalize_tree(quote(a + (-b))), quote(a - b))
  expect_identical(normalize_tree(quote(a - (-b))), quote(a + b))
  expect_identical(normalize_tree(quote((-a) * b)), call("-", quote(a * b)))
  expect_identical(normalize_tree(quote(a * (b * c))), quote(a * b * c))
  expect_identical(normalize_tree(quote(a - (b - c))), quote(a - b + c))
  expect_identical(normalize_tree(quote((x / 2) / 2)), quote(x / 4))
})

test_that("DefDiff artifacts are removed for scalar variables", {
  e <- quote(rep(1, length(x^2 * y)) * y * 2 * x^1 + rep(0, length(x)))
  expect_identical(normalize_tree(e), quote(y * 2 * x))
  expect_identical(normalize_tree(quote(as.numeric(a) / sum(b))), quote(a / b))
})

test_that("kernel calls map back to arithmetic", {
  expect_identical(normalize_tree(quote(fast_vec_mul(a, b))), quote(a * b))
  expect_identical(normalize_tree(quote(fast_vec_add(a, b))), quote(a + b))
  expect_identical(normalize_tree(quote(fast_vec_sub(a, b))), quote(a - b))
  expect_identical(normalize_tree(quote(fast_scalar_mul(s, v))), quote(s * v))
  expect_identical(normalize_tree(quote(fast_scalar_div(a, b))), quote(a / b))
  expect_identical(normalize_tree(quote(fast_vv_cos(a))), quote(cos(a)))
})

test_that("normalize_tree is idempotent", {
  for (e in list(quote(rep(1, length(x)) * cos(x) - 0 * y), quote(-(-(a + -b))), quote(1/7 + x / -2))) {
    n1 <- normalize_tree(e)
    expect_identical(normalize_tree(n1), n1)
  }
})

test_that("printing is precedence-aware", {
  e <- normalize_tree(quote(-(a + b) * c^(x + 1)))
  s <- to_latex(e)
  expect_match(s, "\\left(a + b\\right)", fixed = TRUE)
  expect_match(s, "^{x + 1}", fixed = TRUE)
  expect_true(rt(e))
  expect_identical(to_latex(quote(a / b)), "\\frac{a}{b}")
  expect_identical(to_latex(quote(sin(x)^2)), "\\sin\\left(x\\right)^{2}")
})

test_that("numbers print exactly", {
  expect_identical(to_latex(1/3), "\\frac{1}{3}")
  expect_identical(to_latex(0.123), "0.123")
  expect_identical(to_latex(3), "3")
  expect_identical(to_latex(normalize_tree(-0.5)), "-\\frac{1}{2}")
  for (x in c(1/3, 0.123, pi, exp(1) / 1000, 12345.678, 1e-9 * 3.3)) {
    expect_true(rt(x), info = format(x, digits = 17))
  }
})

test_that("round-trip holds on a battery of trees", {
  battery <- c("x^2 y + \\sin(x)", "\\frac{a}{b} c", "(a+b)(c+d)", "-x^2", "x^{-\\frac{2}{3}}",
               "\\Gamma\\left(\\frac{\\nu+1}{2}\\right)^2", "t_\\nu^{-1}(u_1)", "\\Phi(x) + |y|",
               "\\begin{pmatrix} x y \\\\ x^2 \\end{pmatrix}", "\\sqrt{1 - \\rho^2} \\nu", "n! + \\binom{n}{k}",
               "e^{-x^2/2}", "\\log_2 x", "2 \\cdot 3 x", "x_{1} x_{2} - 2 \\rho x_1 x_2")
  for (tex in battery) {
    e <- normalize_tree(parse_latex(tex)$expr)
    expect_true(rt(e), info = tex)
  }
  expect_true(rt(quote(numeric_partial(qt(u_1, nu), "nu", "nu"))))
})

test_that("multi-argument rep/sum calls keep their meaning (review of #2)", {
  expect_identical(normalize_tree(quote(sum(a, b))), quote(sum(a, b)))
  expect_identical(normalize_tree(quote(rep(x, 3))), quote(rep(x, 3)))
  expect_identical(normalize_tree(quote(rep(2, length(x)))), 2)
})

test_that("to_latex prints parenthesis calls from quote() correctly", {
  s <- to_latex(quote(-(a + b) * c^(x + 1)))
  expect_false(grepl("operatorname", s, fixed = TRUE))
  expect_true(rt(quote(-(a + b) * c^(x + 1))))
  expect_identical(to_latex(quote(qnorm(u))), "\\Phi^{-1}\\left(u\\right)")
  expect_true(rt(quote(qnorm(u) * x)))
})
