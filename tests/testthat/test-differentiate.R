test_that("gradient of a polynomial is computed through DefDiff", {
  r <- latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"))
  f <- as_function(r)
  for (pt in list(c(x = 0.3, y = 1.2), c(x = 1.1, y = 0.7))) {
    v <- f(as.list(pt))
    expect_equal(unname(v["x"]), 2 * pt[["x"]] * pt[["y"]] + cos(pt[["x"]]), tolerance = 1e-12)
    expect_equal(unname(v["y"]), pt[["x"]]^2, tolerance = 1e-12)
  }
  expect_type(r$derivative$latex, "character")
  expect_length(r$derivative$latex, 2L)
  expect_true(r$certificate$pass)
})

test_that("free symbols not in wrt are parameters", {
  r <- latex2grad("a x^2", wrt = "x")
  expect_identical(r$parameters, "a")
  expect_identical(r$derivative$entries$x, quote(a * 2 * x))
})

test_that("Hessian of a two-variable formula", {
  r <- latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"), type = "hessian")
  H <- as_function(r)(x = 0.4, y = 1.5)
  expect_equal(dim(H), c(2L, 2L))
  expect_equal(H["x", "x"], 2 * 1.5 - sin(0.4), tolerance = 1e-12)
  expect_equal(H["x", "y"], 2 * 0.4, tolerance = 1e-12)
  expect_equal(H["y", "x"], 2 * 0.4, tolerance = 1e-12)
  expect_equal(H["y", "y"], 0)
  expect_true(r$certificate$pass)
})

test_that("Jacobian of a vector-valued formula", {
  r <- latex2grad("\\begin{pmatrix} x y \\\\ x^2 \\end{pmatrix}", wrt = c("x", "y"), type = "jacobian")
  J <- as_function(r)(x = 2, y = 3)
  expect_equal(unname(J), matrix(c(3, 4, 2, 0), 2, 2))
  expect_true(r$certificate$pass)
  row <- latex2grad("x y", wrt = c("x", "y"), type = "jacobian")
  expect_equal(dim(row$derivative$entries), c(1L, 2L))
  expect_match(latex2grad("\\begin{pmatrix} x \\\\ y \\end{pmatrix}", wrt = "x")$error, "jacobian")
})

test_that("tangent is lowered into the catalog", {
  r <- latex2grad("\\tan x", wrt = "x")
  expect_identical(r$parsed$lowered, quote(sin(x) / cos(x)))
  expect_true(any(grepl("tan", r$parsed$lowering)))
  expect_equal(nrow(r$out_of_scope), 0L)
  expect_equal(unname(as_function(r)(x = 0.5)), 1 / cos(0.5)^2, tolerance = 1e-12)
})

test_that("other definitional lowerings are exact", {
  for (tex in c("\\sinh x", "\\cosh x", "\\arcsin x", "\\arccos x", "\\cot x", "\\sec x", "\\csc x")) {
    r <- latex2grad(tex, wrt = "x", domain = list(x = c(0.2, 0.8)))
    expect_true(r$certificate$pass, info = tex)
    expect_equal(nrow(r$out_of_scope), 0L, info = tex)
  }
  r <- latex2grad("\\operatorname{dnorm}(x)", wrt = "x")
  expect_equal(unname(as_function(r)(x = 0.7)), -0.7 * dnorm(0.7), tolerance = 1e-12)
})

test_that("symbolic exponents in the base variable use the exp-log workaround", {
  r <- latex2grad("x^{y}", wrt = c("x", "y"))
  expect_equal(nrow(r$out_of_scope), 0L)
  expect_true(length(r$notes) == 1L)
  expect_equal(unname(as_function(r)(x = 1.3, y = 0.4)), c(0.4 * 1.3^-0.6, 1.3^0.4 * log(1.3)),
               tolerance = 1e-12)
  expect_true(r$certificate$pass)
})

test_that("constant formulas and missing variables give zero, with a warning", {
  expect_warning(r <- latex2grad("5 + a", wrt = "x"), "does not occur")
  expect_identical(r$derivative$entries$x, 0)
  expect_true(any(grepl("does not occur", r$notes)))
})

test_that("wrt is checked against the formula (review of #2)", {
  r <- latex2grad("\\nu^2", wrt = "\\nu")
  expect_identical(r$wrt, "nu")
  expect_identical(r$derivative$entries$nu, quote(2 * nu))
  expect_true(any(grepl("read as the symbol", r$notes)))
  expect_warning(latex2grad("x_1^2 + y", wrt = "x_1", where = c(x_1 = "\\sin(z)")), "where")
})

test_that("as_function requires a value for every symbol", {
  f <- as_function(latex2grad("u x^2", wrt = "x"))
  u <- 100
  expect_error(f(x = 2), "no value given for: u")
  expect_equal(unname(f(x = 2, u = 3)), 12)
  expect_error(as_function(latex2grad("T x^2", wrt = "x"))(x = 2), "T")
})
