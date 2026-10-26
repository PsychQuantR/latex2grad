test_that("round-trip is recorded for the source and each entry", {
  r <- latex2grad("x^2 y", wrt = c("x", "y"))
  rt <- r$certificate$roundtrip
  expect_identical(rt$item[1], "source")
  expect_equal(nrow(rt), 3L)
  expect_true(all(rt$ok))
})

test_that("a correct derivative passes, reproducibly", {
  r1 <- latex2grad("\\exp(x y)", wrt = c("x", "y"), n_points = 20, seed = 1)
  r2 <- latex2grad("\\exp(x y)", wrt = c("x", "y"), n_points = 20, seed = 1)
  expect_true(r1$certificate$pass)
  expect_lte(r1$certificate$numeric$max_rel_error, 1e-6)
  expect_identical(r1$certificate$numeric$max_rel_error, r2$certificate$numeric$max_rel_error)
  expect_equal(r1$certificate$numeric$n_valid, 20L)
})

test_that("the caller's RNG stream is not disturbed", {
  set.seed(42)
  a <- runif(1)
  set.seed(42)
  latex2grad("x^2", wrt = "x")
  b <- runif(1)
  expect_identical(a, b)
})

test_that("a corrupted derivative fails and the report says so", {
  r <- latex2grad("x^3", wrt = "x")
  r$derivative$entries$x <- quote(3 * x)
  r$derivative$latex <- to_latex(quote(3 * x))
  r$certificate <- latex2grad:::.certify(r, 25L, 1e-6, 1L, NULL, NULL)
  expect_false(r$certificate$pass)
  expect_false(r$certificate$numeric$pass)
  rep <- l2g_report(r)
  expect_match(rep, "NOT CERTIFIED", fixed = TRUE)
  expect_match(rep, "exceeds the tolerance", fixed = TRUE)
})

test_that("points outside the formula's domain are rejected", {
  r <- latex2grad("\\log(x)", wrt = "x", domain = list(x = c(-1, 1)), n_points = 10)
  expect_gt(r$certificate$numeric$n_rejected, 0L)
  expect_equal(r$certificate$numeric$n_valid, 10L)
  expect_true(r$certificate$pass)
  bad <- latex2grad("\\log(x)", wrt = "x", domain = list(x = c(-2, -1)), n_points = 10)
  expect_false(bad$certificate$pass)
  expect_match(bad$certificate$failures, "valid")
})

test_that("closure level is reported", {
  r <- latex2grad("\\sin(x)^2", wrt = "x")
  lv <- r$certificate$levels
  expect_identical(lv$source, "L_3")
  expect_true(lv$closure)
  expect_equal(lv$n_numeric, 0L)
  r2 <- latex2grad("\\Gamma(x)", wrt = "x", domain = list(x = c(1, 2)))
  expect_true(is.na(r2$certificate$levels$closure))
  expect_equal(r2$certificate$levels$n_numeric, 1L)
})

test_that("parse failures still produce an object with a failed certificate", {
  r <- latex2grad("\\frac{1}{x", wrt = "x")
  expect_false(r$certificate$pass)
  expect_match(r$error, "could not be parsed")
  expect_output(print(r), "NOT CERTIFIED")
})
