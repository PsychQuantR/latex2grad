# Canonical trees carry no `(` nodes; compare against the normalized form of
# a quoted expectation.
expect_canon <- function(object, expected, ...) {
  expect_identical(object, tex_normalize(expected), ...)
}
