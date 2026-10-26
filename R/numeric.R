## numeric.R
## Richardson-extrapolated central differences, and the numeric_partial()
## node that completes the chain rule across the out-of-scope boundary (D6).

#' Numeric partial derivative node
#'
#' `numeric_partial(g, "v")` is the explicit marker latex2grad puts into a
#' derivative tree where a sub-term `g` lies outside DefDiff's language: it
#' evaluates \eqn{\partial g / \partial v} by Richardson-extrapolated central
#' differences at the values bound in the calling environment.
#' `numeric_partial(g, "v", "w")` is \eqn{\partial^2 g / \partial v \partial w}.
#'
#' @param expr The sub-term, unevaluated.
#' @param ... Variable names (character), innermost derivative first.
#' @return A number.
#' @export
#' @examples
#' nu <- 3
#' numeric_partial(gamma(nu), "nu")      # gamma(3) * digamma(3)
numeric_partial <- function(expr, ...) {
  vars <- as.character(c(...))
  .np_eval(substitute(expr), vars, parent.frame())
}

.np_eval <- function(e, vars, env) {
  if (!length(vars)) return(eval(e, env))
  v <- vars[length(vars)]
  rest <- vars[-length(vars)]
  x0 <- get(v, envir = env)
  g <- function(x) {
    local_env <- new.env(parent = env)
    assign(v, x, envir = local_env)
    .np_eval(e, rest, local_env)
  }
  .richardson(g, x0)
}

# Richardson extrapolation of central differences D(h) = (g(x+h) - g(x-h))/2h
# over h0, h0/2, h0/4, h0/8 (error O(h0^8) for smooth g).
.richardson <- function(g, x0, levels = 4L) {
  h0 <- 1e-3 * max(abs(x0), 1)
  A <- matrix(NA_real_, levels, levels)
  for (i in seq_len(levels)) {
    h <- h0 / 2^(i - 1L)
    A[i, 1L] <- (g(x0 + h) - g(x0 - h)) / (2 * h)
    if (i > 1L) {
      for (j in 2:i) {
        A[i, j] <- A[i, j - 1L] + (A[i, j - 1L] - A[i - 1L, j - 1L]) / (4^(j - 1L) - 1)
      }
    }
  }
  A[levels, levels]
}

# Trigonometric functions the grammar produces but base R lacks; trees are
# evaluated with this namespace as parent, so these resolve there.
cot <- function(x) cos(x) / sin(x)
sec <- function(x) 1 / cos(x)
csc <- function(x) 1 / sin(x)

# Evaluate a tree at named values. Every free symbol must be given a value
# (nothing falls through to globals or to T/F); function heads resolve through
# this package's namespace (numeric_partial, stats, base, then the search path).
.eval_tree <- function(expr, values) {
  missing <- setdiff(.free_symbols(expr), names(values))
  if (length(missing)) {
    stop("no value given for: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  env <- list2env(as.list(values), parent = environment(numeric_partial))
  eval(expr, env)
}

# Numeric partial of a tree along `v` at `values`.
.numeric_d <- function(expr, v, values) {
  g <- function(x) {
    values[[v]] <- x
    .eval_tree(expr, values)
  }
  .richardson(g, values[[v]])
}
