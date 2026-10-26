## numeric.R
## Numeric pieces: the `numd` derivative node, special functions R lacks, the
## evaluation environment, and Richardson-extrapolated central differences.

#' Numeric derivative node
#'
#' `numd(body, s, at)` is the derivative of `body` with respect to the bound
#' symbol `s`, evaluated at `s = at`, computed by Richardson extrapolation of
#' central differences. latex2grad emits it for terms outside DefDiff's
#' language that have no closed-form partial (e.g. the degrees-of-freedom
#' derivative of a Student-t quantile). It prints as an evaluation bar
#' \eqn{\left.\frac{\partial}{\partial s}(\cdot)\right|_{s=a}}.
#'
#' @param body An expression in `s` (unevaluated).
#' @param s The bound symbol (unevaluated).
#' @param at The point at which to evaluate the derivative.
#' @return A number.
#' @export
#' @examples
#' numd(sin(s), s, 0)              # 1
#' numd(qt(0.9, s), s, 5)
numd <- function(body, s, at) {
  b <- substitute(body)
  sym <- substitute(s)
  if (!is.symbol(sym)) stop("numd(): `s` must be a bare symbol")
  sname <- as.character(sym)
  env <- parent.frame()
  g <- function(t) {
    e <- new.env(parent = env)
    assign(sname, t, envir = e)
    eval(b, e)
  }
  .l2g_richardson(g, at)
}

#' Error function and its complement
#'
#' \eqn{\mathrm{erf}(x) = 2\Phi(x\sqrt{2}) - 1}, \eqn{\mathrm{erfc}(x) = 1 - \mathrm{erf}(x)}.
#' @param x Numeric vector.
#' @return Numeric vector.
#' @export
#' @examples
#' erf(0.5)
erf <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1

#' @rdname erf
#' @export
erfc <- function(x) 2 * stats::pnorm(-x * sqrt(2))

# Richardson extrapolation of central differences (the numDeriv scheme:
# step d*|x| (+ d near 0), halved r-1 times, eliminated up to order 2r).
.l2g_richardson <- function(g, x, d = 1e-4, r = 4L, v = 2) {
  h <- abs(d * x) + d * (abs(x) < 1e-4)
  a <- numeric(r)
  for (k in seq_len(r)) {
    a[k] <- (g(x + h) - g(x - h)) / (2 * h)
    h <- h / v
  }
  for (m in seq_len(r - 1L)) {
    f <- v^(2 * m)
    a[seq_len(r - m)] <- (a[seq_len(r - m) + 1L] * f - a[seq_len(r - m)]) / (f - 1)
  }
  a[[1L]]
}

# Environment in which formulas and derivatives are evaluated: stats and base
# functions, DefDiff's L_4 evaluators, and latex2grad's numd / erf.
.l2g_eval_env <- function() {
  env <- new.env(parent = asNamespace("stats"))
  env$integral <- DefDiff::integral
  env$implicit <- DefDiff::implicit
  env$numd <- numd
  env$erf <- erf
  env$erfc <- erfc
  env
}

# Evaluate `expr` at a named list of values; NA on error or non-scalar.
.l2g_eval <- function(expr, values, env) {
  data <- list2env(values, parent = env)
  out <- tryCatch(suppressWarnings(eval(expr, data)), error = function(e) NA_real_)
  if (!is.numeric(out) || length(out) != 1L) return(NA_real_)
  as.numeric(out)
}
