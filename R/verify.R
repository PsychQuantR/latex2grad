## verify.R
## The certificate: (1) round-trip of every printed expression through the
## parser and normalizer; (2) comparison of every symbolic derivative with a
## Richardson-extrapolated central difference at random points.

#' Round-trip check of an expression
#'
#' Prints `expr` with [tex_print()], parses the LaTeX back with [tex_parse()]
#' and normalizes it. The check passes when the result is identical to `expr`
#' (which should already be canonical).
#'
#' @param expr A canonical expression.
#' @return A list with `ok`, `tex`, and `reparsed` (or `error`).
#' @export
#' @examples
#' l2g_round_trip(tex_normalize(quote(exp(-x^2 / 2))))$ok
l2g_round_trip <- function(expr) {
  tex <- tryCatch(tex_print(expr), latex2grad_error = function(e) e)
  if (inherits(tex, "error")) return(list(ok = FALSE, tex = NA_character_, error = conditionMessage(tex)))
  back <- tryCatch(tex_normalize(tex_parse(tex)$expr), latex2grad_error = function(e) e)
  if (inherits(back, "error")) return(list(ok = FALSE, tex = tex, error = conditionMessage(back)))
  list(ok = identical(back, expr), tex = tex, reparsed = back)
}

# Numeric verification.
#   f_list : named list of scalar expressions (components; one for grad/hessian)
#   d      : list of derivative entries, each list(label, expr, ref_f, var)
#            where the reference is d ref_f / d var computed numerically.
.l2g_verify_numeric <- function(vars, d, domain, n_points, tol, seed) {
  env <- .l2g_eval_env()
  has_numeric_nodes <- any(vapply(d, function(x) {
    any(c("numd", "integral", "implicit") %in% all.names(x$expr)) ||
      any(c("numd", "integral", "implicit") %in% all.names(x$ref_f))
  }, logical(1)))
  tol_eff <- if (has_numeric_nodes) max(tol, 1e-4) else tol

  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  })
  set.seed(seed)

  ranges <- lapply(vars, function(v) {
    r <- domain[[v]]
    if (is.null(r)) c(-2, 2) else r
  })
  names(ranges) <- vars

  max_attempts <- max(200L, 100L * n_points)
  attempts <- 0L
  points <- list()
  worst <- list(err = 0, label = NA_character_, point = NULL)
  errs <- numeric(0)
  while (length(points) < n_points && attempts < max_attempts) {
    attempts <- attempts + 1L
    p <- lapply(ranges, function(r) stats::runif(1L, r[[1L]], r[[2L]]))
    row <- numeric(length(d))
    ok <- TRUE
    for (k in seq_along(d)) {
      sym <- .l2g_eval(d[[k]]$expr, p, env)
      if (!is.finite(sym)) { ok <- FALSE; break }
      v <- d[[k]]$var
      g <- function(t) { q <- p; q[[v]] <- t; .l2g_eval(d[[k]]$ref_f, q, env) }
      if (!is.finite(g(p[[v]]))) { ok <- FALSE; break }
      ref <- tryCatch(.l2g_richardson(g, p[[v]]), error = function(e) NA_real_)
      if (!is.finite(ref)) { ok <- FALSE; break }
      row[[k]] <- abs(sym - ref) / max(1, abs(ref))
    }
    if (!ok) next
    points[[length(points) + 1L]] <- p
    errs <- c(errs, max(row))
    if (max(row) >= worst$err) {
      worst <- list(err = max(row), label = d[[which.max(row)]]$label, point = p)
    }
  }
  n_ok <- length(points)
  list(
    n_points = n_ok, n_requested = n_points, n_attempts = attempts,
    max_rel_error = if (n_ok) max(errs) else NA_real_,
    worst = worst, tol = tol, tol_effective = tol_eff,
    widened = has_numeric_nodes, ranges = ranges,
    pass = n_ok > 0L && max(errs) <= tol_eff
  )
}
