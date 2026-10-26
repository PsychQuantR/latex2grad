## normalize.R
## Canonical form of an expression tree. The same normalizer is applied to the
## parsed formula and to every derivative, so `parse -> normalize` is the
## inverse of `print` on canonical trees (the round-trip certificate).
##
## Canonical form:
##   * no `(` nodes, no `{` blocks, no DefDiff fast-path kernels;
##   * numeric literals are non-negative doubles (a sign is a unary `-`);
##   * `+` and `*` chains are left-associated; a product carries its numeric
##     coefficient first and its sign outside: -(2 * x * y);
##   * identities x + 0, x * 1, x * 0, x^1, x^0, -(-x), e^a -> exp(a) folded;
##   * numeric sub-expressions folded when the result is exact.

# Binder heads: the third element is a bound symbol, never rewritten.
.l2g_binders <- c("integral", "implicit", "numd")

#' Canonical form of an expression
#'
#' Folds arithmetic identities, removes DefDiff fast-path kernel calls and
#' parentheses, and left-associates sums and products. Normalization is
#' idempotent and semantics-preserving.
#'
#' @param expr An R call, symbol, or number.
#' @return The canonical expression.
#' @export
#' @examples
#' tex_normalize(quote(rep(1, length(x)) * sin(y) * 2 * x^1 + rep(0, length(x))))
tex_normalize <- function(expr) {
  for (k in seq_len(50L)) {
    new <- .l2g_norm(expr)
    if (identical(new, expr)) return(new)
    expr <- new
  }
  expr
}

.l2g_is_num <- function(x, value = NULL) {
  is.numeric(x) && length(x) == 1L && !is.na(x) &&
    (is.null(value) || x == value)
}
.l2g_is_op <- function(x, op, n = NULL) {
  is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), op) &&
    (is.null(n) || length(x) == n + 1L)
}
.l2g_neg <- function(x) .l2g_is_op(x, "-", 1L)

# Fold a numeric operation when the result is exactly representable in the
# printed form (integers, or exact +,-,* of decimals).
.l2g_fold <- function(op, a, b) {
  v <- switch(op, "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b, "^" = a^b)
  if (!is.finite(v)) return(NULL)
  if (op %in% c("/", "^") && (v != round(v) || abs(v) > 1e15)) return(NULL)
  v
}

.l2g_num_lit <- function(v) if (v < 0) call("-", -v) else v

.l2g_norm <- function(e) {
  if (is.numeric(e)) {
    if (length(e) != 1L) return(e)
    e <- as.double(e)
    if (!is.na(e) && e < 0) return(call("-", -e))
    return(e)
  }
  if (is.logical(e) && length(e) == 1L) return(as.double(e))
  if (!is.call(e)) return(e)

  head <- e[[1L]]
  # DefDiff::integral -> integral
  if (is.call(head) && identical(head[[1L]], as.name("::")) && length(head) == 3L) {
    e[[1L]] <- head[[3L]]
    head <- e[[1L]]
  }
  if (!is.symbol(head)) return(e)
  op <- as.character(head)

  if (op == "(") return(.l2g_norm(e[[2L]]))
  if (op == "{") return(.l2g_inline_block(e))

  # Children first (binder slot 3 stays a bare symbol).
  if (length(e) > 1L) {
    for (j in seq_along(e)[-1L]) {
      if (op %in% .l2g_binders && j == 3L) next
      if (is.null(e[[j]])) next
      e[[j]] <- .l2g_norm(e[[j]])
    }
  }

  k <- .l2g_dekernel(op, e)
  if (!is.null(k)) return(k)

  switch(op,
    "+" = .l2g_norm_plus(e),
    "-" = .l2g_norm_minus(e),
    "*" = .l2g_norm_product(e),
    "/" = .l2g_norm_div(e),
    "^" = .l2g_norm_pow(e),
    integral = .l2g_norm_integral(e),
    e)
}

# DefDiff fast-path kernels and shape helpers, read in a scalar context.
.l2g_dekernel <- function(op, e) {
  a <- if (length(e) > 1L) e[[2L]] else NULL
  b <- if (length(e) > 2L) e[[3L]] else NULL
  switch(op,
    fast_scalar_mul = , metal_scalar_mul = , fast_vec_mul = call("*", a, b),
    fast_scalar_div = , fast_vec_div = call("/", a, b),
    fast_vec_add = call("+", a, b),
    fast_vec_sub = call("-", a, b),
    fast_vec_smadd = call("+", call("*", a, b), e[[4L]]),
    fast_vv_sin = call("sin", a), fast_vv_cos = call("cos", a),
    fast_vv_exp = call("exp", a), fast_vv_log = call("log", a),
    fast_vv_tanh = call("tanh", a), fast_vv_sqrt = call("sqrt", a),
    fast_sum_sq = call("^", a, 2), fast_sum_pow = call("^", a, b),
    as.numeric = a,
    sum = if (length(e) == 2L) a else NULL,
    rep = if (length(e) == 3L && .l2g_is_op(b, "length")) a else NULL,
    NULL)
}

# { v <- expr; ...; final }  ->  final with every v substituted.
.l2g_inline_block <- function(e) {
  stmts <- as.list(e)[-1L]
  binds <- list()
  for (s in stmts[-length(stmts)]) {
    if (!(.l2g_is_op(s, "<-", 2L) || .l2g_is_op(s, "=", 2L)) || !is.symbol(s[[2L]])) {
      .l2g_stop("normalize", paste0("cannot inline block statement `", deparse1(s), "`"))
    }
    binds[[as.character(s[[2L]])]] <- .l2g_subst(s[[3L]], binds)
  }
  .l2g_norm(.l2g_subst(stmts[[length(stmts)]], binds))
}

# Substitute free occurrences of the symbols named in `binds` (binder-aware).
.l2g_subst <- function(expr, binds) {
  if (!length(binds)) return(expr)
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nzchar(nm) && !is.null(binds[[nm]])) return(binds[[nm]])
    return(expr)
  }
  if (!is.call(expr)) return(expr)
  op <- if (is.symbol(expr[[1L]])) as.character(expr[[1L]]) else ""
  if (op %in% .l2g_binders && length(expr) >= 3L) {
    bound <- as.character(expr[[3L]])
    inner <- binds[setdiff(names(binds), bound)]
    expr[[2L]] <- .l2g_subst(expr[[2L]], inner)
    for (j in seq_along(expr)[-(1:3)]) expr[[j]] <- .l2g_subst(expr[[j]], binds)
    return(expr)
  }
  for (j in seq_along(expr)[-1L]) {
    if (is.null(expr[[j]])) next
    expr[[j]] <- .l2g_subst(expr[[j]], binds)
  }
  expr
}

.l2g_norm_plus <- function(e) {
  if (length(e) == 2L) return(e[[2L]])
  a <- e[[2L]]; b <- e[[3L]]
  if (.l2g_is_num(a, 0)) return(b)
  if (.l2g_is_num(b, 0)) return(a)
  if (.l2g_is_num(a) && .l2g_is_num(b)) {
    v <- .l2g_fold("+", a, b); if (!is.null(v)) return(.l2g_num_lit(v))
  }
  if (.l2g_neg(b)) return(call("-", a, b[[2L]]))
  if (.l2g_is_op(b, "+", 2L)) return(call("+", call("+", a, b[[2L]]), b[[3L]]))
  if (.l2g_is_op(b, "-", 2L)) return(call("-", call("+", a, b[[2L]]), b[[3L]]))
  e
}

.l2g_norm_minus <- function(e) {
  if (length(e) == 2L) {
    a <- e[[2L]]
    if (.l2g_is_num(a, 0)) return(0)
    if (.l2g_neg(a)) return(a[[2L]])
    return(e)
  }
  a <- e[[2L]]; b <- e[[3L]]
  if (.l2g_is_num(b, 0)) return(a)
  if (.l2g_is_num(a, 0)) return(call("-", b))
  if (identical(a, b)) return(0)
  if (.l2g_is_num(a) && .l2g_is_num(b)) {
    v <- .l2g_fold("-", a, b); if (!is.null(v)) return(.l2g_num_lit(v))
  }
  if (.l2g_neg(b)) return(call("+", a, b[[2L]]))
  if (.l2g_is_op(b, "+", 2L)) return(call("-", call("-", a, b[[2L]]), b[[3L]]))
  if (.l2g_is_op(b, "-", 2L)) return(call("+", call("-", a, b[[2L]]), b[[3L]]))
  e
}

# Flatten a product / quotient into a sign, a rational numeric coefficient
# cn / cd, numerator factors and denominator factors, then rebuild it as
# -(cn * n_1 * ... * n_k) / (cd * d_1 * ... * d_m), left-associated.
.l2g_norm_product <- function(e) {
  sign <- 1
  cn <- 1; cd <- 1
  num <- list(); den <- list()
  walk <- function(x, side) {
    if (.l2g_is_op(x, "*", 2L)) { walk(x[[2L]], side); walk(x[[3L]], side); return(invisible()) }
    if (.l2g_is_op(x, "/", 2L)) { walk(x[[2L]], side); walk(x[[3L]], -side); return(invisible()) }
    if (.l2g_neg(x)) { sign <<- -sign; walk(x[[2L]], side); return(invisible()) }
    if (.l2g_is_num(x) && is.finite(x) && (side > 0 || x != 0)) {
      if (side > 0) cn <<- cn * x else cd <<- cd * x
      return(invisible())
    }
    if (side > 0) num[[length(num) + 1L]] <<- x else den[[length(den) + 1L]] <<- x
  }
  walk(e, 1)
  if (cn == 0) return(0)
  # Combine like factors: a^m * a^n / a^p -> a^(m + n - p) for numeric
  # exponents (the result sits where the base first occurred).
  powers <- list()
  add_factor <- function(f, s) {
    base <- f; k <- 1
    if (.l2g_is_op(f, "^", 2L)) {
      x <- f[[3L]]
      if (.l2g_is_num(x)) { base <- f[[2L]]; k <- x }
      else if (.l2g_neg(x) && .l2g_is_num(x[[2L]])) { base <- f[[2L]]; k <- -x[[2L]] }
    }
    for (j in seq_along(powers)) {
      if (identical(powers[[j]]$base, base)) {
        powers[[j]]$k <<- powers[[j]]$k + s * k
        return(invisible())
      }
    }
    powers[[length(powers) + 1L]] <<- list(base = base, k = s * k)
  }
  for (f in num) add_factor(f, 1)
  for (f in den) add_factor(f, -1)
  pw <- function(b, k) if (k == 1) b else call("^", b, k)
  num <- list(); den <- list()
  for (p in powers) {
    if (p$k > 0) num[[length(num) + 1L]] <- pw(p$base, p$k)
    if (p$k < 0) den[[length(den) + 1L]] <- pw(p$base, -p$k)
  }
  if (cd != 1) {
    if (cn == round(cn) && cd == round(cd) && max(cn, cd) < 1e15) {
      g <- .l2g_gcd(cn, cd)
      cn <- cn / g; cd <- cd / g
    } else {
      q <- cn / cd
      if (as.numeric(format(q, digits = 15)) == q && nchar(format(q, digits = 15)) <= 10L) {
        cn <- q; cd <- 1
      }
    }
  }
  prod_of <- function(coef, fs) {
    items <- c(if (coef != 1 || !length(fs)) list(coef), fs)
    Reduce(function(a, b) call("*", a, b), items)
  }
  out <- prod_of(cn, num)
  if (length(den) || cd != 1) out <- call("/", out, prod_of(cd, den))
  if (sign < 0) out <- call("-", out)
  out
}

.l2g_gcd <- function(a, b) {
  while (b != 0) { t <- b; b <- a %% b; a <- t }
  a
}

.l2g_norm_div <- function(e) {
  if (.l2g_is_num(e[[2L]]) && .l2g_is_num(e[[3L]])) {
    v <- .l2g_fold("/", e[[2L]], e[[3L]]); if (!is.null(v)) return(.l2g_num_lit(v))
  }
  .l2g_norm_product(e)
}

.l2g_norm_pow <- function(e) {
  a <- e[[2L]]; b <- e[[3L]]
  if (.l2g_is_num(b, 1)) return(a)
  if (.l2g_is_num(b, 0)) return(1)
  if (.l2g_is_num(a, 1)) return(1)
  if (.l2g_is_num(a) && .l2g_is_num(b)) {
    v <- .l2g_fold("^", a, b); if (!is.null(v)) return(.l2g_num_lit(v))
  }
  if (identical(a, quote(exp(1)))) return(call("exp", b))
  # (a^m)^n -> a^(m n) for numeric m and integer n (always valid).
  if (.l2g_is_op(a, "^", 2L) && .l2g_is_num(b) && b == round(b)) {
    m <- a[[3L]]
    m <- if (.l2g_is_num(m)) m else if (.l2g_neg(m) && .l2g_is_num(m[[2L]])) -m[[2L]] else NULL
    if (!is.null(m)) return(call("^", a[[2L]], .l2g_num_lit(m * b)))
  }
  e
}

# Linearity: integral(-f) -> -integral(f).
.l2g_norm_integral <- function(e) {
  if (length(e) == 5L && .l2g_neg(e[[2L]])) {
    e[[2L]] <- e[[2L]][[2L]]
    return(call("-", e))
  }
  e
}
