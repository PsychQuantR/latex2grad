## normalize.R
## Canonical form (design D5): a fixpoint of local, meaning-preserving
## rewrites. Operands are never reordered, so the canonical form stays close
## to the source. Negative constants are represented as unary minus applied to
## a positive literal, so folding and printing agree on one shape.

#' Normalize an expression tree to canonical form
#'
#' Applies, bottom-up and to a fixpoint: integer to double, exact constant
#' folding of `+ - * / ^`, identity elimination (`0 + a`, `a * 1`, `a^1`, ...),
#' sign normalization (`a + (-b)` to `a - b`, `(-a) * b` to `-(a * b)`, ...),
#' left-associative flattening of sums and products, and removal of DefDiff
#' evaluation artifacts for length-1 variables (`rep(c, length(.))`,
#' `sum(e)`, `as.numeric(e)`, `fast_*` kernel calls). Function applications on
#' constants (`sqrt(2)`, `log(2)`) and `pi` are kept symbolic.
#'
#' @param expr An R expression tree (call, symbol, or number).
#' @return The canonical tree. `normalize_tree()` is idempotent.
#' @export
#' @examples
#' normalize_tree(quote(0 + 1 * x^1 + (2 - 2) * y))   # x
normalize_tree <- function(expr) {
  for (k in seq_len(200L)) {
    new <- .norm_pass(expr)
    if (identical(new, expr)) return(new)
    expr <- new
  }
  stop("normalize_tree() did not reach a fixpoint", call. = FALSE)
}

# DefDiff kernel calls and their arithmetic meaning (scalar case).
.kernel_binary <- c(fast_vec_mul = "*", fast_vec_add = "+", fast_vec_sub = "-",
                    fast_vec_div = "/", fast_scalar_mul = "*", fast_scalar_div = "/",
                    metal_scalar_mul = "*")
.kernel_unary <- c(fast_vv_sin = "sin", fast_vv_cos = "cos", fast_vv_exp = "exp",
                   fast_vv_log = "log", fast_vv_sqrt = "sqrt", fast_vv_tanh = "tanh")

.norm_pass <- function(e) {
  if (is.integer(e)) return(as.double(e))
  if (is.numeric(e)) {
    if (length(e) != 1L) return(e)
    if (!is.null(attributes(e))) attributes(e) <- NULL
    e <- .snap(e)
    return(if (e < 0) call("-", -e) else e)
  }
  if (!is.call(e)) return(e)
  if (!is.symbol(e[[1L]])) return(e)
  head <- as.character(e[[1L]])
  if (head == "numeric_partial") {
    e[[2L]] <- .norm_pass(e[[2L]])
    return(e)
  }
  if (length(e) > 1L) {
    for (j in seq_along(e)[-1L]) {
      if (!is.null(e[[j]])) e[[j]] <- .norm_pass(e[[j]])
    }
  }
  .norm_node(e, head)
}

.norm_node <- function(e, head) {
  n <- length(e) - 1L
  # ---- syntax / DefDiff artifacts ----
  if (head == "(") return(e[[2L]])
  if (head %in% c("as.numeric", "as.vector", "sum", "drop") && n == 1L) return(e[[2L]])
  if (head == "rep" && n == 2L && .is_call(e[[3L]], "length")) return(e[[2L]])
  if (head == "c" && n == 1L) return(e[[2L]])
  if (head %in% names(.kernel_binary) && n == 2L) {
    return(call(.kernel_binary[[head]], e[[2L]], e[[3L]]))
  }
  if (head %in% names(.kernel_unary) && n == 1L) return(call(.kernel_unary[[head]], e[[2L]]))
  if (head == "fast_sum_sq" && n == 1L) return(call("^", e[[2L]], 2))
  if (head == "fast_sum_pow" && n == 2L) return(call("^", e[[2L]], e[[3L]]))
  if (head == "+" && n == 1L) return(e[[2L]])

  # ---- unary minus ----
  if (head == "-" && n == 1L) {
    a <- e[[2L]]
    if (.is_neg(a)) return(a[[2L]])
    if (.is_zero(a)) return(0)
    return(e)
  }
  if (!(head %in% c("+", "-", "*", "/", "^")) || n != 2L) return(e)
  a <- e[[2L]]
  b <- e[[3L]]

  # ---- constant folding ----
  va <- .num_value(a)
  vb <- .num_value(b)
  if (!is.null(va) && !is.null(vb)) {
    r <- .fold(head, va, vb)
    if (!is.null(r)) return(.make_num(r))
  }

  switch(head,
    "+" = {
      if (.is_zero(b)) return(a)
      if (.is_zero(a)) return(b)
      if (.is_neg(b)) return(call("-", a, b[[2L]]))
      if (.is_call(b, "+", 2L)) return(call("+", call("+", a, b[[2L]]), b[[3L]]))
      if (.is_call(b, "-", 2L)) return(call("-", call("+", a, b[[2L]]), b[[3L]]))
    },
    "-" = {
      if (.is_zero(b)) return(a)
      if (.is_zero(a)) return(call("-", b))
      if (identical(a, b)) return(0)
      if (.is_neg(b)) return(call("+", a, b[[2L]]))
      if (.is_call(b, "+", 2L)) return(call("-", call("-", a, b[[2L]]), b[[3L]]))
      if (.is_call(b, "-", 2L)) return(call("+", call("-", a, b[[2L]]), b[[3L]]))
    },
    "*" = {
      if (.is_zero(a) || .is_zero(b)) return(0)
      if (.is_one(a)) return(b)
      if (.is_one(b)) return(a)
      if (.is_neg(a)) return(call("-", call("*", a[[2L]], b)))
      if (.is_neg(b)) return(call("-", call("*", a, b[[2L]])))
      if (.is_call(b, "*", 2L)) return(call("*", call("*", a, b[[2L]]), b[[3L]]))
    },
    "/" = {
      if (.is_one(b)) return(a)
      if (.is_call(a, "/", 2L) && !is.null(.num_value(a[[3L]])) && !is.null(vb)) {
        r <- .fold("*", .num_value(a[[3L]]), vb)
        if (!is.null(r)) return(call("/", a[[2L]], .make_num(r)))
      }
      if (.is_zero(a) && !.is_zero(b)) return(0)
      if (.is_neg(a)) return(call("-", call("/", a[[2L]], b)))
      if (.is_neg(b)) return(call("-", call("/", a, b[[2L]])))
    },
    "^" = {
      if (.is_one(b)) return(a)
      if (.is_zero(b)) return(1)
      if (.is_one(a)) return(1)
    })
  e
}

.is_call <- function(e, head, n = NULL) {
  is.call(e) && identical(e[[1L]], as.name(head)) && (is.null(n) || length(e) == n + 1L)
}
.is_neg <- function(e) .is_call(e, "-", 1L)
.is_zero <- function(e) is.numeric(e) && length(e) == 1L && e == 0
.is_one <- function(e) is.numeric(e) && length(e) == 1L && e == 1

# Numeric value of a literal or of unary minus applied to a literal; NULL
# otherwise.
.num_value <- function(e) {
  if (is.numeric(e) && length(e) == 1L) return(as.double(e))
  if (.is_neg(e) && is.numeric(e[[2L]]) && length(e[[2L]]) == 1L) return(-as.double(e[[2L]]))
  NULL
}

.make_num <- function(x) if (x < 0) call("-", -x) else x

# Snap a double lying within a few ulp of a small rational p/q (q <= 1000) to
# that rational, so values DefDiff folded in floating point (1/3 - 1) print as
# the exact fraction. Exact rationals and all other doubles are unchanged.
.snap <- function(x) {
  if (!is.finite(x) || x == 0 || !is.null(.as_rational(x))) return(x)
  q <- seq_len(1000L)
  pq <- round(x * q)
  hit <- which(abs(pq / q - x) <= 4 * .Machine$double.eps * abs(x) & abs(pq) < 2^53)
  if (!length(hit)) return(x)
  pq[hit[1L]] / q[hit[1L]]
}

# Exact rational p/q (q <= 1000) for a double, or NULL.
.as_rational <- function(x, qmax = 1000L) {
  if (!is.finite(x)) return(NULL)
  q <- seq_len(qmax)
  pq <- round(x * q)
  hit <- which(pq / q == x & abs(pq) < 2^53)
  if (!length(hit)) return(NULL)
  c(pq[hit[1L]], q[hit[1L]])
}

.gcd <- function(a, b) {
  a <- abs(a); b <- abs(b)
  while (b > 0) {
    t <- a %% b; a <- b; b <- t
  }
  a
}

# Fold op(a, b). Exact rational arithmetic when both are small rationals,
# double arithmetic otherwise; NULL when the result is not finite. Powers fold
# only for integer exponents (2^(1/3) stays symbolic, like sqrt(2)).
.fold <- function(op, a, b) {
  if (op == "^" && b != round(b)) return(NULL)
  ra <- .as_rational(a)
  rb <- .as_rational(b)
  exact <- NULL
  if (!is.null(ra) && !is.null(rb)) {
    num <- switch(op,
      "+" = c(ra[1] * rb[2] + rb[1] * ra[2], ra[2] * rb[2]),
      "-" = c(ra[1] * rb[2] - rb[1] * ra[2], ra[2] * rb[2]),
      "*" = c(ra[1] * rb[1], ra[2] * rb[2]),
      "/" = if (rb[1] != 0) c(ra[1] * rb[2], ra[2] * rb[1]) * sign(rb[1]) else NULL,
      "^" = if (rb[2] == 1 && abs(rb[1]) <= 64) {
        k <- rb[1]
        if (k >= 0) c(ra[1]^k, ra[2]^k) else if (ra[1] != 0) c(ra[2]^-k, ra[1]^-k) * sign(ra[1])^k else NULL
      } else NULL)
    if (!is.null(num) && all(abs(num) < 2^53)) {
      g <- .gcd(num[1], num[2])
      if (g > 0) num <- num / g
      exact <- num[1] / num[2]
    }
  }
  r <- if (!is.null(exact)) exact else switch(op, "+" = a + b, "-" = a - b, "*" = a * b,
                                                "/" = a / b, "^" = a^b)
  if (!is.finite(r)) return(NULL)
  r
}
