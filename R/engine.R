## engine.R
## Differentiation through DefDiff, with the boundary made explicit.
##
## DefDiff differentiates every sub-expression it can. Where it refuses a
## node (a function outside its L_0-L_4 catalog, or a shape its walker does
## not close, such as a power with a symbolic exponent), the chain rule is
## assembled here around that node:
##
##   d/dv h(a_1, ..., a_k) = sum_i  dh/da_i (a) * d a_i / dv
##
## The inner derivatives d a_i / dv come from DefDiff again (recursively).
## The local partial dh/da_i is taken, in order of preference, from
##   1. DefDiff applied to h(s_1, ..., s_k) with fresh symbols s_j;
##   2. the supplementary closed-form table below;
##   3. a numeric derivative node numd(h(..., s, ...), s, a_i), evaluated by
##      Richardson extrapolation.
## Cases 2 and 3 are recorded as boundary entries with DefDiff's reason, and
## reported in the "Out of scope" section.

# Closed-form partials for functions DefDiff 0.2.0 does not differentiate.
# Each entry: nargs, and one function per argument slot taking the argument
# list and returning the partial derivative (NULL = no closed form).
.l2g_supplementary <- local({
  p1 <- function(f) list(nargs = 1L, partials = list(f))
  list(
    "^" = list(nargs = 2L, partials = list(
      function(a) bquote(.(a[[2L]]) * .(a[[1L]])^(.(a[[2L]]) - 1)),
      function(a) bquote(.(a[[1L]])^.(a[[2L]]) * log(.(a[[1L]]))))),
    tan   = p1(function(a) bquote(1 / cos(.(a[[1L]]))^2)),
    asin  = p1(function(a) bquote(1 / sqrt(1 - .(a[[1L]])^2))),
    acos  = p1(function(a) bquote(-(1 / sqrt(1 - .(a[[1L]])^2)))),
    sinh  = p1(function(a) bquote(cosh(.(a[[1L]])))),
    cosh  = p1(function(a) bquote(sinh(.(a[[1L]])))),
    asinh = p1(function(a) bquote(1 / sqrt(.(a[[1L]])^2 + 1))),
    acosh = p1(function(a) bquote(1 / sqrt(.(a[[1L]])^2 - 1))),
    atanh = p1(function(a) bquote(1 / (1 - .(a[[1L]])^2))),
    log10 = p1(function(a) bquote(1 / (.(a[[1L]]) * log(10)))),
    log1p = p1(function(a) bquote(1 / (1 + .(a[[1L]])))),
    expm1 = p1(function(a) bquote(exp(.(a[[1L]])))),
    abs   = p1(function(a) bquote(sign(.(a[[1L]])))),
    sign  = p1(function(a) 0),
    gamma = p1(function(a) bquote(gamma(.(a[[1L]])) * digamma(.(a[[1L]])))),
    lgamma = p1(function(a) bquote(digamma(.(a[[1L]])))),
    digamma = p1(function(a) bquote(trigamma(.(a[[1L]])))),
    trigamma = p1(function(a) bquote(psigamma(.(a[[1L]]), 2))),
    psigamma = list(nargs = 2L, partials = list(
      function(a) if (.l2g_is_num(a[[2L]])) bquote(psigamma(.(a[[1L]]), .(a[[2L]] + 1))) else NULL,
      NULL)),
    beta = list(nargs = 2L, partials = list(
      function(a) bquote(beta(.(a[[1L]]), .(a[[2L]])) *
                           (digamma(.(a[[1L]])) - digamma(.(a[[1L]]) + .(a[[2L]])))),
      function(a) bquote(beta(.(a[[1L]]), .(a[[2L]])) *
                           (digamma(.(a[[2L]])) - digamma(.(a[[1L]]) + .(a[[2L]])))))),
    lbeta = list(nargs = 2L, partials = list(
      function(a) bquote(digamma(.(a[[1L]])) - digamma(.(a[[1L]]) + .(a[[2L]]))),
      function(a) bquote(digamma(.(a[[2L]])) - digamma(.(a[[1L]]) + .(a[[2L]]))))),
    choose = list(nargs = 2L, partials = list(
      function(a) bquote(choose(.(a[[1L]]), .(a[[2L]])) *
                           (digamma(.(a[[1L]]) + 1) - digamma(.(a[[1L]]) - .(a[[2L]]) + 1))),
      function(a) bquote(choose(.(a[[1L]]), .(a[[2L]])) *
                           (digamma(.(a[[1L]]) - .(a[[2L]]) + 1) - digamma(.(a[[2L]]) + 1))))),
    pnorm = p1(function(a) bquote(dnorm(.(a[[1L]])))),
    dnorm = p1(function(a) bquote(-(.(a[[1L]]) * dnorm(.(a[[1L]]))))),
    qnorm = p1(function(a) bquote(1 / dnorm(qnorm(.(a[[1L]]))))),
    erf   = p1(function(a) bquote(2 / sqrt(pi) * exp(-.(a[[1L]])^2))),
    erfc  = p1(function(a) bquote(-(2 / sqrt(pi) * exp(-.(a[[1L]])^2)))),
    pt = list(nargs = 2L, partials = list(
      function(a) bquote(dt(.(a[[1L]]), .(a[[2L]]))),
      NULL)),
    dt = list(nargs = 2L, partials = list(
      function(a) bquote(-((.(a[[2L]]) + 1) * .(a[[1L]]) / (.(a[[2L]]) + .(a[[1L]])^2) *
                             dt(.(a[[1L]]), .(a[[2L]])))),
      function(a) bquote(dt(.(a[[1L]]), .(a[[2L]])) / 2 *
                           (digamma((.(a[[2L]]) + 1) / 2) - digamma(.(a[[2L]]) / 2) -
                              1 / .(a[[2L]]) - log(1 + .(a[[1L]])^2 / .(a[[2L]])) +
                              (.(a[[2L]]) + 1) * .(a[[1L]])^2 /
                              (.(a[[2L]]) * (.(a[[2L]]) + .(a[[1L]])^2)))))),
    qt = list(nargs = 2L, partials = list(
      function(a) bquote(1 / dt(qt(.(a[[1L]]), .(a[[2L]])), .(a[[2L]]))),
      NULL))
  )
})

#' Differentiate an expression through DefDiff
#'
#' Computes the symbolic partial derivatives of a scalar expression with
#' respect to each variable in `wrt`, delegating to DefDiff wherever it can
#' and assembling the chain rule around nodes DefDiff refuses (see Details).
#'
#' Nodes outside DefDiff's language are handled by a closed-form partial from
#' latex2grad's supplementary table when one is known (e.g. the gamma family,
#' normal and Student-t functions, a power with a symbolic exponent) and
#' otherwise by a numeric derivative node `numd(body, s, at)`, evaluated by
#' Richardson extrapolation. Each such node is returned in `boundaries`.
#'
#' @param expr A canonical R expression (see [tex_normalize()]).
#' @param wrt Character vector of variable names.
#' @return A list with `derivatives` (a named list of canonical expressions)
#'   and `boundaries` (a list of records: `fun`, `slot`, `nargs`, `kind`
#'   (`"closed form"` or `"numeric"`), `template` (the node over placeholder
#'   arguments), `rule` (the closed-form partial, or `NULL`), `reason`,
#'   `count`).
#' @export
#' @examples
#' \dontrun{
#' l2g_differentiate(quote(x^2 * sin(y)), c("x", "y"))$derivatives
#' l2g_differentiate(quote(lgamma(nu / 2)), "nu")$boundaries
#' }
l2g_differentiate <- function(expr, wrt) {
  ctx <- .l2g_new_ctx(expr)
  out <- lapply(wrt, function(v) tex_normalize(.l2g_D(expr, v, ctx)))
  names(out) <- wrt
  list(derivatives = out, boundaries = ctx$boundaries)
}

.l2g_new_ctx <- function(expr) {
  ctx <- new.env(parent = emptyenv())
  ctx$boundaries <- list()
  ctx$used <- unique(c(all.names(expr), "pi"))
  ctx
}

# Free variables, binder-aware.
.l2g_free_vars <- function(e) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    return(if (nzchar(nm)) nm else character(0))
  }
  if (!is.call(e)) return(character(0))
  op <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
  if (op %in% .l2g_binders && length(e) >= 3L) {
    inner <- setdiff(.l2g_free_vars(e[[2L]]), as.character(e[[3L]]))
    rest <- unlist(lapply(as.list(e)[-(1:3)], .l2g_free_vars))
    return(unique(c(inner, rest)))
  }
  unique(unlist(lapply(as.list(e)[-1L], .l2g_free_vars)))
}

.l2g_depends <- function(e, v) v %in% .l2g_free_vars(e)

# A bound-variable name used neither in the formula nor anywhere inside the
# node it will bind (sibling numd nodes may share a name; nested ones never
# shadow each other).
.l2g_fresh <- function(ctx, node, prefer = c("s", "xi", "zeta", "tau", "eta")) {
  avoid <- c(ctx$used, all.names(node))
  for (p in prefer) if (!(p %in% avoid)) return(p)
  k <- 1L
  repeat {
    p <- paste0("s_", k)
    if (!(p %in% avoid)) return(p)
    k <- k + 1L
  }
}

# DefDiff on a scalar expression: grad(sum(e), v) with the shape artifacts of
# the vector API removed by the normalizer. Returns list(ok, value | reason).
.l2g_dd <- function(e, v) {
  out <- tryCatch(DefDiff::grad(call("sum", e), v), error = function(err) err)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, reason = conditionMessage(out), class = class(out)[[1L]]))
  }
  val <- tryCatch(tex_normalize(out), latex2grad_error = function(err) err)
  if (inherits(val, "error")) return(list(ok = FALSE, reason = conditionMessage(val)))
  list(ok = TRUE, value = val)
}

.l2g_D <- function(e, v, ctx) {
  if (!.l2g_depends(e, v)) return(0)
  r <- .l2g_dd(e, v)
  if (r$ok) return(r$value)
  if (is.symbol(e)) return(1)
  op <- as.character(e[[1L]])
  if (op %in% .l2g_binders) {
    # A binder DefDiff refuses (e.g. an integral over an out-of-catalog
    # integrand): differentiate the whole node numerically.
    s <- .l2g_fresh(ctx, e)
    binds <- list(as.name(s)); names(binds) <- v
    body <- .l2g_subst(e, binds)
    .l2g_record(ctx, op, NA_integer_, length(e) - 1L, "numeric", NULL, NULL,
                .l2g_reason(op, NA_integer_, r$reason))
    return(call("numd", body, as.name(s), as.name(v)))
  }
  args <- as.list(e)[-1L]
  terms <- list()
  for (i in seq_along(args)) {
    if (!.l2g_depends(args[[i]], v)) next
    da <- .l2g_D(args[[i]], v, ctx)
    if (.l2g_is_num(da, 0)) next
    p <- .l2g_local_partial(e, op, args, i, ctx)
    terms[[length(terms) + 1L]] <- call("*", p, da)
  }
  if (!length(terms)) return(0)
  tex_normalize(Reduce(function(a, b) call("+", a, b), terms))
}

# Partial derivative of h(args) with respect to argument slot i.
.l2g_local_partial <- function(e, op, args, i, ctx) {
  k <- length(args)
  syms <- paste0(".l2g_a", seq_len(k))
  is_lit <- function(x) .l2g_is_num(x) || (.l2g_neg(x) && .l2g_is_num(x[[2L]]))
  tmpl_args <- lapply(seq_len(k), function(j) {
    if (j != i && is_lit(args[[j]])) args[[j]] else as.name(syms[[j]])
  })
  tmpl <- as.call(c(list(as.name(op)), tmpl_args))
  r <- .l2g_dd(tmpl, syms[[i]])
  binds <- args; names(binds) <- syms
  if (r$ok) return(.l2g_subst(r$value, binds))

  # Display form of the rule, over placeholder arguments a, b, c, ...
  shown <- lapply(.l2g_placeholders(k), as.name)
  template <- as.call(c(list(as.name(op)), shown))
  supp <- .l2g_supplementary[[op]]
  if (!is.null(supp) && supp$nargs == k && !is.null(supp$partials[[i]])) {
    rule <- supp$partials[[i]](args)
    if (!is.null(rule)) {
      .l2g_record(ctx, op, i, k, "closed form", template,
                  tex_normalize(supp$partials[[i]](shown)), .l2g_reason(op, i, r$reason))
      return(rule)
    }
  }
  s <- .l2g_fresh(ctx, e)
  body_args <- args
  body_args[[i]] <- as.name(s)
  body <- as.call(c(list(as.name(op)), body_args))
  .l2g_record(ctx, op, i, k, "numeric", template, NULL, .l2g_reason(op, i, r$reason))
  call("numd", body, as.name(s), args[[i]])
}

# Human-readable reason for DefDiff's refusal.
.l2g_reason <- function(op, i, raw) {
  if (op == "^" && identical(i, 1L)) {
    return(paste0("DefDiff 0.2.0 differentiates a power with respect to its base ",
                  "only when the exponent is a numeric literal"))
  }
  g <- regmatches(raw, regexec("Unknown (L_3 )?generator[^:`]*(: |`)([A-Za-z0-9_.]+)", raw))[[1L]]
  if (length(g)) {
    g <- sub("[.]+$", "", g[[4L]])
    if (identical(g, op)) {
      return(paste0("`", op, "` is not in DefDiff's L_0-L_4 generator catalog"))
    }
    return(paste0("contains `", g, "`, which is not in DefDiff's L_0-L_4 generator catalog"))
  }
  raw <- gsub("\\.l2g_a([0-9]+)", "a\\1", raw)
  paste0("DefDiff: ", raw)
}

.l2g_placeholders <- function(k) {
  if (k <= 4L) c("a", "b", "c", "d")[seq_len(k)] else paste0("a_", seq_len(k))
}

.l2g_record <- function(ctx, fun, slot, nargs, kind, template, rule, reason) {
  key <- paste(fun, slot, nargs, kind, sep = "|")
  for (j in seq_along(ctx$boundaries)) {
    if (identical(ctx$boundaries[[j]]$key, key)) {
      ctx$boundaries[[j]]$count <- ctx$boundaries[[j]]$count + 1L
      return(invisible())
    }
  }
  ctx$boundaries[[length(ctx$boundaries) + 1L]] <- list(
    key = key, fun = fun, slot = slot, nargs = nargs, kind = kind,
    template = template, rule = rule, reason = reason, count = 1L)
  invisible()
}
