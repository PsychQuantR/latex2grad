## differentiate.R
## Derivatives through DefDiff (design D2, D3, D6). latex2grad contains no
## derivative rules of its own: every symbolic derivative is a call to
## DefDiff::grad() on the formula lifted as sum(f) over length-1 variables,
## followed by the canonical cleanup. Sub-terms whose head is not a DefDiff
## generator are abstracted to opaque symbols; the chain rule across that
## boundary is completed with explicit numeric_partial() nodes.

# Heads DefDiff can differentiate (L_0 - L_3 of the active catalog).
.defdiff_heads <- function() {
  cat <- DefDiff::language_catalog()
  unique(unlist(cat[intersect(names(cat), c("L_0", "L_1", "L_2", "L_3"))]))
}

.oos_reasons <- c(
  gamma = "the Gamma function is not a DefDiff generator; its derivative needs digamma, which is outside the catalog",
  lgamma = "log-Gamma is not a DefDiff generator; its derivative needs digamma, which is outside the catalog",
  digamma = "polygamma functions are not DefDiff generators",
  trigamma = "polygamma functions are not DefDiff generators",
  factorial = "the factorial (Gamma(n + 1)) is not a DefDiff generator",
  qt = "the Student-t quantile is not a DefDiff generator, and its derivative in the degrees of freedom has no closed form",
  pt = "the Student-t CDF is not a DefDiff generator",
  dt = "the Student-t density is not a DefDiff generator",
  pnorm = "the normal CDF is not a DefDiff generator",
  qnorm = "the normal quantile is not a DefDiff generator",
  abs = "the absolute value is not a DefDiff generator (and is not differentiable at 0)",
  choose = "the binomial coefficient is not a DefDiff generator",
  beta = "the Beta function is not a DefDiff generator",
  lbeta = "the log-Beta function is not a DefDiff generator"
)

.oos_reason <- function(head) {
  r <- .oos_reasons[head]
  if (is.na(r)) paste0("`", head, "` is not a generator of DefDiff's language (catalog L_0-L_3)") else unname(r)
}

# Replace maximal sub-terms with an unknown head by opaque symbols .o<k>.
.abstract <- function(expr, heads) {
  terms <- list()
  syms <- character()
  walk <- function(e) {
    if (!is.call(e)) return(e)
    head <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
    if (!(head %in% heads)) {
      for (k in seq_along(terms)) if (identical(terms[[k]], e)) return(as.name(syms[k]))
      s <- paste0(".o", length(terms) + 1L)
      terms[[length(terms) + 1L]] <<- e
      syms <<- c(syms, s)
      return(as.name(s))
    }
    if (length(e) > 1L) for (j in seq_along(e)[-1L]) e[[j]] <- walk(e[[j]])
    e
  }
  out <- walk(expr)
  list(expr = out, terms = terms, syms = syms)
}

.substitute_syms <- function(expr, syms, terms) {
  if (!length(syms)) return(expr)
  if (is.symbol(expr)) {
    k <- match(as.character(expr), syms)
    return(if (is.na(k)) expr else terms[[k]])
  }
  if (is.call(expr) && length(expr) > 1L) {
    for (j in seq_along(expr)[-1L]) {
      if (!is.null(expr[[j]])) expr[[j]] <- .substitute_syms(expr[[j]], syms, terms)
    }
  }
  expr
}

# DefDiff::grad on the lifted formula. Maximal sub-trees free of `v` are
# frozen to constant symbols .c<k> first: DefDiff 0.2.0's walker builds no
# pullback for a power with a symbolic exponent, so e.g. sum(a / q^m) fails
# w.r.t. a ("walker returned a shim with no pullback"), and a constant
# sub-tree is a constant either way.
.defdiff_grad <- function(f, v) {
  fr <- .freeze_constants(f, v)
  d <- normalize_tree(DefDiff::grad(call("sum", fr$expr), v))
  normalize_tree(.substitute_syms(d, fr$syms, fr$terms))
}

.freeze_constants <- function(f, v) {
  terms <- list()
  syms <- character()
  walk <- function(e) {
    if (!is.call(e)) return(e)
    if (!(v %in% all.vars(e))) {
      for (k in seq_along(terms)) if (identical(terms[[k]], e)) return(as.name(syms[k]))
      s <- paste0(".c", length(terms) + 1L)
      terms[[length(terms) + 1L]] <<- e
      syms <<- c(syms, s)
      return(as.name(s))
    }
    if (length(e) > 1L) for (j in seq_along(e)[-1L]) e[[j]] <- walk(e[[j]])
    e
  }
  list(expr = walk(f), syms = syms, terms = terms)
}

# Numeric partial of g along v; a numeric_partial node extends its variables.
.numeric_partial_of <- function(g, v) {
  if (.is_call(g, "numeric_partial")) return(as.call(c(as.list(g), v)))
  call("numeric_partial", g, v)
}

.oos_entry <- function(term, head, v, treatment, reason) {
  list(term = term, latex = .tex(term), head = head, variable = v,
       treatment = treatment, reason = reason)
}

# Out-of-scope records as a data frame (the trees stay in attr "terms").
.oos_table <- function(entries) {
  col <- function(k) vapply(entries, function(x) x[[k]], "")
  df <- data.frame(latex = col("latex"), head = col("head"), variable = col("variable"),
                   treatment = col("treatment"), reason = col("reason"),
                   stringsAsFactors = FALSE)
  attr(df, "terms") <- lapply(entries, `[[`, "term")
  df
}

# Workaround for DefDiff 0.2.0: grad(sum(a^b), v) with v in the base and a
# symbolic exponent b fails inside the power rule ("non-numeric argument to
# binary operator"). On that failure the power is retried as exp(b * log(a))
# (valid for a > 0) and the result is turned back into a^b.
.explog_note <- paste0("DefDiff 0.2.0 cannot differentiate a^b in a when the exponent b is symbolic; ",
                       "such powers were differentiated as exp(b log a), valid for a > 0")

.pow_to_explog <- function(e, v) {
  if (!is.call(e)) return(e)
  if (length(e) > 1L) for (j in seq_along(e)[-1L]) e[[j]] <- .pow_to_explog(e[[j]], v)
  if (.is_call(e, "^", 2L) && is.null(.num_value(e[[3L]])) &&
      v %in% all.vars(e[[2L]]) && !(v %in% all.vars(e[[3L]]))) {
    return(call("exp", call("*", e[[3L]], call("log", e[[2L]]))))
  }
  e
}

.explog_to_pow <- function(e) {
  if (!is.call(e)) return(e)
  if (length(e) > 1L) for (j in seq_along(e)[-1L]) e[[j]] <- .explog_to_pow(e[[j]])
  if (.is_call(e, "exp", 1L)) {
    arg <- e[[2L]]
    neg <- .is_neg(arg)
    if (neg) arg <- arg[[2L]]
    if (.is_call(arg, "*", 2L) && .is_call(arg[[3L]], "log", 1L)) {
      expo <- if (neg) call("-", arg[[2L]]) else arg[[2L]]
      return(call("^", arg[[3L]][[2L]], expo))
    }
  }
  e
}

# d f / d v. Returns list(expr, oos, notes).
.partial <- function(f, v, heads = .defdiff_heads()) {
  ab <- .abstract(f, heads)
  oos <- list()
  notes <- character()
  fallback <- function(err) {
    list(expr = normalize_tree(.numeric_partial_of(f, v)), fallback = TRUE,
         oos = list(.oos_entry(f, "(whole formula)", v, "numeric",
                               paste0("DefDiff refused the formula: ", paste(conditionMessage(err), collapse = " ")))),
         notes = notes)
  }
  dd <- function(target, by) {
    r <- tryCatch(.defdiff_grad(target, by), error = function(e) e)
    if (!inherits(r, "error")) return(r)
    alt <- .pow_to_explog(target, by)
    if (identical(alt, target)) return(r)
    r2 <- tryCatch(.defdiff_grad(alt, by), error = function(e) e)
    if (inherits(r2, "error")) return(r)
    notes <<- union(notes, .explog_note)
    normalize_tree(.explog_to_pow(r2))
  }
  total <- dd(ab$expr, v)
  if (inherits(total, "error")) return(fallback(total))
  for (k in seq_along(ab$terms)) {
    g <- ab$terms[[k]]
    head <- as.character(g[[1L]])
    derived <- head == "numeric_partial"
    if (!(v %in% all.vars(g))) {
      if (!derived) {
        oos[[length(oos) + 1L]] <- .oos_entry(g, head, v, paste0("constant w.r.t. ", v), .oos_reason(head))
      }
      next
    }
    dfo <- dd(ab$expr, ab$syms[k])
    if (inherits(dfo, "error")) return(fallback(dfo))
    total <- call("+", total, call("*", dfo, .numeric_partial_of(g, v)))
    if (!derived) oos[[length(oos) + 1L]] <- .oos_entry(g, head, v, "numeric", .oos_reason(head))
  }
  total <- .substitute_syms(total, ab$syms, ab$terms)
  list(expr = normalize_tree(total), oos = oos, notes = notes)
}

.merge_notes <- function(parts) unique(unlist(lapply(parts, `[[`, "notes")))

# TRUE for entries DefDiff refused entirely (whole-formula numeric fallback).
.fully_numeric <- function(parts) vapply(parts, function(p) isTRUE(p$fallback), TRUE)

.merge_oos <- function(parts) {
  oos <- do.call(c, lapply(parts, `[[`, "oos"))
  if (is.null(oos)) oos <- list()
  key <- vapply(oos, function(x) paste(x$latex, x$variable), "")
  .oos_table(oos[!duplicated(key)])
}

# Entry point used by latex2grad(). `f` is the lowered canonical tree.
.differentiate <- function(f, wrt, type) {
  heads <- .defdiff_heads()
  if (type == "grad") {
    if (.is_call(f, "c")) stop("the formula is vector-valued; use type = \"jacobian\"", call. = FALSE)
    parts <- lapply(wrt, function(v) .partial(f, v, heads))
    entries <- lapply(parts, `[[`, "expr")
    names(entries) <- wrt
    return(list(entries = entries, gradient = NULL, oos = .merge_oos(parts),
                notes = .merge_notes(parts), fully_numeric = .fully_numeric(parts),
                labels = .entry_labels(type, wrt)))
  }
  if (type == "hessian") {
    if (.is_call(f, "c")) stop("the Hessian needs a scalar formula", call. = FALSE)
    gparts <- lapply(wrt, function(v) .partial(f, v, heads))
    gradient <- lapply(gparts, `[[`, "expr")
    names(gradient) <- wrt
    m <- length(wrt)
    hparts <- vector("list", m * m)
    entries <- vector("list", m * m)
    for (j in seq_len(m)) for (i in seq_len(m)) {
      k <- (j - 1L) * m + i
      hparts[[k]] <- .partial(gradient[[j]], wrt[i], heads)
      entries[[k]] <- hparts[[k]]$expr
    }
    dim(entries) <- c(m, m)
    dimnames(entries) <- list(wrt, wrt)
    return(list(entries = entries, gradient = gradient, oos = .merge_oos(c(gparts, hparts)),
                notes = .merge_notes(c(gparts, hparts)),
                fully_numeric = .fully_numeric(hparts) | rep(.fully_numeric(gparts), each = m),
                labels = .entry_labels(type, wrt)))
  }
  comps <- if (.is_call(f, "c")) as.list(f)[-1L] else list(f)
  m <- length(comps)
  parts <- vector("list", m * length(wrt))
  entries <- vector("list", m * length(wrt))
  for (j in seq_along(wrt)) for (i in seq_len(m)) {
    k <- (j - 1L) * m + i
    parts[[k]] <- .partial(comps[[i]], wrt[j], heads)
    entries[[k]] <- parts[[k]]$expr
  }
  dim(entries) <- c(m, length(wrt))
  dimnames(entries) <- list(paste0("f_", seq_len(m)), wrt)
  list(entries = entries, gradient = NULL, components = comps, oos = .merge_oos(parts),
       notes = .merge_notes(parts), fully_numeric = .fully_numeric(parts),
       labels = .entry_labels(type, wrt, m))
}

# LaTeX labels for the derivative entries, in the storage order of `entries`.
.entry_labels <- function(type, wrt, m = 1L) {
  d <- vapply(wrt, .symbol_to_latex, "")
  if (type == "grad") return(paste0("\\frac{\\partial f}{\\partial ", d, "}"))
  if (type == "hessian") {
    out <- character()
    for (j in seq_along(wrt)) for (i in seq_along(wrt)) {
      out <- c(out, if (i == j) paste0("\\frac{\\partial^{2} f}{\\partial ", d[i], "^{2}}") else
        paste0("\\frac{\\partial^{2} f}{\\partial ", d[i], " \\, \\partial ", d[j], "}"))
    }
    return(out)
  }
  out <- character()
  for (j in seq_along(wrt)) for (i in seq_len(m)) {
    out <- c(out, paste0("\\frac{\\partial f_{", i, "}}{\\partial ", d[j], "}"))
  }
  out
}
