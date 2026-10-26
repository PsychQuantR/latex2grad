## certify.R
## The certificate (design D7): round-trip of every printed tree, DefDiff
## closure levels, and a seeded random-point comparison against
## Richardson-extrapolated central differences.

.level_ranks <- c(L_0 = 0, L_1 = 1, L_2 = 2, L_3 = 3, L_4 = 4, unknown = 5)

.has_numeric_partial <- function(e) {
  if (!is.call(e)) return(FALSE)
  if (.is_call(e, "numeric_partial")) return(TRUE)
  any(vapply(as.list(e)[-1L], .has_numeric_partial, TRUE))
}

# normalize(parse(print(e))) == e
.roundtrip <- function(e, functions = NULL) {
  s <- tryCatch(.tex(e), error = function(err) err)
  if (inherits(s, "error")) return(list(ok = FALSE, message = paste("print failed:", conditionMessage(s))))
  back <- tryCatch(parse_latex(s, vars = all.vars(e), functions = functions)$expr,
                   error = function(err) err)
  if (inherits(back, "error")) {
    return(list(ok = FALSE, message = paste("re-parse failed:", conditionMessage(back))))
  }
  ok <- identical(normalize_tree(back), e)
  list(ok = ok, message = if (ok) "" else "re-parsed tree differs")
}

# Run `code` with a fixed seed without disturbing the caller's RNG stream.
.with_seed <- function(seed, code) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, envir = genv) else
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv))
  set.seed(seed)
  code
}

.rel_err <- function(a, b) abs(a - b) / max(1, abs(b))

# x: a latex2grad object with parsed/derivative filled in.
.certify <- function(x, n_points, tol, seed, domain, functions) {
  src <- x$parsed$canonical
  entries <- x$derivative$entries
  labels <- x$derivative$labels
  n_entries <- length(entries)

  # ---- round-trip ----
  items <- c("source", labels)
  rts <- c(list(.roundtrip(src, functions)), lapply(entries, .roundtrip, functions = functions))
  if (!is.null(x$derivative$gradient)) {
    items <- c(items, paste0("gradient ", names(x$derivative$gradient)))
    rts <- c(rts, lapply(x$derivative$gradient, .roundtrip, functions = functions))
  }
  roundtrip <- data.frame(item = items, ok = vapply(rts, `[[`, TRUE, "ok"),
                          message = vapply(rts, `[[`, "", "message"), stringsAsFactors = FALSE)

  # ---- closure levels ----
  lowered <- x$parsed$lowered
  src_parts <- if (.is_call(lowered, "c")) as.list(lowered)[-1L] else list(lowered)
  ranks <- .level_ranks[vapply(src_parts, function(e) DefDiff::level(e), "")]
  lev_src <- names(.level_ranks)[match(max(ranks), .level_ranks)]
  lev_ent <- vapply(entries, function(e) DefDiff::level(e), "")
  has_np <- vapply(entries, .has_numeric_partial, TRUE)
  closure <- if (lev_src == "unknown") NA else
    all(.level_ranks[lev_ent[!has_np]] <= .level_ranks[[lev_src]])
  levels <- list(source = lev_src, entries = lev_ent, n_numeric = sum(has_np), closure = closure)

  # ---- numeric ----
  syms <- union(.free_symbols(src), x$wrt)
  dom <- lapply(syms, function(s) {
    d <- domain[[s]]
    if (is.null(d)) c(0.5, 1.5) else as.numeric(d)
  })
  names(dom) <- syms
  refs <- .reference_fns(x)
  max_err <- rep(0, n_entries)
  g_err <- if (!is.null(x$derivative$gradient)) rep(0, length(x$derivative$gradient)) else numeric()
  n_valid <- 0L
  n_rejected <- 0L
  first_problem <- NULL
  max_attempts <- max(50L, 20L * n_points)
  .with_seed(seed, {
    attempts <- 0L
    while (n_valid < n_points && attempts < max_attempts) {
      attempts <- attempts + 1L
      vals <- lapply(dom, function(d) stats::runif(1L, d[1L], d[2L]))
      res <- tryCatch({
        f0 <- .eval_tree(src, vals)
        if (!.finite_all(f0)) stop("the formula is not finite at a sampled point")
        sym <- vapply(entries, function(e) as.numeric(.eval_tree(e, vals)), 0)
        num <- vapply(refs$entries, function(r) r(vals), 0)
        gs <- vapply(x$derivative$gradient %||% list(), function(e) as.numeric(.eval_tree(e, vals)), 0)
        gn <- vapply(refs$gradient, function(r) r(vals), 0)
        if (!all(is.finite(c(sym, num, gs, gn)))) stop("the derivative is not finite at a sampled point")
        list(sym = sym, num = num, gs = gs, gn = gn)
      }, error = function(e) {
        if (is.null(first_problem)) first_problem <<- conditionMessage(e)
        NULL
      }, warning = function(w) {
        if (is.null(first_problem)) first_problem <<- conditionMessage(w)
        NULL
      })
      if (is.null(res)) {
        n_rejected <- n_rejected + 1L
        next
      }
      n_valid <- n_valid + 1L
      max_err <- pmax(max_err, mapply(.rel_err, res$sym, res$num))
      if (length(g_err)) g_err <- pmax(g_err, mapply(.rel_err, res$gs, res$gn))
    }
  })
  per_entry <- data.frame(entry = labels, max_rel_error = if (n_valid) max_err else NA_real_,
                          numeric_partial = has_np, stringsAsFactors = FALSE)
  if (length(g_err)) {
    per_entry <- rbind(per_entry, data.frame(
      entry = paste0("\\frac{\\partial f}{\\partial ", vapply(names(x$derivative$gradient), .symbol_to_latex, ""), "}"),
      max_rel_error = if (n_valid) g_err else NA_real_,
      numeric_partial = vapply(x$derivative$gradient, .has_numeric_partial, TRUE),
      stringsAsFactors = FALSE))
  }
  overall <- if (n_valid) max(per_entry$max_rel_error) else NA_real_
  enough <- n_valid >= max(1L, ceiling(n_points / 2))
  numeric <- list(n_requested = n_points, n_valid = n_valid, n_rejected = n_rejected,
                  seed = seed, tol = tol, domain = dom, per_entry = per_entry,
                  max_rel_error = overall, pass = enough && isTRUE(overall <= tol),
                  enough_points = enough, first_problem = first_problem)

  failures <- character()
  fully <- x$derivative$fully_numeric |
    vapply(entries, .only_numeric, TRUE, vars = x$wrt)
  if (any(fully)) {
    failures <- c(failures, paste0(
      "the dependence on the variables of ", paste(labels[fully], collapse = "; "),
      " lies entirely inside numeric-partial nodes (out-of-scope terms or a formula DefDiff refused), ",
      "so comparing it with central differences is circular"))
  }
  if (!all(roundtrip$ok)) {
    failures <- c(failures, paste0("round-trip failed for: ", paste(roundtrip$item[!roundtrip$ok], collapse = "; ")))
  }
  if (!enough) {
    failures <- c(failures, paste0(
      sprintf("only %d of %d requested test points were valid (%d rejected", n_valid, n_points, n_rejected),
      if (!is.null(first_problem)) paste0("; first problem: ", first_problem), ")"))
  } else if (!isTRUE(overall <= tol)) {
    worst <- per_entry$entry[which.max(per_entry$max_rel_error)]
    failures <- c(failures, sprintf("max relative error %.3g exceeds the tolerance %.3g (entry %s)",
                                    overall, tol, worst))
  }
  list(roundtrip = roundtrip, levels = levels, numeric = numeric,
       pass = length(failures) == 0L, failures = failures)
}

# TRUE when an entry's dependence on `vars` lies entirely inside
# numeric_partial() nodes: there is no symbolic content left to check.
.only_numeric <- function(e, vars) {
  if (!.has_numeric_partial(e)) return(FALSE)
  !any(vars %in% all.vars(.drop_numeric_partials(e)))
}

.drop_numeric_partials <- function(e) {
  if (.is_call(e, "numeric_partial")) return(1)
  if (is.call(e) && length(e) > 1L) {
    for (j in seq_along(e)[-1L]) e[[j]] <- .drop_numeric_partials(e[[j]])
  }
  e
}

.finite_all <- function(v) is.numeric(v) && length(v) >= 1L && all(is.finite(v))

`%||%` <- function(a, b) if (is.null(a)) b else a

# Reference (numeric) functions, one per entry, in entry storage order.
.reference_fns <- function(x) {
  d <- x$derivative
  wrt <- x$wrt
  src <- x$parsed$canonical
  mk <- function(expr, v) {
    force(expr); force(v)
    function(vals) .numeric_d(expr, v, vals)
  }
  if (x$type == "grad") return(list(entries = lapply(wrt, function(v) mk(src, v)), gradient = list()))
  if (x$type == "hessian") {
    m <- length(wrt)
    ents <- list()
    for (j in seq_len(m)) for (i in seq_len(m)) ents[[length(ents) + 1L]] <- mk(d$gradient[[j]], wrt[i])
    return(list(entries = ents, gradient = lapply(wrt, function(v) mk(src, v))))
  }
  comps <- if (.is_call(src, "c")) as.list(src)[-1L] else list(src)
  ents <- list()
  for (j in seq_along(wrt)) for (i in seq_along(comps)) ents[[length(ents) + 1L]] <- mk(comps[[i]], wrt[j])
  list(entries = ents, gradient = list())
}
