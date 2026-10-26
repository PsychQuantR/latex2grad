## latex2grad.R
## The pipeline: entry (OCR / LLM disambiguation, the only non-deterministic
## steps) -> parse -> where-substitution -> canonical form -> lowering ->
## DefDiff differentiation -> certificate.

#' Differentiate a LaTeX formula, with a certificate
#'
#' Parses `latex` with the deterministic grammar of [parse_latex()],
#' normalizes it ([normalize_tree()]), lowers common functions into DefDiff's
#' catalog, differentiates with DefDiff, and certifies the result (round-trip
#' of every printed tree plus a seeded random-point comparison against
#' Richardson-extrapolated central differences). Sub-terms outside DefDiff's
#' language are listed in `$out_of_scope` and differentiated numerically.
#'
#' A failure never throws away the work: if the input cannot be parsed or a
#' check fails, the returned object records why and [l2g_report()] still
#' renders a report whose conclusion states the failure.
#'
#' @param latex A LaTeX math string. May be `NULL` when `image` is given.
#' @param wrt Character vector of variables to differentiate with respect to.
#'   Defaults to every free symbol, in order of appearance. Other symbols are
#'   parameters (held constant).
#' @param type `"grad"`, `"hessian"`, or `"jacobian"` (vector input written as
#'   a `pmatrix`/`bmatrix`; a scalar input gives a 1 x n row).
#' @param where Optional named character vector of LaTeX definitions
#'   substituted into the formula, e.g. `c(x_1 = "t_\\nu^{-1}(u_1)")`.
#' @param vars,functions,strict Passed to [parse_latex()].
#' @param domain Named list of sampling intervals for the numeric check, e.g.
#'   `list(nu = c(3, 12))`. Symbols not listed are sampled from `[0.5, 1.5]`.
#' @param n_points Number of random test points.
#' @param tol Tolerance on the maximum relative error `|a - b| / max(1, |b|)`.
#' @param seed Seed for the test points (the caller's RNG state is restored).
#' @param disambiguate Optional entry hook, e.g. [llm_claude()]: called with
#'   `(latex, problem)` when parsing fails (or, with `strict = TRUE`, meets an
#'   ambiguity); must return `list(latex = <rewritten>, decisions = <character>)`.
#' @param image Optional path to an image of the formula (requires `ocr`).
#' @param ocr Optional OCR hook, e.g. [ocr_claude()]: called with the image
#'   path; returns the LaTeX string or `list(latex = , notes = )`.
#' @return An object of class `latex2grad`.
#' @export
#' @examples
#' \dontrun{
#' r <- latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"))
#' r
#' l2g_report(r, file = "report.tex")
#' }
latex2grad <- function(latex = NULL, wrt = NULL, type = c("grad", "hessian", "jacobian"),
                       where = NULL, vars = NULL, functions = NULL, domain = NULL,
                       n_points = 25L, tol = 1e-6, seed = 1L, strict = FALSE,
                       disambiguate = NULL, image = NULL, ocr = NULL) {
  type <- match.arg(type)
  obj <- structure(list(source = list(latex = latex, image = image, ocr_model = NULL,
                                      rewritten = NULL),
                        type = type, wrt = wrt, parameters = character(),
                        assumptions = .assumption_df(list()), parsed = NULL,
                        derivative = NULL, out_of_scope = .oos_table(list()),
                        notes = character(), certificate = NULL, error = NULL),
                   class = "latex2grad")
  entry <- list()

  # ---- entry: OCR ----
  if (is.null(latex)) {
    if (is.null(image)) stop("give `latex` or `image`", call. = FALSE)
    if (!is.function(ocr)) stop("`image` needs an `ocr` hook, e.g. ocr = ocr_claude()", call. = FALSE)
    o <- ocr(image)
    latex <- if (is.list(o)) o$latex else o
    if (!is.character(latex) || length(latex) != 1L) stop("the ocr hook must return a LaTeX string", call. = FALSE)
    model <- .hook_model(ocr)
    obj$source$latex <- latex
    obj$source$ocr_model <- model
    entry[[length(entry) + 1L]] <- list(kind = "ocr", text = paste0(
      "The formula was transcribed from the image `", basename(image), "` by ", model,
      " (OCR); transcription: `", latex, "`"))
    for (nt in if (is.list(o)) o$notes else NULL) {
      entry[[length(entry) + 1L]] <- list(kind = "ocr", text = paste0("[OCR ", model, "] ", nt))
    }
  }

  # ---- entry: parse, with optional disambiguation ----
  pe <- .parse_entry(latex, vars, functions, strict, disambiguate)
  entry <- c(entry, pe$entry)
  obj$source$rewritten <- pe$rewritten
  if (!is.null(pe$error)) {
    obj$assumptions <- .assumption_df(entry)
    return(.fail(obj, paste0("the input could not be parsed: ", conditionMessage(pe$error))))
  }
  parsed <- pe$parsed
  items <- c(entry, .df_items(parsed$assumptions))

  # ---- where-definitions ----
  tree <- parsed$expr
  defs <- data.frame(symbol = character(), latex = character(), stringsAsFactors = FALSE)
  def_trees <- list()
  if (length(where)) {
    if (is.null(names(where)) || any(!nzchar(names(where)))) stop("`where` must be named", call. = FALSE)
    for (nm in names(where)) {
      pd <- tryCatch(parse_latex(where[[nm]], vars = vars, functions = functions, strict = strict),
                     error = function(e) e)
      if (inherits(pd, "error")) {
        obj$assumptions <- .assumption_df(items)
        return(.fail(obj, paste0("the definition of ", nm, " could not be parsed: ", conditionMessage(pd))))
      }
      def_trees[[nm]] <- pd$expr
      a <- .df_items(pd$assumptions)
      for (k in seq_along(a)) a[[k]]$text <- paste0("In the definition of `", nm, "`: ", a[[k]]$text)
      items <- c(items, a)
      defs <- rbind(defs, data.frame(symbol = nm, latex = where[[nm]], stringsAsFactors = FALSE))
    }
    tree <- .substitute_defs(tree, def_trees)
  }
  obj$assumptions <- .assumption_df(items)

  # ---- canonical form and lowering ----
  canonical <- normalize_tree(tree)
  lowered <- .lower(canonical)
  syms <- .free_symbols(canonical)
  if (is.null(wrt)) wrt <- syms
  wrt <- as.character(wrt)
  obj$wrt <- wrt
  obj$parameters <- setdiff(syms, wrt)
  obj$parsed <- list(tree = parsed$expr, canonical = canonical, latex = .tex(canonical),
                     definitions = defs, definition_trees = lapply(def_trees, normalize_tree),
                     lowered = lowered$expr, lowering = lowered$rules, symbols = syms)
  if (!length(wrt)) return(.fail(obj, "the formula has no variables to differentiate"))

  # ---- differentiation ----
  d <- tryCatch(.differentiate(lowered$expr, wrt, type), error = function(e) e)
  if (inherits(d, "error")) return(.fail(obj, paste0("differentiation failed: ", conditionMessage(d))))
  obj$derivative <- d
  obj$out_of_scope <- d$oos
  obj$notes <- d$notes
  obj$derivative$latex <- vapply(d$entries, .tex, "")

  # ---- certificate ----
  obj$certificate <- .certify(obj, as.integer(n_points), tol, seed, domain, functions)
  obj
}

.fail <- function(obj, message) {
  obj$error <- message
  obj$certificate <- list(pass = FALSE, failures = message)
  obj
}

.df_items <- function(df) {
  lapply(seq_len(nrow(df)), function(k) list(kind = df$kind[k], text = df$text[k]))
}

.hook_model <- function(hook) {
  m <- attr(hook, "model")
  if (is.null(m)) "a user-supplied hook" else m
}

.parse_entry <- function(latex, vars, functions, strict, disambiguate) {
  attempt <- function(tex) {
    tryCatch(parse_latex(tex, vars = vars, functions = functions, strict = strict),
             latex2grad_parse_error = function(e) e, latex2grad_ambiguity = function(e) e)
  }
  res <- attempt(latex)
  if (!inherits(res, "condition")) return(list(parsed = res, entry = list()))
  if (is.null(disambiguate)) return(list(error = res, entry = list()))
  model <- .hook_model(disambiguate)
  entry <- list()
  current <- latex
  for (round in 1:2) {
    out <- disambiguate(current, conditionMessage(res))
    if (!is.list(out) || !is.character(out$latex) || length(out$latex) != 1L) {
      stop("the disambiguate hook must return list(latex = <string>, decisions = <character>)", call. = FALSE)
    }
    current <- out$latex
    entry[[length(entry) + 1L]] <- list(kind = "llm", text = paste0(
      "[", model, "] the grammar reported: ", conditionMessage(res), "; the input was rewritten as `", current, "`"))
    for (dec in as.character(out$decisions)) {
      entry[[length(entry) + 1L]] <- list(kind = "llm", text = paste0("[", model, "] ", dec))
    }
    res <- attempt(current)
    if (!inherits(res, "condition")) return(list(parsed = res, entry = entry, rewritten = current))
  }
  list(error = res, entry = entry, rewritten = current)
}

.substitute_defs <- function(tree, defs) {
  for (round in seq_len(length(defs) + 1L)) {
    hit <- intersect(all.vars(tree), names(defs))
    if (!length(hit)) return(tree)
    tree <- .substitute_syms(tree, names(defs), unname(defs))
  }
  stop("`where` definitions are circular: ", paste(intersect(all.vars(tree), names(defs)), collapse = ", "),
       call. = FALSE)
}

#' @export
print.latex2grad <- function(x, ...) {
  what <- switch(x$type, grad = "gradient", hessian = "Hessian", jacobian = "Jacobian")
  cat("<latex2grad> ", what, if (length(x$wrt)) paste0(" w.r.t. ", paste(x$wrt, collapse = ", ")), "\n", sep = "")
  cat("  source:      ", .clip(x$source$latex), "\n", sep = "")
  if (!is.null(x$parsed)) cat("  parsed:      ", .clip(x$parsed$latex), "\n", sep = "")
  if (length(x$parameters)) cat("  parameters:  ", paste(x$parameters, collapse = ", "), "\n", sep = "")
  cat("  assumptions: ", nrow(x$assumptions), if (nrow(x$assumptions)) " (see $assumptions)", "\n", sep = "")
  if (!is.null(x$derivative)) {
    cat("  derivative:\n")
    for (k in seq_along(x$derivative$entries)) {
      cat("    ", x$derivative$labels[k], " = ", .clip(x$derivative$latex[k]), "\n", sep = "")
    }
    n_oos <- nrow(x$out_of_scope)
    cat("  out of scope: ", if (n_oos) paste0(n_oos, " term(s) (see $out_of_scope)") else "none", "\n", sep = "")
  }
  cat("  certificate: ", .conclusion(x), "\n", sep = "")
  invisible(x)
}

.clip <- function(s, n = 200L) {
  if (nchar(s) <= n) return(s)
  paste0(substr(s, 1L, n), " ... [", nchar(s), " characters; see l2g_report()]")
}

.conclusion <- function(x) {
  c0 <- x$certificate
  if (is.null(c0) || !isTRUE(c0$pass)) {
    return(paste0("NOT CERTIFIED: ", paste(c0$failures, collapse = "; "), "."))
  }
  nm <- c0$numeric
  out <- sprintf(paste0("CERTIFIED: every printed tree round-trips, and the derivative agrees with ",
                        "Richardson central differences at %d random points (max relative error %.2g <= %.2g)."),
                 nm$n_valid, nm$max_rel_error, nm$tol)
  if (c0$levels$n_numeric > 0L) {
    out <- paste0(out, sprintf(" %d of %d entries contain numeric-partial nodes for out-of-scope terms.",
                               c0$levels$n_numeric, length(x$derivative$entries)))
  }
  out
}

#' Evaluate a latex2grad derivative
#'
#' @param x A `latex2grad` object.
#' @param ... Ignored.
#' @return A function taking named values (as arguments or one named list or
#'   vector) and returning the derivative: a named vector for `"grad"`, a
#'   matrix for `"hessian"` and `"jacobian"`.
#' @export
as_function <- function(x, ...) UseMethod("as_function")

#' @rdname as_function
#' @export
as_function.latex2grad <- function(x, ...) {
  if (is.null(x$derivative)) stop("no derivative: ", x$error, call. = FALSE)
  entries <- x$derivative$entries
  function(...) {
    vals <- list(...)
    if (length(vals) == 1L && is.null(names(vals))) vals <- as.list(vals[[1L]])
    out <- vapply(entries, function(e) as.numeric(.eval_tree(e, vals)), 0)
    if (!is.null(dim(entries))) {
      dim(out) <- dim(entries)
      dimnames(out) <- dimnames(entries)
    } else {
      names(out) <- names(entries)
    }
    out
  }
}
