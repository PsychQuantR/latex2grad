## latex2grad.R
## The pipeline: entry (optional OCR / LLM disambiguation) -> parse ->
## normalize -> DefDiff -> verify -> report.

#' Differentiate a LaTeX formula and certify the result
#'
#' Runs the full pipeline on a formula written in LaTeX (or captured as an
#' image): parse, normalize to canonical form, differentiate through DefDiff,
#' verify, and assemble a report. Everything after the entry is
#' deterministic. A failed check still produces a report, with the failure
#' stated in its conclusion.
#'
#' @param latex A single string of LaTeX math. An equation `f(x, y) = ...`
#'   names the function and its variables; with a semicolon,
#'   `c(u; \rho, \nu) = ...`, the variables after it are parameters.
#' @param wrt Variables to differentiate with respect to (R names such as
#'   `"nu"`, `"u_1"`, or LaTeX spellings such as `"\\nu"`). Defaults to the
#'   signature's variables, else every free symbol in order of appearance.
#' @param op `"grad"` (default for a scalar formula), `"hessian"`, or
#'   `"jacobian"` (default for a vector `\begin{pmatrix}...\end{pmatrix}`).
#' @param domain Named list of sampling ranges for the numeric check, e.g.
#'   `list(nu = c(2, 30), rho = c(-0.9, 0.9))`. Unlisted variables are drawn
#'   from `[-2, 2]`; points where anything is undefined are redrawn.
#' @param n_points Number of random test points.
#' @param tol Relative tolerance of the numeric check, as
#'   `|symbolic - numeric| / max(1, |numeric|)`. Widened to `1e-4` when an
#'   integral or numeric derivative node is involved.
#' @param seed Seed for the test points (the global RNG state is restored).
#' @param choices Named list overriding the grammar's choice at specific
#'   ambiguities, keyed by ambiguity id (see the `assumptions` of a report).
#' @param llm Optional entry assistant, e.g. [claude_entry()]: a list with
#'   any of `resolve(latex, ambiguities)`, `rewrite(latex, error)`,
#'   `ocr(image)` and a `name`. Used only at the entry; every decision it
#'   makes is written into the report's assumptions.
#' @param image Optional path to an image of the formula (requires
#'   `llm$ocr`); `latex` may then be omitted.
#' @return An object of class `latex2grad_report`. Print it, or render it
#'   with [format()][format.latex2grad_report] / [write_report()].
#' @export
#' @examples
#' \dontrun{
#' r <- latex2grad("f(x, y) = x^2 \\sin y")
#' r
#' write_report(r, "report.tex")
#'
#' latex2grad("\\ln\\Gamma\\left(\\frac{\\nu + 1}{2}\\right) - \\ln\\Gamma(\\nu / 2)",
#'            wrt = "nu", domain = list(nu = c(1, 30)))
#' }
latex2grad <- function(latex = NULL, wrt = NULL,
                       op = c("grad", "hessian", "jacobian"),
                       domain = list(), n_points = 20L, tol = 1e-6, seed = 1L,
                       choices = list(), llm = NULL, image = NULL) {
  op_given <- !missing(op)
  op <- match.arg(op)
  assumptions <- list()
  add <- function(source, text, id = NA_character_, chosen = NA_character_,
                  alternatives = character(0)) {
    assumptions[[length(assumptions) + 1L]] <<- list(
      id = id, source = source, text = text, chosen = chosen,
      alternatives = alternatives)
  }

  # ---- entry ---------------------------------------------------------------
  if (!is.null(image)) {
    if (is.null(llm) || !is.function(llm$ocr)) {
      .l2g_stop("entry", "image input needs an `llm` entry with an `ocr` function (see claude_entry()).")
    }
    if (!file.exists(image)) .l2g_stop("entry", paste0("image not found: ", image))
    latex <- llm$ocr(image)
    add(.l2g_llm_name(llm), paste0("LaTeX transcribed from the image `", basename(image), "`."))
  }
  if (is.null(latex) || !is.character(latex) || length(latex) != 1L) {
    .l2g_stop("entry", "`latex` must be a single string (or pass `image` with an OCR entry).")
  }
  source_latex <- latex
  entry <- .l2g_parse_entry(latex, choices, llm)
  latex <- entry$latex
  if (!identical(latex, source_latex)) {
    add(.l2g_llm_name(llm), paste0("Input rewritten into the supported grammar after the parse error \"",
                                   entry$error, "\": `", latex, "`."))
  }
  parsed <- entry$parsed
  for (a in parsed$ambiguities) {
    src <- if (a$source == "grammar default") "grammar default"
           else if (a$id %in% names(choices)) "user choice"
           else .l2g_llm_name(llm)
    add(src, a$text, a$id, a$chosen, setdiff(a$options, a$chosen))
  }
  for (nt in parsed$notes) add("grammar", nt$text)

  # ---- normalize -----------------------------------------------------------
  expr <- tex_normalize(parsed$expr)
  is_vec <- .l2g_is_op(expr, "c")
  if (is_vec && op != "jacobian") {
    if (op_given) .l2g_stop("entry", "a vector-valued formula needs op = \"jacobian\".")
    op <- "jacobian"
    add("pipeline", "Vector-valued formula: computing the Jacobian.")
  }
  comps <- if (is_vec) as.list(expr)[-1L] else list(expr)

  sig <- parsed$signature
  fname <- if (!is.null(sig$name)) sig$name else "f"
  free <- setdiff(unique(unlist(lapply(comps, .l2g_free_vars))), "pi")
  if (is.null(wrt)) {
    if (!is.null(sig$name) && length(sig$vars)) {
      wrt <- sig$vars
      add("pipeline", paste0("Differentiation variables not given; using the signature's variables: ",
                             paste(wrt, collapse = ", "), "."))
    } else {
      wrt <- free
      add("pipeline", paste0("Differentiation variables not given; using every free symbol in order ",
                             "of appearance: ", paste(wrt, collapse = ", "), "."))
    }
  } else {
    wrt <- vapply(wrt, .l2g_r_name, character(1), USE.NAMES = FALSE)
  }
  if (!length(wrt)) .l2g_stop("entry", "the formula has no free variables to differentiate.")
  for (v in setdiff(wrt, free)) {
    add("pipeline", paste0("`", v, "` does not occur in the formula; its derivative is 0."))
  }
  if (length(domain)) names(domain) <- vapply(names(domain), .l2g_r_name, character(1))

  # ---- differentiate -------------------------------------------------------
  entries <- list()
  boundaries <- list()
  fn_tex <- .l2g_symbol_tex(fname)
  new_entry <- function(label, expr, ref_f, var, row, col) {
    list(label = label, expr = expr, ref_f = ref_f, var = var, row = row, col = col)
  }
  if (op == "grad") {
    g <- l2g_differentiate(expr, wrt)
    boundaries <- c(boundaries, list(g$boundaries))
    for (v in wrt) {
      entries[[length(entries) + 1L]] <- new_entry(
        paste0("\\frac{\\partial ", fn_tex, "}{\\partial ", .l2g_symbol_tex(v), "}"),
        g$derivatives[[v]], expr, v, v, NA)
    }
  } else if (op == "hessian") {
    g <- l2g_differentiate(expr, wrt)
    boundaries <- c(boundaries, list(g$boundaries))
    for (vi in wrt) {
      h <- l2g_differentiate(g$derivatives[[vi]], wrt)
      boundaries <- c(boundaries, list(h$boundaries))
      for (vj in wrt) {
        den <- if (vi == vj) paste0("\\partial ", .l2g_symbol_tex(vi), "^{2}")
               else paste0("\\partial ", .l2g_symbol_tex(vi), " \\, \\partial ", .l2g_symbol_tex(vj))
        entries[[length(entries) + 1L]] <- new_entry(
          paste0("\\frac{\\partial^{2} ", fn_tex, "}{", den, "}"),
          h$derivatives[[vj]], g$derivatives[[vi]], vj, vi, vj)
      }
    }
  } else {
    for (k in seq_along(comps)) {
      jk <- l2g_differentiate(comps[[k]], wrt)
      boundaries <- c(boundaries, list(jk$boundaries))
      sub <- if (length(comps) > 1L) paste0("_{", k, "}") else ""
      for (v in wrt) {
        entries[[length(entries) + 1L]] <- new_entry(
          paste0("\\frac{\\partial ", fn_tex, sub, "}{\\partial ", .l2g_symbol_tex(v), "}"),
          jk$derivatives[[v]], comps[[k]], v, k, v)
      }
    }
  }
  boundaries <- .l2g_merge_boundaries(boundaries)

  # ---- verify --------------------------------------------------------------
  rt_formula <- l2g_round_trip(expr)
  for (k in seq_along(entries)) {
    entries[[k]]$round_trip <- l2g_round_trip(entries[[k]]$expr)
    entries[[k]]$tex <- entries[[k]]$round_trip$tex
    entries[[k]]$numeric_node <- "numd" %in% all.names(entries[[k]]$expr)
  }
  vars <- setdiff(unique(c(free, wrt)), "pi")
  numeric <- .l2g_verify_numeric(vars, entries, domain, as.integer(n_points), tol, seed)
  rt_ok <- isTRUE(rt_formula$ok) && all(vapply(entries, function(e) isTRUE(e$round_trip$ok), logical(1)))
  pass <- rt_ok && isTRUE(numeric$pass)

  report <- list(
    source = list(latex = source_latex, used_latex = latex, image = image),
    parsed = list(expr = expr, tex = rt_formula$tex, signature = sig),
    assumptions = assumptions,
    op = op, wrt = wrt, fname = fname,
    derivatives = entries,
    boundaries = boundaries,
    verification = list(round_trip = rt_formula, numeric = numeric),
    pass = pass,
    versions = .l2g_versions()
  )
  report$conclusion <- .l2g_conclusion(report)
  structure(report, class = "latex2grad_report")
}

.l2g_llm_name <- function(llm) {
  if (!is.null(llm$name)) paste0("LLM (", llm$name, ")") else "LLM"
}

# Parse with the optional LLM entry: rewrite on a parse error, then let the
# resolver revisit the grammar's choices (a re-parse can expose new choices,
# so this iterates a few times).
.l2g_parse_entry <- function(latex, choices, llm) {
  err <- NULL
  parsed <- tryCatch(tex_parse(latex, choices), latex2grad_parse_error = function(e) e)
  if (inherits(parsed, "error")) {
    if (is.null(llm) || !is.function(llm$rewrite)) stop(parsed)
    err <- conditionMessage(parsed)
    latex <- llm$rewrite(latex, err)
    parsed <- tex_parse(latex, choices)
  }
  if (!is.null(llm) && is.function(llm$resolve) && length(parsed$ambiguities)) {
    seen <- character(0)
    llm_choices <- list()
    for (pass in 1:3) {
      open <- Filter(function(a) !(a$id %in% c(seen, names(choices))) && length(a$options) > 1L,
                     parsed$ambiguities)
      if (!length(open)) break
      got <- llm$resolve(latex, open)
      seen <- c(seen, vapply(open, `[[`, "", "id"))
      llm_choices <- utils::modifyList(llm_choices, as.list(got))
      parsed <- tex_parse(latex, utils::modifyList(llm_choices, choices))
    }
  }
  list(parsed = parsed, latex = latex, error = err)
}

.l2g_merge_boundaries <- function(lists) {
  out <- list()
  for (l in lists) for (b in l) {
    hit <- FALSE
    for (j in seq_along(out)) {
      if (identical(out[[j]]$key, b$key)) {
        out[[j]]$count <- out[[j]]$count + b$count
        hit <- TRUE
        break
      }
    }
    if (!hit) out[[length(out) + 1L]] <- b
  }
  out
}

.l2g_versions <- function() {
  v <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "dev")
  list(latex2grad = v("latex2grad"), DefDiff = v("DefDiff"), R = R.version$version.string)
}

.l2g_conclusion <- function(r) {
  rt <- r$verification$round_trip
  num <- r$verification$numeric
  n_d <- length(r$derivatives)
  rt_d <- sum(vapply(r$derivatives, function(e) isTRUE(e$round_trip$ok), logical(1)))
  parts <- character(0)
  if (!isTRUE(rt$ok)) parts <- c(parts, "the parsed formula does not round-trip")
  if (rt_d < n_d) parts <- c(parts, paste0(n_d - rt_d, " of ", n_d, " derivative(s) do not round-trip"))
  if (num$n_points == 0L) {
    parts <- c(parts, "no test point where the formula and its derivatives are all defined was found (set `domain`)")
  } else if (!isTRUE(num$pass)) {
    parts <- c(parts, sprintf("the numeric check failed (max relative error %.3g > %.3g, worst at %s)",
                              num$max_rel_error, num$tol_effective, num$worst$label))
  }
  head <- if (r$pass) "Verified." else paste0("NOT verified: ", paste(parts, collapse = "; "), ".")
  extra <- character(0)
  if (r$pass) {
    extra <- c(extra, sprintf("All %d printed expressions round-trip; %d derivative(s) agree with Richardson-extrapolated central differences at %d random point(s) (max relative error %.2g, tolerance %.2g).",
                              n_d + 1L, n_d, num$n_points, num$max_rel_error, num$tol_effective))
    if (num$n_points < num$n_requested) {
      extra <- c(extra, sprintf("Only %d of %d requested points were valid.", num$n_points, num$n_requested))
    }
  }
  nb <- length(r$boundaries)
  if (nb) {
    nn <- sum(vapply(r$boundaries, function(b) b$kind == "numeric", logical(1)))
    extra <- c(extra, sprintf("%d term(s) lie outside DefDiff's language (%d handled by closed-form rules, %d numerically); see Out of scope.",
                              nb, nb - nn, nn))
  }
  nn_d <- sum(vapply(r$derivatives, `[[`, logical(1), "numeric_node"))
  if (nn_d) {
    extra <- c(extra, sprintf("%d derivative(s) contain numeric derivative nodes; the numeric check covers the assembly around them, not the nodes themselves.", nn_d))
  }
  paste(c(head, extra), collapse = " ")
}
