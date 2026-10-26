## report.R
## Rendering of a latex2grad_report: console text, Markdown, and a LaTeX
## document with the sections Source / Parsed form / Assumptions /
## Derivative / Verification / Out of scope / Conclusion.

#' Render a latex2grad report
#'
#' @param x A `latex2grad_report`.
#' @param format `"latex"` (a complete document), `"markdown"`, or `"text"`.
#' @param ... Unused.
#' @return A single string.
#' @export
format.latex2grad_report <- function(x, format = c("text", "latex", "markdown"), ...) {
  format <- match.arg(format)
  switch(format,
         text = .l2g_render_text(x),
         latex = .l2g_render_latex(x),
         markdown = .l2g_render_markdown(x))
}

#' @export
print.latex2grad_report <- function(x, ...) {
  cat(.l2g_render_text(x), "\n", sep = "")
  invisible(x)
}

#' Write a report to disk
#'
#' @param x A `latex2grad_report`.
#' @param file Output path. The format follows the extension: `.tex` writes
#'   a LaTeX document, `.md` Markdown, anything else plain text.
#' @return `file`, invisibly.
#' @export
write_report <- function(x, file) {
  ext <- tolower(tools::file_ext(file))
  fmt <- switch(ext, tex = "latex", md = "markdown", "text")
  writeLines(format(x, format = fmt), file, useBytes = TRUE)
  invisible(file)
}

# ---- shared pieces --------------------------------------------------------

# Identical decisions made at several places are listed once, with a count.
.l2g_group_assumptions <- function(as) {
  out <- list()
  for (a in as) {
    key <- paste(a$text, a$source, a$chosen, sep = "\r")
    hit <- FALSE
    for (j in seq_along(out)) {
      if (identical(out[[j]]$key, key)) {
        out[[j]]$count <- out[[j]]$count + 1L
        out[[j]]$ids <- c(out[[j]]$ids, a$id)
        hit <- TRUE
        break
      }
    }
    if (!hit) out[[length(out) + 1L]] <- c(a, list(key = key, count = 1L, ids = a$id))
  }
  out
}

.l2g_assumption_line <- function(a) {
  alt <- if (length(a$alternatives)) paste0("; alternative: ", paste(a$alternatives, collapse = " / ")) else ""
  ids <- a$ids[!is.na(a$ids)]
  id <- if (length(ids)) paste0(" [", paste(ids, collapse = ", "), "]") else ""
  times <- if (isTRUE(a$count > 1L)) paste0(" (x", a$count, ")") else ""
  paste0(a$text, times, " (", a$source, alt, ")", id)
}

.l2g_boundary_slot <- function(b) {
  if (is.na(b$slot)) return("the whole node")
  if (b$nargs == 1L) return("its argument")
  paste0("argument ", b$slot, " of ", b$nargs)
}

.l2g_rule_tex <- function(b) {
  if (is.null(b$template)) return(NULL)
  ph <- .l2g_placeholders(b$nargs)
  lhs <- paste0("\\frac{\\partial}{\\partial ", ph[[b$slot]], "} ", tex_print(b$template))
  if (b$kind == "closed form") paste0(lhs, " = ", tex_print(b$rule)) else lhs
}

.l2g_fmt <- function(x) formatC(x, digits = 3, format = "g")

# ---- text -----------------------------------------------------------------

.l2g_render_text <- function(x) {
  num <- x$verification$numeric
  L <- c(
    paste0("latex2grad report (", x$op, ")"),
    paste0("  Source       ", x$source$latex),
    paste0("  Parsed       ", deparse1(x$parsed$expr)),
    paste0("  Canonical    ", x$parsed$tex),
    paste0("  wrt          ", paste(x$wrt, collapse = ", ")))
  if (length(x$assumptions)) {
    L <- c(L, "  Assumptions")
    L <- c(L, paste0("    - ", vapply(.l2g_group_assumptions(x$assumptions), .l2g_assumption_line, character(1))))
  }
  L <- c(L, "  Derivative")
  for (e in x$derivatives) {
    lab <- if (x$op == "hessian") paste0("d2/d", e$row, "d", e$col)
           else if (x$op == "jacobian") paste0("d[", e$row, "]/d", e$col)
           else paste0("d/d", e$var)
    L <- c(L, paste0("    ", lab, " = ", deparse1(e$expr)))
  }
  rt_n <- sum(vapply(x$derivatives, function(e) isTRUE(e$round_trip$ok), logical(1)))
  L <- c(L, "  Verification",
         paste0("    round-trip   formula ", if (isTRUE(x$verification$round_trip$ok)) "ok" else "FAILED",
                "; derivatives ", rt_n, "/", length(x$derivatives)),
         paste0("    numeric      ", num$n_points, " point(s) (", num$n_attempts, " drawn), max rel. error ",
                .l2g_fmt(num$max_rel_error), ", tol ", .l2g_fmt(num$tol_effective), " -> ",
                if (isTRUE(num$pass)) "pass" else "FAIL"))
  if (length(x$boundaries)) {
    L <- c(L, "  Out of scope")
    for (b in x$boundaries) {
      how <- if (b$kind == "closed form") paste0("closed form: ", deparse1(b$rule)) else "numeric (Richardson)"
      L <- c(L, paste0("    - ", b$fun, ", ", .l2g_boundary_slot(b), ": ", b$reason, "; ", how))
    }
  }
  paste(c(L, paste0("  Conclusion   ", x$conclusion)), collapse = "\n")
}

# ---- markdown ------------------------------------------------------------

.l2g_render_markdown <- function(x) {
  num <- x$verification$numeric
  rt <- x$verification$round_trip
  md <- c("# latex2grad report", "",
          "## Source", "", "```latex", x$source$latex, "```")
  if (!is.null(x$source$image)) md <- c(md, "", paste0("![formula](", x$source$image, ")"))
  if (!identical(x$source$used_latex, x$source$latex)) {
    md <- c(md, "", "Rewritten for the grammar:", "", "```latex", x$source$used_latex, "```")
  }
  md <- c(md, "", "## Parsed form", "", "```r", deparse1(x$parsed$expr), "```", "",
          .l2g_markdown_block(list(list(lhs = .l2g_lhs_tex(x), expr = x$parsed$expr))),
          "", "## Assumptions", "")
  md <- c(md, if (length(x$assumptions))
    paste0("- ", vapply(.l2g_group_assumptions(x$assumptions), .l2g_assumption_line, character(1)))
    else "- None: the grammar read the input without choices.")
  md <- c(md, "", paste0("## Derivative (", x$op, " with respect to ", paste(x$wrt, collapse = ", "), ")"), "")
  d <- .l2g_derivative_eqs(x)
  md <- c(md, .l2g_markdown_block(d$eqs), "")
  if (length(d$defs)) md <- c(md, "where", "", .l2g_markdown_block(d$defs), "")
  rt_n <- sum(vapply(x$derivatives, function(e) isTRUE(e$round_trip$ok), logical(1)))
  md <- c(md, "## Verification", "",
          "| Check | Result |", "|---|---|",
          paste0("| Round-trip, formula | ", if (isTRUE(rt$ok)) "pass" else "**FAIL**", " |"),
          paste0("| Round-trip, derivatives | ", rt_n, " / ", length(x$derivatives), " |"),
          paste0("| Test points | ", num$n_points, " valid of ", num$n_attempts, " drawn |"),
          paste0("| Max relative error | ", .l2g_fmt(num$max_rel_error), " (tolerance ",
                 .l2g_fmt(num$tol_effective), if (num$widened) ", widened for numeric nodes" else "", ") |"),
          paste0("| Numeric check | ", if (isTRUE(num$pass)) "pass" else "**FAIL**", " |"), "")
  md <- c(md, "## Out of scope", "")
  if (length(x$boundaries)) {
    for (b in x$boundaries) {
      rule <- .l2g_rule_tex(b)
      how <- if (b$kind == "closed form") "closed-form rule" else "evaluated numerically (Richardson extrapolation)"
      md <- c(md, paste0("- `", b$fun, "`, ", .l2g_boundary_slot(b), ": ", b$reason, ". ", how,
                         if (!is.null(rule)) paste0(": $", rule, "$") else "", "."))
    }
  } else {
    md <- c(md, "- Nothing: DefDiff differentiated every term.")
  }
  md <- c(md, "", "## Conclusion", "", x$conclusion, "",
          paste0("<sub>latex2grad ", x$versions$latex2grad, ", DefDiff ", x$versions$DefDiff,
                 ", ", x$versions$R, "</sub>"))
  paste(md, collapse = "\n")
}

# ---- LaTeX ----------------------------------------------------------------

# Escape text for LaTeX; `code` spans become \texttt{}.
.l2g_tex_text <- function(s) {
  esc <- function(t) {
    t <- gsub("\\\\", "\001", t)
    t <- gsub("([{}_$%#&])", "\\\\\\1", t)
    t <- gsub("\\^", "\\\\^{}", t)
    t <- gsub("~", "\\\\~{}", t)
    gsub("\001", "\\\\textbackslash{}", t)
  }
  parts <- strsplit(s, "`", fixed = TRUE)[[1L]]
  if (!length(parts)) return("")
  out <- vapply(seq_along(parts), function(k) {
    if (k %% 2 == 0) paste0("\\texttt{", esc(parts[[k]]), "}") else esc(parts[[k]])
  }, character(1))
  paste(out, collapse = "")
}

# Left-hand side for the parsed formula: f(x, y; p).
.l2g_lhs_tex <- function(x) {
  sig <- x$parsed$signature
  if (is.null(sig$name)) return(.l2g_symbol_tex(x$fname))
  v <- vapply(sig$vars, .l2g_symbol_tex, character(1))
  p <- vapply(sig$params, .l2g_symbol_tex, character(1))
  args <- paste(v, collapse = ", ")
  if (length(p)) args <- paste0(args, "; ", paste(p, collapse = ", "))
  if (!length(v) && !length(p)) return(.l2g_symbol_tex(sig$name))
  paste0(.l2g_symbol_tex(sig$name), "\\left(", args, "\\right)")
}

.l2g_render_latex <- function(x) {
  num <- x$verification$numeric
  rt <- x$verification$round_trip
  T <- c("\\documentclass[11pt]{article}",
         "\\usepackage[margin=2.2cm]{geometry}",
         "\\usepackage{amsmath,amssymb}",
         "\\usepackage{graphicx}",
         "\\usepackage{booktabs}",
         "\\setlength{\\parindent}{0pt}",
         "\\setlength{\\parskip}{0.5em}",
         "\\begin{document}",
         "{\\Large\\bfseries latex2grad report}\\par",
         paste0("\\small ", .l2g_tex_text(paste0("latex2grad ", x$versions$latex2grad, ", DefDiff ",
                                                 x$versions$DefDiff, ", ", x$versions$R)), "\\normalsize"),
         "", "\\section*{Source}", "\\begin{verbatim}", strwrap(x$source$latex, 95), "\\end{verbatim}")
  if (!is.null(x$source$image)) {
    T <- c(T, paste0("\\includegraphics[width=0.6\\linewidth]{", x$source$image, "}"))
  }
  if (!identical(x$source$used_latex, x$source$latex)) {
    T <- c(T, "Rewritten for the grammar:", "\\begin{verbatim}", x$source$used_latex, "\\end{verbatim}")
  }
  T <- c(T, "\\section*{Parsed form}", "R expression:", "\\begin{verbatim}",
         strwrap(deparse1(x$parsed$expr), 90, exdent = 2), "\\end{verbatim}",
         "Canonical form, printed back:",
         .l2g_latex_block(list(list(lhs = .l2g_lhs_tex(x), expr = x$parsed$expr))),
         "\\section*{Assumptions}")
  if (length(x$assumptions)) {
    T <- c(T, "\\begin{itemize}",
           paste0("\\item ", vapply(.l2g_group_assumptions(x$assumptions),
                                    function(a) .l2g_tex_text(.l2g_assumption_line(a)),
                                    character(1))),
           "\\end{itemize}")
  } else {
    T <- c(T, "None: the grammar read the input without choices.")
  }
  T <- c(T, paste0("\\section*{Derivative}"),
         .l2g_tex_text(paste0(x$op, " with respect to ", paste(x$wrt, collapse = ", "), ".")))
  d <- .l2g_derivative_eqs(x)
  T <- c(T, .l2g_latex_block(d$eqs))
  if (length(d$defs)) {
    T <- c(T, "where", .l2g_latex_block(d$defs),
           .l2g_tex_text("(Abbreviations are for display; the certificate covers the full expressions.)"))
  }
  rt_n <- sum(vapply(x$derivatives, function(e) isTRUE(e$round_trip$ok), logical(1)))
  ok <- function(b) if (isTRUE(b)) "pass" else "\\textbf{FAIL}"
  T <- c(T, "\\section*{Verification}",
         "\\begin{tabular}{ll}", "\\toprule", "Check & Result \\\\", "\\midrule",
         paste0("Round-trip \\texttt{parse(print(e)) == e}, formula & ", ok(rt$ok), " \\\\"),
         paste0("Round-trip, derivatives & ", rt_n, " / ", length(x$derivatives), " \\\\"),
         paste0("Test points & ", num$n_points, " valid of ", num$n_attempts, " drawn \\\\"),
         paste0("Max relative error & ", .l2g_fmt(num$max_rel_error), " (tolerance ",
                .l2g_fmt(num$tol_effective), if (num$widened) ", widened" else "", ") \\\\"),
         paste0("Numeric check & ", ok(num$pass), " \\\\"),
         "\\bottomrule", "\\end{tabular}",
         "",
         paste0("Relative error is $|s - n| / \\max(1, |n|)$, with $s$ the symbolic and $n$ the numeric ",
                "derivative (a Richardson-extrapolated central difference)."),
         "\\section*{Out of scope}")
  if (length(x$boundaries)) {
    T <- c(T, "\\begin{itemize}")
    for (b in x$boundaries) {
      rule <- .l2g_rule_tex(b)
      how <- if (b$kind == "closed form") "Closed-form rule" else "Evaluated numerically (Richardson extrapolation)"
      T <- c(T, paste0("\\item ", .l2g_tex_text(paste0("`", b$fun, "`, ", .l2g_boundary_slot(b), ": ",
                                                       b$reason, ". ", how)),
                       if (!is.null(rule)) paste0(": $", rule, "$") else "", "."))
    }
    T <- c(T, "\\end{itemize}")
  } else {
    T <- c(T, "Nothing: DefDiff differentiated every term.")
  }
  T <- c(T, "\\section*{Conclusion}", .l2g_tex_text(x$conclusion), "\\end{document}")
  paste(T, collapse = "\n")
}
