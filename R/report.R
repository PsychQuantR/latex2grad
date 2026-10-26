## report.R
## LaTeX report with the README layout: Source, Parsed form, Assumptions,
## Derivative, Verification, Out of scope, Conclusion. Only amsmath, amssymb,
## graphicx and booktabs are used.

#' Render a latex2grad result as a LaTeX report
#'
#' @param x A `latex2grad` object.
#' @param file Optional path; when given the report is written there.
#' @return The report as a single string (invisibly when `file` is given).
#' @export
#' @examples
#' \dontrun{
#' l2g_report(latex2grad("x^2 y", wrt = c("x", "y")), file = "report.tex")
#' }
l2g_report <- function(x, file = NULL) {
  stopifnot(inherits(x, "latex2grad"))
  out <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage{amsmath,amssymb,graphicx,booktabs}",
    "\\allowdisplaybreaks",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{0.5em}",
    "\\begin{document}",
    "{\\Large\\bfseries latex2grad report}\\par",
    .rep_summary(x),
    .rep_source(x),
    .rep_parsed(x),
    .rep_assumptions(x),
    .rep_derivative(x),
    .rep_verification(x),
    .rep_oos(x),
    "\\section*{Conclusion}",
    .rep_conclusion(x),
    "\\end{document}"
  )
  txt <- paste(out, collapse = "\n")
  if (!is.null(file)) {
    writeLines(txt, file, useBytes = TRUE)
    return(invisible(txt))
  }
  txt
}

# Escape plain text; `code` spans in backticks become \texttt{}.
.tex_text <- function(s) {
  esc <- function(t) {
    t <- gsub("\\", "\001", t, fixed = TRUE)
    for (ch in c("{", "}", "$", "&", "%", "#", "_")) t <- gsub(ch, paste0("\\", ch), t, fixed = TRUE)
    t <- gsub("^", "\\^{}", t, fixed = TRUE)
    t <- gsub("~", "\\~{}", t, fixed = TRUE)
    t <- gsub("<=", "\002", t, fixed = TRUE)
    t <- gsub("<", "$<$", t, fixed = TRUE)
    t <- gsub(">", "$>$", t, fixed = TRUE)
    t <- gsub("\002", "$\\le$", t, fixed = TRUE)
    .unicode_to_tex(gsub("\001", "\\textbackslash{}", t, fixed = TRUE))
  }
  vapply(s, function(one) {
    parts <- strsplit(one, "`", fixed = TRUE)[[1L]]
    if (!length(parts)) return("")
    out <- character(length(parts))
    for (k in seq_along(parts)) {
      out[k] <- if (k %% 2L == 0L) paste0("\\texttt{", esc(parts[k]), "}") else esc(parts[k])
    }
    paste(out, collapse = "")
  }, "", USE.NAMES = FALSE)
}

# Non-ASCII text (LLM decisions, OCR notes) would stop pdflatex: Greek letters
# and common math symbols become math commands, anything else "[U+XXXX]".
.greek_lower_cp <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
                     "iota", "kappa", "lambda", "mu", "nu", "xi", "o", "pi", "rho",
                     "varsigma", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega")
.greek_upper_cp <- c("A", "B", "Gamma", "Delta", "E", "Z", "H", "Theta", "I", "K", "Lambda",
                     "M", "N", "Xi", "O", "Pi", "P", NA, "Sigma", "T", "Upsilon", "Phi", "X",
                     "Psi", "Omega")
.unicode_math <- c("2264" = "\\le", "2265" = "\\ge", "2260" = "\\neq", "2202" = "\\partial",
                   "2211" = "\\sum", "220F" = "\\prod", "221E" = "\\infty", "00D7" = "\\times",
                   "00B7" = "\\cdot", "2212" = "-", "2192" = "\\to", "2032" = "'", "221A" = "\\surd",
                   "00B1" = "\\pm", "2248" = "\\approx", "2208" = "\\in", "00B0" = "^\\circ")
.unicode_text <- c("2013" = "--", "2014" = "---", "2018" = "`", "2019" = "'", "201C" = "``",
                   "201D" = "''", "00A0" = "~")

.unicode_char_tex <- function(cp) {
  hex <- sprintf("%04X", cp)
  if (cp >= 0x3B1 && cp <= 0x3C9) {
    g <- .greek_lower_cp[cp - 0x3B0]
    return(if (nchar(g) == 1L) g else paste0("$\\", g, "$"))
  }
  if (cp >= 0x391 && cp <= 0x3A9 && !is.na(.greek_upper_cp[cp - 0x390])) {
    g <- .greek_upper_cp[cp - 0x390]
    return(if (nchar(g) == 1L) g else paste0("$\\", g, "$"))
  }
  if (hex %in% names(.unicode_math)) return(paste0("$", .unicode_math[[hex]], "$"))
  if (hex %in% names(.unicode_text)) return(.unicode_text[[hex]])
  paste0("[U+", hex, "]")
}

.unicode_to_tex <- function(t, verbatim = FALSE) {
  # Input from hooks is UTF-8 even when the session locale is not.
  if (!validUTF8(t)) t <- iconv(t, "latin1", "UTF-8")
  Encoding(t) <- "UTF-8"
  cps <- utf8ToInt(t)
  if (all(cps < 128L)) return(t)
  out <- vapply(cps, function(cp) {
    if (cp < 128L) return(intToUtf8(cp))
    if (verbatim) sprintf("[U+%04X]", cp) else .unicode_char_tex(cp)
  }, "")
  paste(out, collapse = "")
}

# Verbatim block in \small type, hard-wrapped at `width` characters
# (continuation lines start with "> ").
.verbatim <- function(s, width = 64L) {
  s <- gsub("\\end{verbatim}", "\\end{verbatim }", s, fixed = TRUE)
  s <- vapply(s, .unicode_to_tex, "", verbatim = TRUE, USE.NAMES = FALSE)
  lines <- unlist(strsplit(s, "\n", fixed = TRUE))
  wrapped <- unlist(lapply(lines, function(l) {
    if (nchar(l) <= width) return(l)
    starts <- seq(1L, nchar(l), by = width - 2L)
    chunks <- substring(l, starts, pmin(starts + width - 3L, nchar(l)))
    c(chunks[1L], paste0("> ", chunks[-1L]))
  }))
  c("{\\small", "\\begin{verbatim}", wrapped, "\\end{verbatim}", "}")
}

.math_list <- function(names) paste0("$", vapply(names, .symbol_to_latex, ""), "$", collapse = ", ")

.rep_summary <- function(x) {
  what <- switch(x$type, grad = "Gradient", hessian = "Hessian", jacobian = "Jacobian")
  wrt <- if (length(x$wrt)) paste0(" with respect to ", .math_list(x$wrt)) else ""
  paste0(what, wrt, ". Generated by latex2grad ", .pkg_version(), " using DefDiff ",
         .pkg_version("DefDiff"), ".")
}

.pkg_version <- function(pkg = "latex2grad") {
  v <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "(development)")
  v
}

.rep_source <- function(x) {
  out <- c("\\section*{Source}", "Input LaTeX, verbatim:", .verbatim(x$source$latex))
  if (!is.null(x$source$image)) {
    img <- x$source$image
    ext <- tolower(tools::file_ext(img))
    if (ext %in% c("png", "jpg", "jpeg", "pdf") && file.exists(img)) {
      path <- normalizePath(img, winslash = "/")
      out <- c(out, paste0("Transcribed from the image below by ", .tex_text(x$source$ocr_model), "."),
               "\\begin{center}",
               paste0("\\includegraphics[width=0.6\\linewidth]{\\detokenize{", path, "}}"),
               "\\end{center}")
    } else {
      out <- c(out, paste0("Transcribed from the image ", .tex_text(paste0("`", basename(img), "`")),
                           " by ", .tex_text(x$source$ocr_model),
                           " (not embedded: pdflatex includes only PNG, JPEG and PDF images)."))
    }
  }
  if (!is.null(x$source$rewritten)) {
    out <- c(out, "Rewritten by the disambiguation hook (see Assumptions):", .verbatim(x$source$rewritten))
  }
  if (!is.null(x$parsed)) {
    rendered <- if (is.null(x$source$rewritten)) x$source$latex else x$source$rewritten
    out <- c(out, "Rendered:", "\\[", rendered, "\\]")
  }
  out
}

.rep_parsed <- function(x) {
  out <- "\\section*{Parsed form}"
  if (is.null(x$parsed)) return(c(out, "Not available: the input could not be parsed."))
  p <- x$parsed
  out <- c(out, "R expression (canonical form):", .verbatim(deparse(p$canonical, width.cutoff = 70L)),
           "Canonical tree printed back to LaTeX:", "\\begin{align*}",
           .aligned_lines("f", p$canonical), "\\end{align*}")
  if (nrow(p$definitions)) {
    out <- c(out, "Definitions substituted (from \\texttt{where}):", "\\begin{itemize}")
    for (k in seq_len(nrow(p$definitions))) {
      out <- c(out, paste0("\\item $", .symbol_to_latex(p$definitions$symbol[k]), " := ",
                           p$definitions$latex[k], "$"))
    }
    out <- c(out, "\\end{itemize}")
  }
  out <- c(out, paste0("Variables: ", if (length(x$wrt)) .math_list(x$wrt) else "none", ". ",
                       "Parameters (held constant): ",
                       if (length(x$parameters)) .math_list(x$parameters) else "none", "."))
  if (length(p$lowering)) {
    out <- c(out, "Lowered for DefDiff by exact identities:", "\\begin{itemize}",
             paste0("\\item ", .tex_text(paste0("`", p$lowering, "`"))), "\\end{itemize}")
  } else {
    out <- c(out, "No lowering was needed: every function is a DefDiff generator or out of scope.")
  }
  out
}

.rep_assumptions <- function(x) {
  out <- "\\section*{Assumptions}"
  a <- x$assumptions
  if (!nrow(a)) return(c(out, "None: the input was read without applying any notational convention, and no language model was involved."))
  c(out, "\\begin{enumerate}", paste0("\\item ", .tex_text(a$text)), "\\end{enumerate}")
}

.rep_derivative <- function(x) {
  out <- "\\section*{Derivative}"
  if (is.null(x$derivative)) return(c(out, "Not available."))
  d <- x$derivative
  disp <- .display_forms(x)
  for (k in seq_along(disp$entries)) {
    out <- c(out, "\\begin{align*}", .aligned_lines(d$labels[k], disp$entries[[k]]), "\\end{align*}")
  }
  if (length(disp$abbreviations) || length(disp$definitions)) {
    out <- c(out, "where", "\\begin{align*}")
    lines <- character()
    for (nm in names(disp$abbreviations)) lines <- c(lines, .aligned_lines(.symbol_to_latex(nm), disp$abbreviations[[nm]]))
    for (nm in names(disp$definitions)) lines <- c(lines, .aligned_lines(.symbol_to_latex(nm), disp$definitions[[nm]]))
    out <- c(out, paste(lines, collapse = " \\\\\n"), "\\end{align*}",
             "Abbreviations and folded definitions are for display only; the certified trees are the fully expanded forms (\\texttt{x\\$derivative\\$entries}).")
  }
  if (x$type == "hessian") {
    out <- c(out, "The Hessian entries were computed independently (both triangles), each as the DefDiff gradient of a DefDiff gradient entry.")
  }
  if (length(x$notes)) {
    out <- c(out, "Notes:", "\\begin{itemize}", paste0("\\item ", .tex_text(x$notes)), "\\end{itemize}")
  }
  out
}

# Display-only shortening of derivative entries: fold the `where`
# definitions back into their symbols, then name repeated large sub-trees
# A_1, A_2, ... (largest first). Exact: identical sub-trees only.
.display_forms <- function(x, min_chars = 60L, max_abbr = 12L) {
  entries <- as.list(x$derivative$entries)
  defs <- x$parsed$definition_trees
  used <- character()
  for (nm in names(defs)) {
    hit <- vapply(entries, .contains_subtree, TRUE, target = defs[[nm]])
    if (any(hit)) {
      used <- c(used, nm)
      entries <- lapply(entries, .replace_subtree, target = defs[[nm]], sym = as.name(nm))
    }
  }
  bases <- unique(vapply(all.vars(c(x$parsed$canonical, entries)), function(s) .split_symbol(s)$base, ""))
  letter <- setdiff(c("A", "B", "C", "K", "Q", "W"), bases)[1L]
  abbr <- list()
  if (!is.na(letter)) {
    for (k in seq_len(max_abbr)) {
      cand <- .repeated_subtrees(c(entries, abbr), min_chars)
      if (!length(cand)) break
      sym <- as.name(paste0(letter, "_", k))
      entries <- lapply(entries, .replace_subtree, target = cand, sym = sym)
      abbr <- lapply(abbr, .replace_subtree, target = cand, sym = sym)
      abbr[[as.character(sym)]] <- cand
    }
  }
  defs_out <- lapply(used, function(nm) defs[[nm]])
  names(defs_out) <- used
  list(entries = entries, abbreviations = abbr, definitions = defs_out)
}

.contains_subtree <- function(e, target) {
  if (identical(e, target)) return(TRUE)
  is.call(e) && any(vapply(as.list(e)[-1L], .contains_subtree, TRUE, target = target))
}

.replace_subtree <- function(e, target, sym) {
  if (identical(e, target)) return(sym)
  if (is.call(e) && length(e) > 1L) {
    for (j in seq_along(e)[-1L]) if (!is.null(e[[j]])) e[[j]] <- .replace_subtree(e[[j]], target, sym)
  }
  e
}

# The largest call sub-tree (by printed length) occurring at least twice.
.repeated_subtrees <- function(trees, min_chars) {
  seen <- new.env(hash = TRUE, parent = emptyenv())
  walk <- function(e) {
    if (!is.call(e) || .is_call(e, "numeric_partial")) return(invisible())
    key <- paste(deparse(e), collapse = "")
    if (is.null(seen[[key]])) seen[[key]] <- list(tree = e, n = 0L)
    seen[[key]]$n <- seen[[key]]$n + 1L
    for (a in as.list(e)[-1L]) walk(a)
  }
  for (t in trees) walk(t)
  best <- NULL
  best_len <- min_chars - 1L
  for (key in ls(seen)) {
    item <- seen[[key]]
    if (item$n < 2L) next
    len <- nchar(.tex(item$tree))
    if (len > best_len) {
      best <- item$tree
      best_len <- len
    }
  }
  best
}

# "label &= t1 + t2 \\ &\quad - t3 ..." splitting a long top-level sum.
.aligned_lines <- function(label, e) {
  terms <- list()
  signs <- character()
  while (is.call(e) && (.is_call(e, "+", 2L) || .is_call(e, "-", 2L))) {
    terms <- c(list(e[[3L]]), terms)
    signs <- c(as.character(e[[1L]]), signs)
    e <- e[[2L]]
  }
  terms <- c(list(e), terms)
  signs <- c("", signs)
  pieces <- vapply(seq_along(terms), function(k) {
    s <- .tex_child(terms[[k]], if (k == 1L) 10 else 11)
    if (k == 1L) s else paste(signs[k], s)
  }, "")
  lines <- character()
  cur <- ""
  for (pc in pieces) {
    if (nzchar(cur) && nchar(cur) + nchar(pc) > 160L) {
      lines <- c(lines, cur)
      cur <- pc
    } else {
      cur <- if (nzchar(cur)) paste(cur, pc) else pc
    }
  }
  lines <- c(lines, cur)
  first <- paste0(label, " &= ", lines[1L])
  rest <- if (length(lines) > 1L) paste0("&\\quad ", lines[-1L]) else character()
  paste(c(first, rest), collapse = " \\\\\n")
}

.fmt_err <- function(v) ifelse(is.na(v), "n/a", sprintf("%.2e", v))

.rep_verification <- function(x) {
  out <- "\\section*{Verification}"
  c0 <- x$certificate
  if (is.null(c0$roundtrip)) return(c(out, paste0("Not run: ", .tex_text(paste(c0$failures, collapse = "; ")), ".")))
  rt <- c0$roundtrip
  item_tex <- ifelse(rt$item == "source", "source formula $f$",
                     ifelse(startsWith(rt$item, "gradient "), paste0("gradient entry $",
                            vapply(sub("^gradient ", "", rt$item), .symbol_to_latex, ""), "$"),
                            paste0("$", rt$item, "$")))
  out <- c(out, "\\textbf{Round-trip} ($\\mathrm{normalize}(\\mathrm{parse}(\\mathrm{print}(e))) = e$):",
           "\\begin{center}\\begin{tabular}{ll}", "\\toprule", "Tree & Status \\\\", "\\midrule",
           paste0(item_tex, " & ", ifelse(rt$ok, "pass", paste0("FAIL (", .tex_text(rt$message), ")")), " \\\\"),
           "\\bottomrule", "\\end{tabular}\\end{center}")
  nm <- c0$numeric
  dom <- paste0("$", vapply(names(nm$domain), .symbol_to_latex, ""), " \\in [",
                vapply(nm$domain, function(d) format(d[1L]), ""), ", ",
                vapply(nm$domain, function(d) format(d[2L]), ""), "]$", collapse = ", ")
  out <- c(out, paste0("\\textbf{Numeric}: ", nm$n_valid, " of ", nm$n_requested,
                       " requested random points were valid (", nm$n_rejected, " rejected",
                       if (!is.null(nm$first_problem)) paste0("; first problem: ", .tex_text(nm$first_problem)),
                       "); seed ", nm$seed, "; sampling box ", dom,
                       ". Each entry is compared with a Richardson-extrapolated central difference",
                       if (x$type == "hessian") " of the symbolic gradient (gradient entries: of $f$)" else " of $f$",
                       "; error $= |a - b| / \\max(1, |b|)$; tolerance ", format(nm$tol), "."),
           "\\begin{center}\\begin{tabular}{lll}", "\\toprule",
           "Entry & max relative error & numeric-partial nodes \\\\", "\\midrule",
           paste0("$", nm$per_entry$entry, "$ & ", .fmt_err(nm$per_entry$max_rel_error), " & ",
                  ifelse(nm$per_entry$numeric_partial, "yes", "no"), " \\\\"),
           "\\bottomrule", "\\end{tabular}\\end{center}",
           paste0("Overall: max relative error ", .fmt_err(nm$max_rel_error), " -- ",
                  if (nm$pass) "\\textbf{pass}." else "\\textbf{FAIL}."))
  lv <- c0$levels
  closure <- if (is.na(lv$closure)) "not applicable (the source contains out-of-scope terms)" else
    if (lv$closure) "preserved" else "NOT preserved"
  out <- c(out, paste0("\\textbf{Closure level} (DefDiff \\texttt{level()}): source ",
                       .tex_text(lv$source), "; entries ", .tex_text(paste(unique(lv$entries), collapse = ", ")),
                       "; closure ", closure, ". ", lv$n_numeric, " of ", length(lv$entries),
                       " entries contain numeric-partial nodes."))
  out
}

.rep_oos <- function(x) {
  out <- "\\section*{Out of scope}"
  o <- x$out_of_scope
  if (!nrow(o)) {
    return(c(out, if (is.null(x$derivative)) "Not available." else
      "None: every sub-term is in DefDiff's language."))
  }
  c(out, paste0("Sub-terms outside DefDiff's language. Where a term depends on the variable, the ",
                "chain rule is completed with a numeric partial derivative ",
                "(Richardson-extrapolated central differences), marked $\\left.\\cdot\\right|_{\\mathrm{num}}$ above."),
    "\\begin{center}\\begin{tabular}{p{4.2cm}lllp{5.2cm}}", "\\toprule",
    "Term & Head & Variable & Treatment & Reason \\\\", "\\midrule",
    paste0("$", o$latex, "$ & \\texttt{", .tex_text(o$head), "} & $", vapply(o$variable, .symbol_to_latex, ""),
           "$ & ", .tex_text(o$treatment), " & ", .tex_text(o$reason), " \\\\"),
    "\\bottomrule", "\\end{tabular}\\end{center}")
}

.rep_conclusion <- function(x) {
  s <- .conclusion(x)
  if (startsWith(s, "CERTIFIED")) {
    paste0("\\textbf{CERTIFIED}", .tex_text(substring(s, 10L)))
  } else {
    paste0("\\textbf{NOT CERTIFIED}", .tex_text(substring(s, 14L)))
  }
}
