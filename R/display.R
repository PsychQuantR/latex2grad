## display.R
## Presentation of long results in the LaTeX / Markdown reports: repeated
## sub-expressions are abbreviated ("where A_1 = ...") and long sums are broken
## across aligned lines. Presentation only - the certificate (round-trip and
## numeric check) is always computed on the full expressions.

# Abbreviate sub-expressions repeated across `exprs`. Returns the rewritten
# expressions and the definitions, in an order where each definition uses
# only names defined before it.
.l2g_abbreviate <- function(exprs, max_defs = 8L) {
  avoid <- unique(unlist(lapply(exprs, all.names)))
  letters_ok <- setdiff(c("A", "B", "C", "D", "E", "G", "H", "K", "M", "N", "P",
                          "Q", "R", "S", "U", "V", "W", "Z"), avoid)
  if (!length(letters_ok)) return(list(exprs = exprs, defs = list()))
  letter <- letters_ok[[1L]]
  defs <- list()
  for (k in seq_len(max_defs)) {
    pool <- c(exprs, lapply(defs, `[[`, "expr"))
    counts <- new.env(parent = emptyenv())
    nodes <- list()
    collect <- function(e) {
      if (!is.call(e)) return(invisible())
      key <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
      if (is.null(counts[[key]])) { counts[[key]] <- 0L; nodes[[key]] <<- e }
      counts[[key]] <- counts[[key]] + 1L
      head <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
      if (head %in% .l2g_binders) return(invisible())
      for (j in seq_along(e)[-1L]) collect(e[[j]])
    }
    for (e in pool) collect(e)
    best <- NULL; best_score <- 5
    for (key in names(nodes)) {
      n <- counts[[key]]
      if (n < 2L) next
      size <- length(all.names(nodes[[key]]))
      if (size < 3L) next
      score <- (n - 1L) * size
      if (score > best_score) { best_score <- score; best <- nodes[[key]] }
    }
    if (is.null(best)) break
    sym <- as.name(paste0(".l2g_abbr", k))
    exprs <- lapply(exprs, .l2g_replace_subtree, best, sym)
    defs <- lapply(defs, function(d) { d$expr <- .l2g_replace_subtree(d$expr, best, sym); d })
    defs[[length(defs) + 1L]] <- list(tmp = as.character(sym), expr = best)
  }
  if (!length(defs)) return(list(exprs = exprs, defs = list()))
  # Order definitions so each uses only names defined before it.
  tmps <- vapply(defs, `[[`, "", "tmp")
  ordered <- list()
  placed <- character(0)
  while (length(ordered) < length(defs)) {
    for (d in defs) {
      if (d$tmp %in% placed) next
      uses <- intersect(all.names(d$expr), tmps)
      if (all(uses %in% placed)) {
        ordered[[length(ordered) + 1L]] <- d
        placed <- c(placed, d$tmp)
      }
    }
  }
  defs <- ordered
  binds <- list()
  for (i in seq_along(defs)) {
    binds[[defs[[i]]$tmp]] <- as.name(paste0(letter, "_", i))
  }
  defs <- lapply(seq_along(defs), function(i) {
    list(name = paste0(letter, "_", i), expr = .l2g_subst(defs[[i]]$expr, binds))
  })
  list(exprs = lapply(exprs, .l2g_subst, binds), defs = defs)
}

.l2g_replace_subtree <- function(e, target, sym) {
  if (identical(e, target)) return(sym)
  if (!is.call(e)) return(e)
  head <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
  if (head %in% .l2g_binders) return(e)
  for (j in seq_along(e)[-1L]) e[[j]] <- .l2g_replace_subtree(e[[j]], target, sym)
  e
}

# Split a left-associated +/- chain into signed terms.
.l2g_terms <- function(e) {
  if (.l2g_is_op(e, "+", 2L)) return(c(.l2g_terms(e[[2L]]), list(list(op = "+", e = e[[3L]]))))
  if (.l2g_is_op(e, "-", 2L)) return(c(.l2g_terms(e[[2L]]), list(list(op = "-", e = e[[3L]]))))
  list(list(op = "", e = e))
}

# Break the printed form of `e` into pieces between which a line break is
# allowed. Long sums break between terms; a long fraction N / D is shown as
# (1/D) [N]; a long parenthesised factor opens \Bigl( ... \Bigr). The pieces
# concatenate to a rendering of the same expression.
.l2g_pieces <- function(e, width) {
  f <- .l2g_tex(e)
  if (nchar(f$s) <= width) return(f$s)
  wrap <- function(p, open, close) {
    p[[1L]] <- paste0(open, p[[1L]])
    p[[length(p)]] <- paste0(p[[length(p)]], close)
    p
  }
  if (.l2g_is_op(e, "+", 2L) || .l2g_is_op(e, "-", 2L)) {
    out <- character(0)
    for (t in .l2g_terms(e)) {
      p <- .l2g_pieces(t$e, width)
      ft <- .l2g_tex(t$e)
      if (t$op != "" && ft$p <= .P_NEG) p <- wrap(p, "\\Bigl(", "\\Bigr)")
      if (t$op != "") p[[1L]] <- paste0(t$op, " ", p[[1L]])
      out <- c(out, p)
    }
    return(out)
  }
  if (.l2g_neg(e)) {
    inner <- e[[2L]]
    p <- .l2g_pieces(inner, width)
    if (.l2g_tex(inner)$p <= .P_NEG) return(wrap(p, "-\\Bigl(", "\\Bigr)"))
    p[[1L]] <- paste0("-", p[[1L]])
    return(p)
  }
  if (.l2g_is_op(e, "/", 2L)) {
    p <- .l2g_pieces(e[[2L]], width)
    if (length(p) == 1L) return(f$s)
    return(wrap(p, paste0("\\frac{1}{", .l2g_tex(e[[3L]])$s, "} \\Bigl[ "), " \\Bigr]"))
  }
  if (.l2g_is_op(e, "*", 2L)) {
    facs <- list()
    walk <- function(x) {
      if (.l2g_is_op(x, "*", 2L)) { walk(x[[2L]]); walk(x[[3L]]) } else facs[[length(facs) + 1L]] <<- x
    }
    walk(e)
    out <- character(0)
    cur <- ""
    for (fa in facs) {
      tf <- .l2g_tex(fa)
      s <- if (tf$p <= .P_PROD) .l2g_paren(tf) else tf$s
      if (nchar(s) > width && (tf$p <= .P_NEG || .l2g_is_op(fa, "/", 2L))) {
        p <- .l2g_pieces(fa, width)
        if (tf$p <= .P_NEG) p <- wrap(p, "\\Bigl(", "\\Bigr)")
        p[[1L]] <- paste0(cur, if (nzchar(cur)) " " else "", p[[1L]])
        out <- c(out, p)
        cur <- ""
      } else {
        cur <- if (nzchar(cur)) paste(cur, s) else s
      }
    }
    if (nzchar(cur)) out <- c(out, cur)
    return(out)
  }
  f$s
}

# Display lines for `lhs = expr`: a list of c(left part, right part) for an
# aligned environment; pieces are packed greedily up to `width` characters.
.l2g_display_lines <- function(lhs, expr, width = 150L) {
  pieces <- .l2g_pieces(expr, width)
  lines <- character(0)
  current <- NULL
  for (piece in pieces) {
    if (!is.null(current) && nchar(current) + nchar(piece) + 1L > width) {
      lines <- c(lines, current)
      current <- piece
    } else {
      current <- if (is.null(current)) piece else paste(current, piece)
    }
  }
  lines <- c(lines, current)
  out <- list(c(lhs, lines[[1L]]))
  for (l in lines[-1L]) out[[length(out) + 1L]] <- c("", paste0("\\quad ", l))
  out
}

# LaTeX block for several `lhs = rhs` equations (align*); over-long single
# lines are scaled to the text width.
.l2g_latex_block <- function(eqs, width = 150L) {
  rows <- character(0)
  for (eq in eqs) {
    for (ln in .l2g_display_lines(eq$lhs, eq$expr, width)) {
      rhs <- ln[[2L]]
      if (nchar(rhs) > 2L * width) {
        rhs <- paste0("\\resizebox{0.8\\linewidth}{!}{$\\displaystyle ", rhs, "$}")
      }
      rows <- c(rows, paste0(ln[[1L]], if (nzchar(ln[[1L]])) " &= " else " & ", rhs))
    }
  }
  c("\\begin{align*}", paste(rows, collapse = " \\\\\n"), "\\end{align*}")
}

.l2g_markdown_block <- function(eqs, width = 150L) {
  rows <- character(0)
  for (eq in eqs) {
    for (ln in .l2g_display_lines(eq$lhs, eq$expr, width)) {
      rows <- c(rows, paste0(ln[[1L]], if (nzchar(ln[[1L]])) " &= " else " & ", ln[[2L]]))
    }
  }
  c("$$", "\\begin{aligned}", paste(rows, collapse = " \\\\\n"), "\\end{aligned}", "$$")
}

# Equations of the Derivative section, abbreviated when long.
.l2g_derivative_eqs <- function(x) {
  exprs <- lapply(x$derivatives, `[[`, "expr")
  long <- any(vapply(x$derivatives, function(e) nchar(e$tex) > 400L, logical(1)))
  ab <- if (long) .l2g_abbreviate(exprs) else list(exprs = exprs, defs = list())
  eqs <- lapply(seq_along(exprs), function(k) list(lhs = x$derivatives[[k]]$label, expr = ab$exprs[[k]]))
  defs <- lapply(ab$defs, function(d) list(lhs = .l2g_symbol_tex(d$name), expr = d$expr))
  list(eqs = eqs, defs = defs)
}
