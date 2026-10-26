## tex_print.R
## Canonical tree -> LaTeX (DefTeX interim layer, design D1/D5).
##
## Printing choices that make `normalize_tree(parse_latex(to_latex(e))$expr)`
## reproduce `e`: division is always \frac, function arguments are always
## \left( ... \right), a factor that starts with a digit, a sign, or a
## parenthesis is joined with \cdot, and numbers print as integers, exact
## rationals, or the shortest decimal that parses back to the same double.

#' Print an expression tree as LaTeX
#'
#' @param expr A canonical expression tree (see [normalize_tree()]), or a list
#'   of trees.
#' @return A character string (a character vector for a list input).
#' @export
#' @examples
#' to_latex(quote(-(a + b) * c^(x + 1)))
#' to_latex(normalize_tree(quote(1/3 * sin(x))))
to_latex <- function(expr) {
  if (is.list(expr) && !is.call(expr)) return(vapply(expr, .tex, ""))
  .tex(expr)
}

.fun_latex <- c(sin = "\\sin", cos = "\\cos", tan = "\\tan", cot = "\\cot",
                sec = "\\sec", csc = "\\csc", sinh = "\\sinh", cosh = "\\cosh",
                tanh = "\\tanh", asin = "\\arcsin", acos = "\\arccos",
                atan = "\\arctan", exp = "\\exp", log = "\\log", gamma = "\\Gamma",
                pnorm = "\\Phi")

.paren <- function(s) paste0("\\left(", s, "\\right)")

.prec <- function(e) {
  if (is.numeric(e)) return(if (e < 0) 15 else if (startsWith(.fmt_num(e), "\\frac")) 30 else 100)
  if (is.symbol(e)) return(100)
  if (!is.call(e) || !is.symbol(e[[1L]])) return(100)
  head <- as.character(e[[1L]])
  if (head == "(") return(.prec(e[[2L]]))
  n <- length(e) - 1L
  if (head %in% c("+", "-") && n == 2L) return(10)
  if (head == "-" && n == 1L) return(15)
  if (head == "*") return(20)
  if (head == "/") return(30)
  if (head == "^") return(40)
  if (head == "c") return(5)
  90
}

.tex_child <- function(e, need) {
  s <- .tex(e)
  if (.prec(e) < need) .paren(s) else s
}

.tex <- function(e) {
  if (is.numeric(e)) {
    if (length(e) != 1L) stop("cannot print a numeric vector", call. = FALSE)
    if (e < 0) return(paste0("-", .tex_child(-e, 20)))
    return(.fmt_num(e))
  }
  if (is.symbol(e)) {
    nm <- as.character(e)
    return(if (nm == "pi") "\\pi" else .symbol_to_latex(nm))
  }
  if (!is.call(e) || !is.symbol(e[[1L]])) stop("cannot print this expression", call. = FALSE)
  head <- as.character(e[[1L]])
  if (head == "(") return(.tex(e[[2L]]))
  args <- as.list(e)[-1L]
  n <- length(args)
  if (head %in% c("+", "-") && n == 2L) {
    return(paste(.tex_child(args[[1L]], 10), head, .tex_child(args[[2L]], 11)))
  }
  if (head == "-" && n == 1L) return(paste0("-", .tex_child(args[[1L]], 20)))
  if (head == "*" && n == 2L) return(.tex_product(e))
  if (head == "/" && n == 2L) {
    return(paste0("\\frac{", .tex(args[[1L]]), "}{", .tex(args[[2L]]), "}"))
  }
  if (head == "^" && n == 2L) {
    return(paste0(.tex_child(args[[1L]], 90), "^{", .tex(args[[2L]]), "}"))
  }
  if (head %in% names(.fun_latex) && n == 1L) {
    return(paste0(.fun_latex[[head]], .paren(.tex(args[[1L]]))))
  }
  switch(head,
    sqrt = paste0("\\sqrt{", .tex(args[[1L]]), "}"),
    abs = paste0("\\left|", .tex(args[[1L]]), "\\right|"),
    factorial = paste0(.tex_child(args[[1L]], 90), "!"),
    choose = paste0("\\binom{", .tex(args[[1L]]), "}{", .tex(args[[2L]]), "}"),
    qt = {
      df <- args[[2L]]
      if (is.symbol(df) || (is.numeric(df) && df >= 0 && df == round(df))) {
        paste0("t_{", .tex(df), "}^{-1}", .paren(.tex(args[[1L]])))
      } else {
        paste0("\\operatorname{qt}", .paren(paste(vapply(args, .tex, ""), collapse = ", ")))
      }
    },
    qnorm = if (n == 1L) paste0("\\Phi^{-1}", .paren(.tex(args[[1L]]))) else
      paste0("\\operatorname{qnorm}", .paren(paste(vapply(args, .tex, ""), collapse = ", "))),
    numeric_partial = .tex_numeric_partial(args),
    c = paste0("\\begin{pmatrix} ", paste(vapply(args, .tex, ""), collapse = " \\\\ "),
               " \\end{pmatrix}"),
    paste0("\\operatorname{", head, "}", .paren(paste(vapply(args, .tex, ""), collapse = ", "))))
}

.tex_product <- function(e) {
  factors <- list()
  while (.is_call(e, "*", 2L)) {
    factors <- c(list(e[[3L]]), factors)
    e <- e[[2L]]
  }
  factors <- c(list(e), factors)
  parts <- vapply(factors, .tex_child, "", need = 20)
  out <- parts[1L]
  for (k in seq_along(parts)[-1L]) {
    s <- parts[k]
    tight <- grepl("^[0-9.-]", s) || startsWith(s, "\\left(") || startsWith(s, "\\frac{") &&
      is.numeric(factors[[k]])
    out <- paste0(out, if (tight) " \\cdot " else " ", s)
  }
  out
}

.tex_numeric_partial <- function(args) {
  g <- args[[1L]]
  vars <- vapply(args[-1L], as.character, "")
  k <- length(vars)
  num <- if (k == 1L) "\\partial" else paste0("\\partial^{", k, "}")
  den <- paste(paste0("\\partial ", vapply(vars, .symbol_to_latex, "")), collapse = " \\, ")
  paste0("\\left.\\frac{", num, "}{", den, "}\\left[", .tex(g), "\\right]\\right|_{\\mathrm{num}}")
}

# Non-negative number -> LaTeX: integer, small exact rational, shortest
# round-tripping decimal, larger rational, long decimal.
.fmt_num <- function(x) {
  if (x == round(x) && x < 1e15) return(sprintf("%.0f", x))
  r <- .as_rational(x)
  if (!is.null(r) && r[2] <= 100) return(sprintf("\\frac{%.0f}{%.0f}", r[1], r[2]))
  for (d in 1:6) {
    s <- formatC(x, digits = d, format = "fg")
    if (as.numeric(s) == x) return(trimws(s))
  }
  if (!is.null(r)) return(sprintf("\\frac{%.0f}{%.0f}", r[1], r[2]))
  for (d in 7:17) {
    s <- formatC(x, digits = d, format = "fg")
    if (as.numeric(s) == x) return(trimws(s))
  }
  sprintf("%.17g", x)
}
