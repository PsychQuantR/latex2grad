## printer.R
## R expression -> LaTeX. Output is chosen so that `tex_parse()` followed by
## `tex_normalize()` returns the same canonical tree: every function argument
## is parenthesised, products of letters are separated by spaces, numbers are
## printed with enough digits to read back exactly.

# Precedence levels of printed fragments.
.P_SUM <- 1L; .P_NEG <- 2L; .P_PROD <- 3L; .P_POW <- 4L; .P_ATOM <- 5L

#' Print an expression as LaTeX
#'
#' @param expr An R call, symbol, or number (canonical form recommended; see
#'   [tex_normalize()]). A top-level `c(...)` prints as a column vector.
#' @return A single string of LaTeX math.
#' @export
#' @examples
#' tex_print(quote(exp(-x^2 / 2) / sqrt(2 * pi)))
#' tex_print(quote(qt(u, nu)))
tex_print <- function(expr) {
  if (.l2g_is_op(expr, "c")) {
    comps <- vapply(as.list(expr)[-1L], function(x) .l2g_tex(x)$s, character(1))
    return(paste0("\\begin{pmatrix} ", paste(comps, collapse = " \\\\ "), " \\end{pmatrix}"))
  }
  .l2g_tex(expr)$s
}

.l2g_frag <- function(s, p) list(s = s, p = p)

.l2g_paren <- function(f) paste0("\\left(", f$s, "\\right)")

.l2g_num_tex <- function(x) {
  if (is.na(x)) .l2g_stop("print", "cannot print NA")
  if (is.infinite(x)) return(if (x > 0) "\\infty" else "-\\infty")
  for (d in c(15L, 16L, 17L)) {
    s <- format(x, digits = d, scientific = FALSE, trim = TRUE)
    if (as.numeric(s) == x) return(s)
  }
  s
}

.l2g_tex <- function(e) {
  if (is.numeric(e)) {
    if (length(e) != 1L) .l2g_stop("print", "vector literals are not printable")
    s <- .l2g_num_tex(e)
    return(.l2g_frag(s, if (e < 0) .P_NEG else .P_ATOM))
  }
  if (is.symbol(e)) return(.l2g_frag(.l2g_symbol_tex(as.character(e)), .P_ATOM))
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    .l2g_stop("print", paste0("cannot print `", deparse1(e), "`"))
  }
  op <- as.character(e[[1L]])
  args <- as.list(e)[-1L]
  n <- length(args)

  if (op == "(") return(.l2g_tex(args[[1L]]))
  if (op == "+" && n == 2L) {
    a <- .l2g_tex(args[[1L]]); b <- .l2g_tex(args[[2L]])
    bs <- if (b$p <= .P_NEG) .l2g_paren(b) else b$s
    return(.l2g_frag(paste0(a$s, " + ", bs), .P_SUM))
  }
  if (op == "-" && n == 2L) {
    a <- .l2g_tex(args[[1L]]); b <- .l2g_tex(args[[2L]])
    bs <- if (b$p <= .P_NEG) .l2g_paren(b) else b$s
    return(.l2g_frag(paste0(a$s, " - ", bs), .P_SUM))
  }
  if (op == "-" && n == 1L) {
    a <- .l2g_tex(args[[1L]])
    as <- if (a$p <= .P_NEG) .l2g_paren(a) else a$s
    return(.l2g_frag(paste0("-", as), .P_NEG))
  }
  if (op == "+" && n == 1L) return(.l2g_tex(args[[1L]]))
  if (op == "*" && n == 2L) {
    a <- .l2g_tex(args[[1L]]); b <- .l2g_tex(args[[2L]])
    as <- if (a$p < .P_PROD) .l2g_paren(a) else a$s
    bs <- if (b$p <= .P_PROD) .l2g_paren(b) else b$s
    sep <- if (grepl("^[0-9.]", bs)) " \\cdot " else " "
    return(.l2g_frag(paste0(as, sep, bs), .P_PROD))
  }
  if (op == "/" && n == 2L) {
    return(.l2g_frag(paste0("\\frac{", .l2g_tex(args[[1L]])$s, "}{",
                            .l2g_tex(args[[2L]])$s, "}"), .P_ATOM))
  }
  if (op == "^" && n == 2L) return(.l2g_tex_pow(args[[1L]], args[[2L]]))
  if (op == "exp" && n == 1L) {
    if (.l2g_is_num(args[[1L]], 1)) return(.l2g_frag("e", .P_ATOM))
    a <- .l2g_tex(args[[1L]])
    if (nchar(a$s) <= 40L && !grepl("\\\\int", a$s)) {
      return(.l2g_frag(paste0("e^{", a$s, "}"), .P_POW))
    }
    return(.l2g_frag(paste0("\\exp", .l2g_paren(a)), .P_ATOM))
  }
  if (op == "sqrt" && n == 1L) {
    return(.l2g_frag(paste0("\\sqrt{", .l2g_tex(args[[1L]])$s, "}"), .P_ATOM))
  }
  if (op == "abs" && n == 1L) {
    return(.l2g_frag(paste0("\\left|", .l2g_tex(args[[1L]])$s, "\\right|"), .P_ATOM))
  }
  if (op %in% names(.l2g_print_fun1) && n == 1L) {
    return(.l2g_frag(paste0(.l2g_print_fun1[[op]], .l2g_paren(.l2g_tex(args[[1L]]))), .P_ATOM))
  }
  if (op %in% names(.l2g_print_fun2) && n == 2L) {
    inner <- paste0(.l2g_tex(args[[1L]])$s, ", ", .l2g_tex(args[[2L]])$s)
    return(.l2g_frag(paste0(.l2g_print_fun2[[op]], "\\left(", inner, "\\right)"), .P_ATOM))
  }
  if (op == "qt" && n == 2L) {
    return(.l2g_frag(paste0("t_{", .l2g_tex(args[[2L]])$s, "}^{-1}",
                            .l2g_paren(.l2g_tex(args[[1L]]))), .P_ATOM))
  }
  if (op == "choose" && n == 2L) {
    return(.l2g_frag(paste0("\\binom{", .l2g_tex(args[[1L]])$s, "}{",
                            .l2g_tex(args[[2L]])$s, "}"), .P_ATOM))
  }
  if (op %in% c("max", "min")) {
    inner <- paste(vapply(args, function(x) .l2g_tex(x)$s, character(1)), collapse = ", ")
    return(.l2g_frag(paste0("\\", op, "\\left(", inner, "\\right)"), .P_ATOM))
  }
  if (op == "integral" && n == 4L) {
    body <- .l2g_tex(args[[1L]])$s
    s <- paste0("\\int_{", .l2g_tex(args[[3L]])$s, "}^{", .l2g_tex(args[[4L]])$s,
                "} ", body, " \\,\\mathrm{d}", .l2g_symbol_tex(as.character(args[[2L]])))
    return(.l2g_frag(s, .P_PROD))
  }
  if (op == "numd" && n == 3L) {
    s <- .l2g_symbol_tex(as.character(args[[2L]]))
    body <- .l2g_paren(.l2g_tex(args[[1L]]))
    out <- paste0("\\left.\\frac{\\partial}{\\partial ", s, "} ", body, "\\right|_{",
                  s, " = ", .l2g_tex(args[[3L]])$s, "}")
    return(.l2g_frag(out, .P_ATOM))
  }
  # Generic function application.
  inner <- paste(vapply(args, function(x) .l2g_tex(x)$s, character(1)), collapse = ", ")
  .l2g_frag(paste0("\\operatorname{", gsub("_", "\\\\_", op), "}\\left(", inner, "\\right)"),
            .P_ATOM)
}

.l2g_tex_pow <- function(base, ex) {
  b <- .l2g_tex(base)
  x <- .l2g_tex(ex)$s
  # \sin^{2}\left(x\right) for the elementary function commands.
  if (is.call(base) && is.symbol(base[[1L]]) && length(base) == 2L &&
      !identical(ex, call("-", 1))) {
    f <- as.character(base[[1L]])
    cmd <- c(sin = "\\sin", cos = "\\cos", tan = "\\tan", sinh = "\\sinh",
             cosh = "\\cosh", tanh = "\\tanh", log = "\\ln")
    if (f %in% names(cmd)) {
      return(.l2g_frag(paste0(cmd[[f]], "^{", x, "}", .l2g_paren(.l2g_tex(base[[2L]]))),
                       .P_ATOM))
    }
  }
  head <- if (is.call(base) && is.symbol(base[[1L]])) as.character(base[[1L]]) else ""
  simple <- is.symbol(base) || (is.numeric(base) && base >= 0) ||
    head %in% c("sqrt", "abs", "qt", "max", "min", "choose",
                names(.l2g_print_fun1), names(.l2g_print_fun2))
  bs <- if (simple) b$s else .l2g_paren(b)
  .l2g_frag(paste0(bs, "^{", x, "}"), .P_POW)
}
