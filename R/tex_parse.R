## tex_parse.R
## Deterministic LaTeX -> R expression tree parser (DefTeX interim layer, D1).
##
## Precedence, loosest first:
##   expr     := term (('+' | '-') term)*
##   term     := implicit (('\cdot' | '\times' | '*' | '/') implicit)*
##   implicit := unary power*                 (juxtaposition)
##   unary    := ('-' | '+') unary | power
##   power    := postfix ('^' script)?
##   postfix  := primary '!'?
## Implicit products bind tighter than an explicit '/', so `a/bc` is a/(b c)
## (recorded as an assumption, design D9). Every convention the grammar applies
## is recorded through .assume(); in strict mode the ambiguous ones raise
## `latex2grad_ambiguity` instead.

# R function for each LaTeX function command.
.fun_cmds <- c(
  sin = "sin", cos = "cos", tan = "tan", cot = "cot", sec = "sec", csc = "csc",
  sinh = "sinh", cosh = "cosh", tanh = "tanh", arcsin = "asin", arccos = "acos",
  arctan = "atan", exp = "exp", log = "log", ln = "log"
)
.inverse_fun <- c(sin = "asin", cos = "acos", tan = "atan")

# Function names accepted through \operatorname{} / \mathrm{} (allowlist: the
# parser never builds a call to anything else), with their arity.
.named_fun_arity <- c(
  sin = 1, cos = 1, tan = 1, cot = 1, sec = 1, csc = 1, sinh = 1, cosh = 1,
  tanh = 1, asin = 1, acos = 1, atan = 1, exp = 1, log = 1, sqrt = 1, abs = 1,
  gamma = 1, lgamma = 1, digamma = 1, trigamma = 1, factorial = 1,
  pnorm = 1, qnorm = 1, dnorm = 1, qt = 2, pt = 2, dt = 2,
  beta = 2, lbeta = 2, choose = 2
)

# Plain-letter runs read as function names (assumption `plain_function`).
.letter_functions <- c("sinh", "cosh", "tanh", "sin", "cos", "tan", "exp", "log", "ln")

# Assumption kinds that strict mode turns into errors.
.strict_kinds <- c("letters", "symbol_paren", "arg_scope", "slash", "fn_power",
                   "inverse_fn", "euler", "phi_cdf", "t_quantile", "plain_function")

#' Parse LaTeX into an R expression tree
#'
#' Deterministic grammar for LaTeX math. Every notational convention applied
#' while parsing (implicit multiplication, the scope of an unbraced function
#' argument, Euler's `e`, ...) is returned as an assumption.
#'
#' @param latex A single LaTeX math string (without `$` delimiters).
#' @param vars Optional character vector of symbol names that are variables.
#'   Declaring `"e"` stops `e` from being read as Euler's number.
#' @param functions Optional named character vector mapping a LaTeX symbol name
#'   to an R function name, e.g. `c(f = "myfun")`; `f(x)` is then a function
#'   application instead of a product.
#' @param strict If `TRUE`, conventions that resolve a genuine ambiguity raise a
#'   condition of class `latex2grad_ambiguity` instead of being applied.
#' @return An object of class `latex2grad_parsed`: a list with `expr` (the
#'   tree, not yet normalized), `assumptions` (data frame with columns `kind`
#'   and `text`), `symbols` (free symbols), and `latex` (the input).
#' @export
#' @examples
#' parse_latex("\\frac{1}{2} x^2 + \\sin x y")
parse_latex <- function(latex, vars = NULL, functions = NULL, strict = FALSE) {
  p <- new.env(parent = emptyenv())
  p$src <- latex
  p$toks <- .tex_tokenize(latex)
  p$n <- length(p$toks)
  p$i <- 1L
  p$strict <- isTRUE(strict)
  p$vars <- as.character(vars)
  p$functions <- if (is.null(functions)) character() else unlist(functions)
  p$abs_depth <- 0L
  p$last_n_factors <- 1L
  p$meta <- list()
  p$assumptions <- list()
  if (p$n == 0L) .parse_error("empty formula")
  expr <- .p_top(p)
  structure(
    list(expr = expr, assumptions = .assumption_df(p$assumptions),
         symbols = .free_symbols(expr), latex = latex),
    class = "latex2grad_parsed"
  )
}

#' @export
print.latex2grad_parsed <- function(x, ...) {
  cat("<latex2grad_parsed>\n")
  cat("  tree:   ", paste(deparse(x$expr), collapse = " "), "\n", sep = "")
  cat("  symbols:", if (length(x$symbols)) paste(x$symbols, collapse = ", ") else "(none)", "\n")
  if (nrow(x$assumptions)) {
    cat("  assumptions:\n")
    for (t in x$assumptions$text) cat("   - ", t, "\n", sep = "")
  }
  invisible(x)
}

# ---- conditions -------------------------------------------------------------

.parse_error <- function(message, position = NULL) {
  if (!is.null(position)) message <- paste0(message, " (at character ", position, ")")
  stop(structure(class = c("latex2grad_parse_error", "error", "condition"),
                 list(message = message, call = NULL, position = position)))
}

.assume <- function(p, kind, text) {
  if (p$strict && kind %in% .strict_kinds) {
    stop(structure(class = c("latex2grad_ambiguity", "error", "condition"),
                   list(message = paste0("ambiguous notation: ", text), call = NULL,
                        kind = kind)))
  }
  p$assumptions[[length(p$assumptions) + 1L]] <- list(kind = kind, text = text)
  invisible(NULL)
}

.assumption_df <- function(items) {
  if (!length(items)) return(data.frame(kind = character(), text = character(),
                                        stringsAsFactors = FALSE))
  df <- data.frame(kind = vapply(items, `[[`, "", "kind"),
                   text = vapply(items, `[[`, "", "text"), stringsAsFactors = FALSE)
  df[!duplicated(df$text), , drop = FALSE]
}

# ---- token helpers ----------------------------------------------------------

.peek <- function(p, k = 0L) {
  j <- p$i + k
  if (j > p$n) NULL else p$toks[[j]]
}
.advance <- function(p) {
  tok <- .peek(p)
  if (is.null(tok)) .parse_error("unexpected end of formula")
  p$i <- p$i + 1L
  tok
}
.is <- function(tok, type, value = NULL) {
  !is.null(tok) && identical(tok$type, type) && (is.null(value) || tok$value %in% value)
}
.expect <- function(p, type, value = NULL, what = value) {
  tok <- .peek(p)
  if (!.is(tok, type, value)) {
    got <- if (is.null(tok)) "end of formula" else paste0("'", tok$value, "'")
    .parse_error(paste0("expected ", what, " but found ", got), tok$start)
  }
  .advance(p)
}
.span <- function(p, from, to) {
  substr(p$src, p$toks[[from]]$start, p$toks[[to]]$end)
}
.opens_paren <- function(tok) {
  .is(tok, "op", c("(", "[")) || .is(tok, "cmd", "left")
}

# ---- grammar ----------------------------------------------------------------

.p_top <- function(p) {
  eq <- .top_level_equals(p)
  if (length(eq) > 1L) .parse_error("more than one '=' in the formula", p$toks[[eq[2]]]$start)
  if (length(eq) == 1L) {
    if (eq == 1L || eq == p$n) .parse_error("'=' needs a left- and a right-hand side")
    .assume(p, "lhs", paste0("Left-hand side `", .span(p, 1L, eq - 1L),
                             "` names the formula; the right-hand side is differentiated"))
    p$i <- eq + 1L
  }
  expr <- if (.is(.peek(p), "cmd", "begin")) .p_vector(p) else .p_expr(p)
  tok <- .peek(p)
  if (!is.null(tok)) .parse_error(paste0("unexpected '", tok$value, "'"), tok$start)
  expr
}

.top_level_equals <- function(p) {
  depth <- 0L
  out <- integer()
  for (j in seq_len(p$n)) {
    t <- p$toks[[j]]
    if (.is(t, "lbrace") || .is(t, "op", c("(", "["))) depth <- depth + 1L
    if (.is(t, "rbrace") || .is(t, "op", c(")", "]"))) depth <- depth - 1L
    if (depth == 0L && .is(t, "op", "=")) out <- c(out, j)
  }
  out
}

.p_expr <- function(p) {
  left <- .p_term(p)
  while (.is(.peek(p), "op", c("+", "-"))) {
    op <- .advance(p)$value
    left <- call(op, left, .p_term(p))
  }
  left
}

.p_term <- function(p) {
  left <- .p_implicit(p)
  repeat {
    tok <- .peek(p)
    if (.is(tok, "cmd", c("cdot", "times")) || .is(tok, "op", "*")) {
      .advance(p)
      left <- call("*", left, .p_implicit(p))
    } else if (.is(tok, "op", "/")) {
      slash <- p$i
      .advance(p)
      right <- .p_implicit(p)
      if (p$last_n_factors > 1L) {
        .assume(p, "slash", paste0("`", .span(p, slash - 1L, p$i - 1L),
                                   "`: the implicit product after '/' is the whole denominator"))
      }
      left <- call("/", left, right)
    } else {
      break
    }
  }
  left
}

.starts_factor <- function(p) {
  tok <- .peek(p)
  if (is.null(tok)) return(FALSE)
  switch(tok$type,
    num = , letter = , name = , lbrace = TRUE,
    op = tok$value %in% c("(", "[") || (tok$value == "|" && p$abs_depth == 0L),
    cmd = tok$value %in% c(.greek, names(.fun_cmds), "frac", "dfrac", "tfrac",
                           "sqrt", "left", "sum", "prod", "binom", "lvert", "{"),
    FALSE)
}

.p_implicit <- function(p) {
  start <- p$i
  factors <- list(.p_unary(p))
  while (.starts_factor(p)) {
    prev <- factors[[length(factors)]]
    nxt <- .peek(p)
    if (is.symbol(prev) && .opens_paren(nxt) &&
        !(as.character(prev) %in% .constant_symbols)) {
      .assume(p, "symbol_paren", paste0("`", .span(p, start, p$i), "...)` is read as a product with `",
                                        as.character(prev), "`, not as a function application ",
                                        "(declare `functions` to change this)"))
    }
    if (is.symbol(prev) && .is(nxt, "letter") && .is(p$toks[[p$i - 1L]], "letter")) {
      .assume(p, "letters", paste0("Juxtaposed letters `", .span(p, p$i - 1L, p$i),
                                   "` are a product of single-letter symbols"))
    }
    factors[[length(factors) + 1L]] <- .p_power(p)
  }
  p$last_n_factors <- length(factors)
  Reduce(function(a, b) call("*", a, b), factors)
}

.p_unary <- function(p) {
  tok <- .peek(p)
  if (.is(tok, "op", "-")) {
    .advance(p)
    return(call("-", .p_unary(p)))
  }
  if (.is(tok, "op", "+")) {
    .advance(p)
    return(.p_unary(p))
  }
  .p_power(p)
}

.p_power <- function(p) {
  base <- .p_postfix(p)
  tok <- .peek(p)
  if (.is(tok, "op", "'")) .parse_error("prime (derivative) notation is not supported", tok$start)
  if (.is(tok, "op", "_")) .parse_error("subscript on a non-symbol", tok$start)
  if (.is(tok, "op", "^")) {
    .advance(p)
    expo <- .p_script(p)
    if (.is(.peek(p), "op", "^")) .parse_error("double superscript", .peek(p)$start)
    if (isTRUE(attr(base, "euler"))) return(call("exp", expo))
    return(call("^", .strip_attr(base), expo))
  }
  .strip_attr(base)
}

.strip_attr <- function(e) {
  if (!is.null(attributes(e))) attributes(e) <- NULL
  e
}

.p_postfix <- function(p) {
  x <- .p_primary(p)
  if (.is(.peek(p), "op", "!")) {
    .advance(p)
    x <- call("factorial", .strip_attr(x))
  }
  x
}

# Argument of ^ or _ (and of an unbraced \frac): a group, or a single token.
.p_script <- function(p) {
  tok <- .peek(p)
  if (.is(tok, "lbrace")) return(.p_group(p))
  if (.is(tok, "op", "-")) {
    .advance(p)
    return(call("-", .p_script(p)))
  }
  if (.is(tok, "num")) return(.single_digit(p))
  if (.is(tok, "letter")) {
    .advance(p)
    return(as.name(tok$value))
  }
  if (.is(tok, "cmd", .greek)) {
    .advance(p)
    return(as.name(tok$value))
  }
  if (.is(tok, "cmd", c("frac", "dfrac", "tfrac", "sqrt"))) return(.p_primary(p))
  .parse_error("unsupported superscript/subscript argument", if (!is.null(tok)) tok$start)
}

# LaTeX takes one character from an unbraced digit run: x^23 is x^2 * 3.
.single_digit <- function(p) {
  tok <- .peek(p)
  if (nchar(tok$value) == 1L) {
    .advance(p)
    return(as.numeric(tok$value))
  }
  first <- substr(tok$value, 1L, 1L)
  rest <- substr(tok$value, 2L, nchar(tok$value))
  if (startsWith(rest, ".")) .parse_error("unbraced decimal in a script; use braces", tok$start)
  p$toks[[p$i]]$value <- rest
  p$toks[[p$i]]$start <- tok$start + 1L
  as.numeric(first)
}

.p_group <- function(p) {
  .expect(p, "lbrace", what = "'{'")
  e <- .p_expr(p)
  .expect(p, "rbrace", what = "'}'")
  e
}

.p_frac_arg <- function(p) {
  if (.is(.peek(p), "lbrace")) .p_group(p) else .p_script(p)
}

.p_primary <- function(p) {
  tok <- .advance(p)
  switch(tok$type,
    num = as.numeric(tok$value),
    letter = .p_letter(p, tok),
    name = .p_name(p, tok),
    lbrace = {
      e <- .p_expr(p)
      .expect(p, "rbrace", what = "'}'")
      e
    },
    op = {
      if (tok$value == "(") {
        e <- .p_expr(p)
        .expect(p, "op", ")")
        e
      } else if (tok$value == "[") {
        e <- .p_expr(p)
        .expect(p, "op", "]")
        e
      } else if (tok$value == "|") {
        p$abs_depth <- p$abs_depth + 1L
        e <- .p_expr(p)
        .expect(p, "op", "|", "closing '|'")
        p$abs_depth <- p$abs_depth - 1L
        call("abs", e)
      } else {
        .parse_error(paste0("unexpected '", tok$value, "'"), tok$start)
      }
    },
    cmd = .p_command(p, tok),
    .parse_error(paste0("unexpected '", tok$value, "'"), tok$start))
}

.p_command <- function(p, tok) {
  v <- tok$value
  if (v %in% .greek) return(.p_greek(p, tok))
  if (v %in% names(.fun_cmds)) return(.p_fun_app(p, .fun_cmds[[v]], v))
  switch(v,
    frac = , dfrac = , tfrac = {
      if (.is(.peek(p), "lbrace") && .is(.peek(p, 1L), "cmd", "partial")) {
        .parse_error("Leibniz derivative notation is not supported in the input", tok$start)
      }
      a <- .p_frac_arg(p)
      b <- .p_frac_arg(p)
      call("/", a, b)
    },
    sqrt = {
      if (.is(.peek(p), "op", "[")) {
        .advance(p)
        idx <- .p_expr(p)
        .expect(p, "op", "]")
        call("^", .p_frac_arg(p), call("/", 1, idx))
      } else {
        call("sqrt", .p_frac_arg(p))
      }
    },
    left = .p_left(p, tok),
    lvert = {
      e <- .p_expr(p)
      .expect(p, "cmd", "rvert", "\\rvert")
      call("abs", e)
    },
    "{" = {
      e <- .p_expr(p)
      .expect(p, "cmd", "}", "\\}")
      e
    },
    sum = , prod = .p_bigop(p, v, tok),
    binom = {
      n <- .p_frac_arg(p)
      k <- .p_frac_arg(p)
      call("choose", n, k)
    },
    begin = .parse_error("environments are only supported as the whole formula (a vector)", tok$start),
    infty = .parse_error("\\infty is not supported", tok$start),
    partial = .parse_error("\\partial is only supported inside a numeric-partial node", tok$start),
    .parse_error(paste0("unsupported command \\", v), tok$start))
}

# \left<delim> ... \right<delim>
.p_left <- function(p, tok) {
  d <- .advance(p)
  close_with <- function(type, value) {
    .expect(p, "cmd", "right", "\\right")
    .expect(p, type, value)
  }
  if (.is(d, "op", "(")) {
    e <- .p_expr(p); close_with("op", ")"); return(e)
  }
  if (.is(d, "op", "[")) {
    e <- .p_expr(p); close_with("op", "]"); return(e)
  }
  if (.is(d, "cmd", "{")) {
    e <- .p_expr(p); close_with("cmd", "}"); return(e)
  }
  if (.is(d, "op", "|") || .is(d, "cmd", "lvert")) {
    saved <- p$abs_depth
    p$abs_depth <- 0L
    e <- .p_expr(p)
    p$abs_depth <- saved
    .expect(p, "cmd", "right", "\\right")
    .expect(p, if (.is(d, "op", "|")) "op" else "cmd", if (.is(d, "op", "|")) "|" else "rvert")
    return(call("abs", e))
  }
  if (.is(d, "op", ".")) return(.p_numeric_partial(p))
  .parse_error(paste0("unsupported \\left delimiter '", d$value, "'"), d$start)
}

# Comma-separated arguments in (...), [...] or \left( ... \right).
.p_call_args <- function(p) {
  tok <- .advance(p)
  if (.is(tok, "cmd", "left")) {
    d <- .expect(p, "op", c("(", "["), "'(' after \\left")
    close <- if (d$value == "(") ")" else "]"
    args <- .p_arg_list(p)
    .expect(p, "cmd", "right", "\\right")
    .expect(p, "op", close)
    return(args)
  }
  close <- if (tok$value == "(") ")" else "]"
  args <- .p_arg_list(p)
  .expect(p, "op", close)
  args
}

.p_arg_list <- function(p) {
  args <- list(.p_expr(p))
  while (.is(.peek(p), "op", ",")) {
    .advance(p)
    args[[length(args) + 1L]] <- .p_expr(p)
  }
  args
}

.check_arity <- function(fname, args, tok_start = NULL) {
  want <- .named_fun_arity[fname]
  if (!is.na(want) && length(args) != want) {
    .parse_error(paste0(fname, "() takes ", want, " argument(s), got ", length(args)), tok_start)
  }
  invisible(TRUE)
}

# Function application after a function command (\sin, \Gamma, ...).
.p_fun_app <- function(p, fname, shown = fname) {
  start <- p$i - 1L
  pow <- NULL
  if (.is(.peek(p), "op", "^")) {
    .advance(p)
    s <- .p_script(p)
    if (identical(s, quote(-1))) {
      inv <- .inverse_fun[shown]
      if (is.na(inv)) .parse_error(paste0("no inverse known for \\", shown))
      .assume(p, "inverse_fn", paste0("`\\", shown, "^{-1}` is the inverse function ", inv, ", not a reciprocal"))
      fname <- unname(inv)
    } else {
      pow <- s
      .assume(p, "fn_power", paste0("`\\", shown, "^{", paste(deparse(s), collapse = ""),
                                    "} x` is (\\", shown, " x)^{", paste(deparse(s), collapse = ""), "}"))
    }
  }
  base <- NULL
  if (fname == "log" && .is(.peek(p), "op", "_")) {
    .advance(p)
    base <- .p_script(p)
  }
  nxt <- .peek(p)
  if (.opens_paren(nxt)) {
    args <- .p_call_args(p)
  } else if (.is(nxt, "lbrace")) {
    args <- list(.p_group(p))
  } else {
    args <- list(.p_fn_chain(p, shown, start))
  }
  .check_arity(fname, args)
  e <- as.call(c(as.name(fname), args))
  if (!is.null(base)) e <- call("/", call("log", args[[1L]]), call("log", base))
  if (!is.null(pow)) e <- call("^", e, pow)
  e
}

# Unbraced function argument: absorbs the following implicit product of
# simple factors (`\sin 2x y` -> sin(2 x y)); stops at another function,
# a parenthesis, or any operator.
.p_fn_chain <- function(p, shown, start) {
  factors <- list(.p_unary(p))
  repeat {
    tok <- .peek(p)
    simple <- .is(tok, "num") || .is(tok, "lbrace") ||
      (.is(tok, "letter") && is.null(.letter_function_at(p, p$i))) ||
      (.is(tok, "name") && !(tok$value %in% names(.named_fun_arity))) ||
      .is(tok, "cmd", c(setdiff(.greek, c("Gamma", "Phi")), "frac", "dfrac", "tfrac", "sqrt"))
    if (!simple) break
    factors[[length(factors) + 1L]] <- .p_power(p)
  }
  if (length(factors) > 1L) {
    .assume(p, "arg_scope", paste0("`", .span(p, start, p$i - 1L), "`: the unbraced argument of \\",
                                   shown, " extends over the whole implicit product"))
  }
  Reduce(function(a, b) call("*", a, b), factors)
}

# Plain letters spelling a function name at token j (e.g. `sin x`).
.letter_function_at <- function(p, j) {
  for (w in .letter_functions) {
    L <- nchar(w)
    if (j + L - 1L > p$n) next
    toks <- p$toks[j:(j + L - 1L)]
    if (!all(vapply(toks, function(t) identical(t$type, "letter"), TRUE))) next
    if (paste(vapply(toks, `[[`, "", "value"), collapse = "") != w) next
    adjacent <- all(diff(vapply(toks, `[[`, 0L, "start")) == 1L)
    if (!adjacent) next
    after <- if (j + L <= p$n) p$toks[[j + L]] else NULL
    if (.is(after, "letter") && after$start == toks[[L]]$end + 1L) next
    return(w)
  }
  NULL
}

# Subscript after '_': returns the name part (or NULL when the subscript is
# not a plain name) and the token range of its content.
.p_subscript <- function(p) {
  .expect(p, "op", "_")
  if (.is(.peek(p), "lbrace")) {
    open <- p$i
    .advance(p)
    depth <- 1L
    from <- p$i
    while (depth > 0L) {
      t <- .advance(p)
      if (.is(t, "lbrace")) depth <- depth + 1L
      if (.is(t, "rbrace")) depth <- depth - 1L
    }
    to <- p$i - 2L
    if (to < from) .parse_error("empty subscript", p$toks[[open]]$start)
    parts <- character()
    ok <- TRUE
    for (t in p$toks[from:to]) {
      if (.is(t, "letter") || .is(t, "num")) {
        parts <- c(parts, t$value)
      } else if (.is(t, "cmd", .greek)) {
        parts <- c(parts, t$value)
      } else if (.is(t, "op", ",")) {
        parts <- c(parts, ",")
      } else {
        ok <- FALSE
      }
    }
    return(list(name = if (ok) paste(parts, collapse = "") else NULL,
                parts = if (ok) parts else NULL, range = c(from, to), last = p$i - 1L))
  }
  tok <- .peek(p)
  if (.is(tok, "num")) {
    d <- .single_digit(p)
    return(list(name = as.character(d), parts = as.character(d), range = c(p$i - 1L, p$i - 1L),
                last = p$i - 1L))
  }
  if (.is(tok, "letter") || .is(tok, "cmd", .greek)) {
    .advance(p)
    return(list(name = tok$value, parts = tok$value, range = c(p$i - 1L, p$i - 1L),
                last = p$i - 1L))
  }
  .parse_error("unsupported subscript", if (!is.null(tok)) tok$start)
}

.make_symbol <- function(p, base, sub) {
  if (is.null(sub)) return(as.name(base))
  if (is.null(sub$name)) .parse_error(paste0("unsupported subscript on '", base, "'"))
  name <- paste0(base, "_", sub$name)
  p$meta[[name]] <- list(base = base, parts = sub$parts)
  as.name(name)
}

.is_minus_one_script <- function(p) {
  # ^{-1} or ^-1 at the current position (after '^' has NOT been consumed)
  .is(.peek(p), "op", "^") &&
    ((.is(.peek(p, 1L), "lbrace") && .is(.peek(p, 2L), "op", "-") &&
        .is(.peek(p, 3L), "num", "1") && .is(.peek(p, 4L), "rbrace")) ||
       (.is(.peek(p, 1L), "op", "-") && .is(.peek(p, 2L), "num", "1")))
}

.p_letter <- function(p, tok) {
  j <- p$i - 1L
  w <- .letter_function_at(p, j)
  if (!is.null(w)) {
    p$i <- j + nchar(w)
    .assume(p, "plain_function", paste0("Letters `", w, "` written without a backslash are the function \\", w))
    return(.p_fun_app(p, .fun_cmds[[w]], w))
  }
  ch <- tok$value
  sub <- if (.is(.peek(p), "op", "_")) .p_subscript(p) else NULL
  # Student-t quantile t_{df}^{-1}(u)
  if (ch == "t" && !is.null(sub) && .is_minus_one_script(p)) {
    .advance(p)
    if (.is(.peek(p), "lbrace")) {
      .advance(p); .advance(p); .advance(p); .advance(p)
    } else {
      .advance(p); .advance(p)
    }
    if (!.opens_paren(.peek(p))) .parse_error("t_{df}^{-1} must be applied to a parenthesized argument")
    df <- .p_tokens_expr(p, sub$range)
    args <- .p_call_args(p)
    if (length(args) != 1L) .parse_error("t_{df}^{-1}() takes one argument")
    .assume(p, "t_quantile", paste0("`", .span(p, j, sub$last),
                                    "^{-1}(...)` is the Student-t quantile function qt(u, df)"))
    return(call("qt", args[[1L]], df))
  }
  if (ch == "e" && is.null(sub) && !("e" %in% p$vars)) {
    .assume(p, "euler", "`e` is Euler's number (declare `vars = \"e\"` to make it a variable)")
    return(structure(quote(exp(1)), euler = TRUE))
  }
  sym <- .make_symbol(p, ch, sub)
  .maybe_declared_function(p, sym)
}

.p_greek <- function(p, tok) {
  v <- tok$value
  if (v == "Gamma" && (.opens_paren(.peek(p)) || .is(.peek(p), "lbrace"))) {
    return(.p_fun_app(p, "gamma", "Gamma"))
  }
  if (v == "Phi" && .opens_paren(.peek(p)) && !("Phi" %in% names(p$functions))) {
    .assume(p, "phi_cdf", "`\\Phi(...)` is the standard normal CDF pnorm()")
    return(.p_fun_app(p, "pnorm", "Phi"))
  }
  sub <- if (.is(.peek(p), "op", "_")) .p_subscript(p) else NULL
  sym <- .make_symbol(p, v, sub)
  .maybe_declared_function(p, sym)
}

.maybe_declared_function <- function(p, sym) {
  nm <- as.character(sym)
  if (nm %in% names(p$functions) && .opens_paren(.peek(p))) {
    args <- .p_call_args(p)
    return(as.call(c(as.name(p$functions[[nm]]), args)))
  }
  sym
}

# \mathrm{...} / \operatorname{...} / \mathit{...} / \text{...}
.p_name <- function(p, tok) {
  v <- tok$value
  if (v %in% names(.fun_cmds) && tok$kind %in% c("operatorname", "mathrm")) {
    return(.p_fun_app(p, .fun_cmds[[v]], v))
  }
  if (v %in% names(.named_fun_arity) && tok$kind %in% c("operatorname", "mathrm")) {
    if (!.opens_paren(.peek(p))) .parse_error(paste0(v, " must be applied to a parenthesized argument"), tok$start)
    args <- .p_call_args(p)
    .check_arity(v, args, tok$start)
    return(as.call(c(as.name(v), args)))
  }
  if (v %in% p$functions && tok$kind %in% c("operatorname", "mathrm")) {
    if (!.opens_paren(.peek(p))) .parse_error(paste0(v, " must be applied to a parenthesized argument"), tok$start)
    return(as.call(c(as.name(v), .p_call_args(p))))
  }
  if (tok$kind == "operatorname") {
    .parse_error(paste0("unknown function \\operatorname{", v, "}"), tok$start)
  }
  if (tok$kind == "mathrm" && v == "e") {
    return(structure(quote(exp(1)), euler = TRUE))
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9]*$", v)) .parse_error(paste0("unsupported name '", v, "'"), tok$start)
  sub <- if (.is(.peek(p), "op", "_")) .p_subscript(p) else NULL
  .maybe_declared_function(p, .make_symbol(p, v, sub))
}

# Parse a token range (e.g. a subscript) as an expression with a sub-parser.
.p_tokens_expr <- function(p, range) {
  q <- new.env(parent = emptyenv())
  for (nm in ls(p, all.names = TRUE)) assign(nm, get(nm, envir = p), envir = q)
  q$toks <- p$toks[range[1]:range[2]]
  q$n <- length(q$toks)
  q$i <- 1L
  e <- .p_expr(q)
  if (q$i <= q$n) .parse_error("could not parse subscript expression")
  p$assumptions <- q$assumptions
  p$meta <- q$meta
  e
}

# \sum_{i=a}^{b} body / \prod_{i=a}^{b} body with integer bounds: expanded.
.p_bigop <- function(p, op, tok) {
  idx <- NULL; lower <- NULL; upper <- NULL
  for (k in 1:2) {
    t <- .peek(p)
    if (.is(t, "op", "_") && is.null(idx)) {
      .advance(p)
      if (.is(.peek(p), "lbrace")) {
        .advance(p)
        it <- .advance(p)
        if (!(.is(it, "letter") || .is(it, "cmd", .greek))) .parse_error("summation index must be a symbol", it$start)
        idx <- it$value
        .expect(p, "op", "=", "'=' in the summation subscript")
        lower <- .p_expr(p)
        .expect(p, "rbrace", what = "'}'")
      } else {
        .parse_error(paste0("\\", op, " needs explicit bounds _{i=a}^{b}"), t$start)
      }
    } else if (.is(t, "op", "^") && is.null(upper)) {
      .advance(p)
      upper <- .p_script(p)
    }
  }
  if (is.null(idx) || is.null(upper)) .parse_error(paste0("\\", op, " needs explicit bounds _{i=a}^{b}"), tok$start)
  lo <- .num_value(normalize_tree(lower))
  hi <- .num_value(normalize_tree(upper))
  if (is.null(lo) || is.null(hi) || lo != round(lo) || hi != round(hi)) {
    .parse_error(paste0("\\", op, " with symbolic bounds is not supported; give integer bounds"), tok$start)
  }
  if (hi - lo > 1000) .parse_error(paste0("\\", op, " bounds span more than 1000 terms"), tok$start)
  start <- p$i
  body <- .p_term(p)
  if (hi < lo) return(if (op == "sum") 0 else 1)
  terms <- lapply(seq(lo, hi), function(k) .subst_index(p, body, idx, k))
  .assume(p, "bigop_expand", paste0("Finite \\", op, " over ", idx, " = ", lo, ", ..., ", hi,
                                    " expanded into ", length(terms), " explicit terms"))
  Reduce(function(a, b) call(if (op == "sum") "+" else "*", a, b), terms)
}

.subst_index <- function(p, expr, idx, k) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm == idx) return(as.numeric(k))
    m <- p$meta[[nm]]
    if (!is.null(m) && idx %in% m$parts) {
      parts <- m$parts
      parts[parts == idx] <- as.character(k)
      new <- paste0(m$base, "_", paste(parts, collapse = ""))
      p$meta[[new]] <- list(base = m$base, parts = parts)
      return(as.name(new))
    }
    return(expr)
  }
  if (is.call(expr)) {
    for (j in seq_along(expr)[-1L]) expr[[j]] <- .subst_index(p, expr[[j]], idx, k)
  }
  expr
}

# \begin{pmatrix} a \\ b \end{pmatrix} -> c(a, b). Vectors only.
.p_vector <- function(p) {
  .advance(p)
  env <- .p_env_name(p)
  if (!(env %in% c("pmatrix", "bmatrix"))) .parse_error(paste0("unsupported environment '", env, "'"))
  cells <- list()
  seps <- character()
  repeat {
    if (.is(.peek(p), "cmd", "end")) break
    cells[[length(cells) + 1L]] <- .p_expr(p)
    t <- .peek(p)
    if (.is(t, "op", "&")) {
      .advance(p); seps <- c(seps, "&")
    } else if (.is(t, "cmd", "\\")) {
      .advance(p); seps <- c(seps, "\\")
    } else if (!.is(t, "cmd", "end")) {
      .parse_error("expected '&', '\\\\' or \\end in the vector", if (!is.null(t)) t$start)
    }
  }
  .advance(p)
  end_env <- .p_env_name(p)
  if (end_env != env) .parse_error(paste0("\\begin{", env, "} closed by \\end{", end_env, "}"))
  if (length(unique(seps)) > 1L) .parse_error("matrices are not supported; write a row or a column vector")
  if (length(cells) < 1L) .parse_error("empty vector")
  as.call(c(as.name("c"), cells))
}

.p_env_name <- function(p) {
  .expect(p, "lbrace", what = "'{'")
  chars <- character()
  while (!.is(.peek(p), "rbrace")) chars <- c(chars, .advance(p)$value)
  .advance(p)
  paste(chars, collapse = "")
}

# \left.\frac{\partial^{k}}{\partial v_1 \partial v_2}\left[ g \right]\right|_{\mathrm{num}}
.p_numeric_partial <- function(p) {
  .expect(p, "cmd", c("frac", "dfrac"), "\\frac in a numeric-partial node")
  .expect(p, "lbrace", what = "'{'")
  .expect(p, "cmd", "partial", "\\partial")
  if (.is(.peek(p), "op", "^")) {
    .advance(p)
    .p_script(p)
  }
  .expect(p, "rbrace", what = "'}'")
  .expect(p, "lbrace", what = "'{'")
  vars <- character()
  while (.is(.peek(p), "cmd", "partial")) {
    .advance(p)
    t <- .advance(p)
    base <- if (.is(t, "letter") || .is(t, "cmd", .greek)) t$value else
      if (.is(t, "name")) t$value else .parse_error("expected a variable after \\partial", t$start)
    sub <- if (.is(.peek(p), "op", "_")) .p_subscript(p) else NULL
    vars <- c(vars, as.character(.make_symbol(p, base, sub)))
  }
  .expect(p, "rbrace", what = "'}'")
  if (!length(vars)) .parse_error("numeric-partial node without variables")
  .expect(p, "cmd", "left", "\\left[")
  .expect(p, "op", "[")
  g <- .p_expr(p)
  .expect(p, "cmd", "right", "\\right]")
  .expect(p, "op", "]")
  .expect(p, "cmd", "right", "\\right|")
  .expect(p, "op", "|")
  .expect(p, "op", "_")
  .expect(p, "lbrace", what = "'{'")
  tag <- .expect(p, "name", what = "\\mathrm{num}")
  if (tag$value != "num") .parse_error("numeric-partial node must be tagged \\mathrm{num}", tag$start)
  .expect(p, "rbrace", what = "'}'")
  as.call(c(as.name("numeric_partial"), g, as.list(vars)))
}
