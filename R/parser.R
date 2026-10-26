## parser.R
## Recursive-descent parser: LaTeX math -> R call. Every reading the grammar
## had to choose (implicit multiplication, scope of `\sin x y`, `e` as Euler's
## number, ...) is recorded as an *ambiguity* with its options and the option
## taken; the choice can be overridden by a resolver (e.g. an LLM at the
## entry) through `choices`, keyed by ambiguity id. Readings that involve no
## choice but change the surface form (`\log_2 x` -> `log(x) / log(2)`) are
## recorded as *notes*.

#' Parse LaTeX math into an R expression
#'
#' Parses a LaTeX formula (optionally an equation `f(x, y) = ...`) into an R
#' call. The grammar is deterministic; wherever the input admits more than one
#' reading, the default reading is taken and recorded in `ambiguities`, and a
#' caller-supplied `choices` list can select another option.
#'
#' @param latex A single string of LaTeX math (no surrounding `$` needed).
#' @param choices Named list mapping ambiguity ids to the chosen option label,
#'   as produced by a resolver. Unknown ids are ignored.
#' @return A list with elements `expr` (the R call, not yet normalized),
#'   `signature` (`NULL`, or a list with `name`, `vars`, `params`, `lhs`),
#'   `ambiguities` and `notes` (lists of records).
#' @export
#' @examples
#' tex_parse("f(x, y) = x^2 \\sin y")$expr
#' tex_parse("\\frac{1}{\\sqrt{2\\pi}} e^{-x^2/2}")$expr
tex_parse <- function(latex, choices = list()) {
  if (!is.character(latex) || length(latex) != 1L || is.na(latex)) {
    .l2g_stop("parse", "`latex` must be a single string.")
  }
  toks <- .l2g_lex(latex)
  P <- new.env(parent = emptyenv())
  P$src <- latex
  P$choices <- choices
  P$ambiguities <- list()
  P$notes <- list()
  P$abs_depth <- 0L
  P$in_integral <- 0L
  P$run_until <- 0L
  P$declared <- character(0)

  split <- .l2g_split_equation(toks)
  signature <- NULL
  if (!is.null(split$lhs)) {
    signature <- .l2g_parse_signature(split$lhs, latex)
    if (is.null(signature$name)) {
      .l2g_note(P, "lhs", paste0("Left-hand side `", signature$lhs,
                                 "` is not of the form f(x, ...); kept as a label only."))
    }
    P$declared <- c(signature$vars, signature$params)
  }
  P$toks <- split$rhs
  P$i <- 1L

  if (identical(.l2g_peek(P)$type, "begin")) {
    expr <- .l2g_parse_matrix(P)
  } else {
    expr <- .l2g_parse_expr(P)
  }
  tok <- .l2g_peek(P)
  if (tok$type != "eof") {
    .l2g_stop("parse", paste0("unexpected `", .l2g_tok_text(tok), "` at position ",
                              tok$pos, " (input not fully consumed)"))
  }
  list(expr = expr, signature = signature,
       ambiguities = P$ambiguities, notes = P$notes)
}

# ---- token cursor ---------------------------------------------------------

.l2g_peek <- function(P, k = 0L) {
  j <- P$i + k
  if (j > length(P$toks)) return(P$toks[[length(P$toks)]])
  P$toks[[j]]
}
.l2g_advance <- function(P) {
  tok <- .l2g_peek(P)
  P$i <- P$i + 1L
  tok
}
.l2g_is <- function(tok, type, value = NULL) {
  identical(tok$type, type) && (is.null(value) || tok$value %in% value)
}
.l2g_expect <- function(P, type, value = NULL, what = NULL) {
  tok <- .l2g_peek(P)
  if (!.l2g_is(tok, type, value)) {
    want <- if (!is.null(what)) what else if (!is.null(value)) paste0("`", value[[1L]], "`") else type
    .l2g_stop("parse", paste0("expected ", want, " but found `", .l2g_tok_text(tok),
                              "` at position ", tok$pos))
  }
  .l2g_advance(P)
}
.l2g_tok_text <- function(tok) {
  switch(tok$type,
         eof = "end of input",
         cmd = paste0("\\", tok$value),
         name = paste0("\\", tok$cmd, "{", tok$value, "}"),
         left = paste0("\\left", tok$value),
         right = paste0("\\right", tok$value),
         begin = paste0("\\begin{", tok$value, "}"),
         end = paste0("\\end{", tok$value, "}"),
         tok$value)
}

# Source text from token k (absolute index) up to (excluding) token m.
.l2g_snippet <- function(P, k, m) {
  a <- P$toks[[k]]$pos
  b <- if (m <= length(P$toks)) P$toks[[m]]$pos - 1L else nchar(P$src)
  trimws(substr(P$src, a, b))
}

# ---- ambiguity / note records ---------------------------------------------

# Record an ambiguity and return the chosen option label.
.l2g_decide <- function(P, id, kind, text, options, default = options[[1L]]) {
  chosen <- default
  source <- "grammar default"
  ch <- P$choices[[id]]
  if (!is.null(ch) && ch %in% options) {
    chosen <- ch
    source <- "resolver"
  }
  P$ambiguities[[length(P$ambiguities) + 1L]] <- list(
    id = id, kind = kind, text = text, options = options,
    default = default, chosen = chosen, source = source)
  chosen
}

.l2g_note <- function(P, kind, text) {
  P$notes[[length(P$notes) + 1L]] <- list(kind = kind, text = text)
  invisible(NULL)
}

# ---- equation / signature -------------------------------------------------

.l2g_split_equation <- function(toks) {
  depth <- 0L
  eq <- integer(0)
  for (k in seq_along(toks)) {
    t <- toks[[k]]
    if (t$type == "op" && t$value %in% c("(", "[", "{")) depth <- depth + 1L
    if (t$type == "op" && t$value %in% c(")", "]", "}")) depth <- depth - 1L
    if (t$type %in% c("lbrace", "left", "begin")) depth <- depth + 1L
    if (t$type %in% c("rbrace", "right", "end")) depth <- depth - 1L
    if (t$type == "op" && t$value == "=" && depth == 0L) eq <- c(eq, k)
  }
  eof <- toks[[length(toks)]]
  if (length(eq) == 0L) return(list(lhs = NULL, rhs = toks))
  if (length(eq) > 1L) {
    .l2g_stop("parse", "more than one top-level `=`; pass a single formula or equation.")
  }
  k <- eq[[1L]]
  if (k == 1L) .l2g_stop("parse", "empty left-hand side before `=`")
  lhs <- c(toks[seq_len(k - 1L)], list(eof))
  rhs <- toks[(k + 1L):length(toks)]
  if (length(rhs) == 1L) .l2g_stop("parse", "empty right-hand side after `=`")
  list(lhs = lhs, rhs = rhs)
}

# LHS of the form  name(v1, v2; p1, p2)  or a bare name. Returns a list with
# `name` NULL when the LHS has another shape.
.l2g_parse_signature <- function(lhs, src) {
  a <- lhs[[1L]]$pos
  b <- lhs[[length(lhs) - 1L]]$pos
  last <- lhs[[length(lhs) - 1L]]
  b <- b + nchar(.l2g_tok_text(last)) - 1L
  lhs_tex <- trimws(substr(src, a, b))
  Q <- new.env(parent = emptyenv())
  Q$toks <- lhs
  Q$i <- 1L
  out <- list(name = NULL, vars = character(0), params = character(0), lhs = lhs_tex)
  name <- tryCatch(.l2g_read_symbol_name(Q), latex2grad_error = function(e) NULL)
  if (is.null(name)) return(out)
  if (.l2g_peek(Q)$type == "eof") { out$name <- name; return(out) }
  open <- .l2g_peek(Q)
  if (!(.l2g_is(open, "op", "(") || .l2g_is(open, "left", "("))) return(out)
  .l2g_advance(Q)
  vars <- character(0); params <- character(0); in_params <- FALSE
  repeat {
    nm <- tryCatch(.l2g_read_symbol_name(Q), latex2grad_error = function(e) NULL)
    if (is.null(nm)) return(out)
    if (in_params) params <- c(params, nm) else vars <- c(vars, nm)
    t <- .l2g_advance(Q)
    if (.l2g_is(t, "op", ",")) next
    if (.l2g_is(t, "op", ";") || .l2g_is(t, "op", "|") || .l2g_is(t, "cmd", "mid")) {
      if (in_params) return(out)
      in_params <- TRUE
      next
    }
    if (.l2g_is(t, "op", ")") || .l2g_is(t, "right", ")")) break
    return(out)
  }
  if (.l2g_peek(Q)$type != "eof") return(out)
  out$name <- name; out$vars <- vars; out$params <- params
  out
}

# A variable name: letter / Greek / \mathrm{..} with an optional subscript.
.l2g_read_symbol_name <- function(P) {
  tok <- .l2g_peek(P)
  if (tok$type == "ident") {
    base <- tok$value
  } else if (tok$type == "cmd" && tok$value %in% .l2g_greek) {
    base <- tok$value
  } else if (tok$type == "name" && tok$cmd != "operatorname") {
    base <- tok$value
  } else {
    .l2g_stop("parse", paste0("expected a variable name at position ", tok$pos))
  }
  .l2g_advance(P)
  if (.l2g_is(.l2g_peek(P), "op", "_")) {
    .l2g_advance(P)
    return(paste0(base, "_", .l2g_parse_subscript_text(P)))
  }
  base
}

# ---- expressions ------------------------------------------------------------

.l2g_parse_expr <- function(P) {
  left <- .l2g_parse_term(P)
  repeat {
    tok <- .l2g_peek(P)
    if (.l2g_is(tok, "op", c("+", "-"))) {
      .l2g_advance(P)
      right <- .l2g_parse_term(P)
      left <- call(tok$value, left, right)
    } else if (.l2g_is(tok, "cmd", "pm")) {
      .l2g_stop("parse", "`\\pm` denotes two formulas; pass each one separately.")
    } else {
      return(left)
    }
  }
}

.l2g_parse_term <- function(P) {
  left <- .l2g_parse_unary(P)
  repeat {
    tok <- .l2g_peek(P)
    if (.l2g_is(tok, "op", "*") || .l2g_is(tok, "cmd", c("cdot", "times", "ast"))) {
      .l2g_advance(P)
      left <- call("*", left, .l2g_parse_unary(P))
    } else if (.l2g_is(tok, "op", "/") || .l2g_is(tok, "cmd", "div")) {
      k <- P$i
      .l2g_advance(P)
      den <- .l2g_parse_unary(P)
      if (.l2g_starts_factor(P)) {
        m <- P$i
        while (m <= length(P$toks) && !(P$toks[[m]]$type %in% c("eof")) &&
               !.l2g_is(P$toks[[m]], "op", c("+", "-", ")", "=", ",")) &&
               !.l2g_is(P$toks[[m]], "right")) m <- m + 1L
        opt <- .l2g_decide(
          P, paste0("slash@", tok$pos), "slash-scope",
          paste0("`", .l2g_snippet(P, k - 1L, m), "`: scope of the denominator after `/`"),
          c("(a/b) c", "a/(b c)"))
        if (opt == "a/(b c)") {
          while (.l2g_starts_factor(P)) den <- call("*", den, .l2g_parse_power(P))
        }
      }
      left <- call("/", left, den)
    } else if (.l2g_starts_factor(P)) {
      left <- call("*", left, .l2g_parse_power(P))
    } else {
      return(left)
    }
  }
}

.l2g_parse_unary <- function(P) {
  tok <- .l2g_peek(P)
  if (.l2g_is(tok, "op", "-")) {
    .l2g_advance(P)
    return(call("-", .l2g_parse_unary(P)))
  }
  if (.l2g_is(tok, "op", "+")) {
    .l2g_advance(P)
    return(.l2g_parse_unary(P))
  }
  .l2g_parse_power(P)
}

# A primary with trailing superscripts / factorials.
.l2g_parse_power <- function(P) {
  base <- .l2g_parse_primary(P)
  repeat {
    tok <- .l2g_peek(P)
    if (.l2g_is(tok, "op", "^")) {
      .l2g_advance(P)
      base <- call("^", base, .l2g_parse_script_arg(P))
    } else if (.l2g_is(tok, "op", "!")) {
      .l2g_advance(P)
      .l2g_note(P, "factorial", "`n!` read as \\Gamma(n + 1).")
      base <- call("gamma", call("+", base, 1))
    } else if (.l2g_is(tok, "op", "'")) {
      .l2g_stop("parse", paste0("prime notation at position ", tok$pos,
                                " is not supported; name the derivative explicitly."))
    } else {
      return(base)
    }
  }
}

# TRUE when the next token can begin an implicitly multiplied factor.
.l2g_starts_factor <- function(P, allow_functions = TRUE) {
  tok <- .l2g_peek(P)
  if (P$in_integral > 0L && .l2g_is_differential(P, 0L)) return(FALSE)
  switch(tok$type,
    num = TRUE,
    ident = TRUE,
    name = allow_functions || tok$cmd != "operatorname",
    lbrace = TRUE,
    left = TRUE,
    cmd = {
      v <- tok$value
      if (v %in% c("frac", "dfrac", "tfrac", "cfrac", "sqrt", "pi", "infty",
                   "binom")) return(TRUE)
      if (v %in% .l2g_greek) {
        if (!allow_functions && v %in% names(.l2g_greek_funs) &&
            .l2g_followed_by_paren(P, 1L)) return(FALSE)
        return(TRUE)
      }
      if (v %in% c(names(.l2g_fun_cmds), "int", "sum", "prod", "max", "min")) {
        return(allow_functions)
      }
      FALSE
    },
    op = {
      if (tok$value %in% c("(", "[", "{")) return(TRUE)
      if (tok$value == "|") return(P$abs_depth == 0L)
      FALSE
    },
    FALSE)
}

.l2g_followed_by_paren <- function(P, k) {
  t <- .l2g_peek(P, k)
  .l2g_is(t, "op", "(") || .l2g_is(t, "left", "(")
}

# Argument of ^ / _ / \frac: a braced group or a single token (LaTeX takes
# one character of a multi-digit number: `x^23` is x^2 * 3).
.l2g_parse_script_arg <- function(P) {
  tok <- .l2g_peek(P)
  if (.l2g_is(tok, "op", "{")) {
    .l2g_advance(P)
    e <- .l2g_parse_expr(P)
    .l2g_expect(P, "op", "}")
    return(e)
  }
  if (tok$type == "num") {
    return(.l2g_take_digit(P))
  }
  if (tok$type == "ident") {
    .l2g_advance(P)
    if (tok$value == "e" && !("e" %in% P$declared)) return(.l2g_euler(P, tok))
    return(as.name(tok$value))
  }
  if (tok$type == "cmd") {
    if (tok$value %in% .l2g_greek) { .l2g_advance(P); return(as.name(tok$value)) }
    if (tok$value == "pi") { .l2g_advance(P); return(quote(pi)) }
    if (tok$value == "infty") { .l2g_advance(P); return(Inf) }
    if (tok$value %in% c("frac", "dfrac", "tfrac", "sqrt")) return(.l2g_parse_primary(P))
  }
  if (tok$type == "name") return(.l2g_parse_primary(P))
  if (.l2g_is(tok, "op", "-")) {
    .l2g_advance(P)
    .l2g_note(P, "script", "An unbraced `^-` was read as a negative exponent.")
    return(call("-", .l2g_parse_script_arg(P)))
  }
  .l2g_stop("parse", paste0("unsupported script argument `", .l2g_tok_text(tok),
                            "` at position ", tok$pos))
}

# Consume the first digit of the current number token (LaTeX script rule).
.l2g_take_digit <- function(P) {
  tok <- .l2g_peek(P)
  v <- tok$value
  if (nchar(v) == 1L) { .l2g_advance(P); return(as.numeric(v)) }
  P$toks[[P$i]]$value <- substring(v, 2L)
  P$toks[[P$i]]$pos <- tok$pos + 1L
  P$toks[[P$i]]$space <- FALSE
  as.numeric(substr(v, 1L, 1L))
}

# Subscript as name text: `_1`, `_{12}`, `_{ij}`, `_\nu`, `_{\max}`.
.l2g_parse_subscript_text <- function(P) {
  tok <- .l2g_peek(P)
  if (.l2g_is(tok, "op", "{")) {
    .l2g_advance(P)
    parts <- character(0)
    depth <- 1L
    repeat {
      t <- .l2g_advance(P)
      if (t$type == "eof") .l2g_stop("parse", "unterminated subscript")
      if (.l2g_is(t, "op", "{")) { depth <- depth + 1L; next }
      if (.l2g_is(t, "op", "}")) {
        depth <- depth - 1L
        if (depth == 0L) break
        next
      }
      parts <- c(parts, if (t$type == "name") t$value else t$value)
    }
    if (!length(parts)) .l2g_stop("parse", "empty subscript")
    return(paste(parts, collapse = ""))
  }
  if (tok$type == "num") return(format(.l2g_take_digit(P)))
  if (tok$type == "ident") { .l2g_advance(P); return(tok$value) }
  if (tok$type == "cmd" && tok$value %in% .l2g_greek) { .l2g_advance(P); return(tok$value) }
  if (tok$type == "name") { .l2g_advance(P); return(tok$value) }
  .l2g_stop("parse", paste0("unsupported subscript at position ", tok$pos))
}

.l2g_euler <- function(P, tok) {
  opt <- .l2g_decide(P, paste0("euler@", tok$pos), "constant",
                     "`e` read as Euler's number (not a variable)",
                     c("Euler's number", "variable e"))
  if (opt == "Euler's number") quote(exp(1)) else as.name("e")
}

# ---- primaries ------------------------------------------------------------

.l2g_parse_primary <- function(P) {
  tok <- .l2g_peek(P)
  switch(tok$type,
    num = { .l2g_advance(P); return(as.numeric(tok$value)) },
    ident = return(.l2g_parse_identifier(P)),
    name = return(.l2g_parse_name(P)),
    lbrace = {
      .l2g_advance(P)
      e <- .l2g_parse_expr(P)
      .l2g_expect(P, "rbrace", what = "`\\}`")
      return(e)
    },
    left = return(.l2g_parse_left(P)),
    begin = .l2g_stop("parse", paste0("`\\begin{", tok$value,
                                      "}` is only supported as the whole formula (a vector for jacobian)")),
    op = {
      if (tok$value %in% c("(", "[")) {
        close <- if (tok$value == "(") ")" else "]"
        .l2g_advance(P)
        e <- .l2g_parse_expr(P)
        .l2g_expect(P, "op", close)
        return(e)
      }
      if (tok$value == "{") {
        .l2g_advance(P)
        e <- .l2g_parse_expr(P)
        .l2g_expect(P, "op", "}")
        return(e)
      }
      if (tok$value == "|") {
        .l2g_advance(P)
        P$abs_depth <- P$abs_depth + 1L
        e <- .l2g_parse_expr(P)
        P$abs_depth <- P$abs_depth - 1L
        .l2g_expect(P, "op", "|", what = "closing `|`")
        return(call("abs", e))
      }
    },
    cmd = return(.l2g_parse_command(P)),
    NULL)
  .l2g_stop("parse", paste0("unexpected `", .l2g_tok_text(tok), "` at position ", tok$pos))
}

.l2g_parse_left <- function(P) {
  tok <- .l2g_advance(P)
  d <- tok$value
  if (d == ".") return(.l2g_parse_eval_bar(P, tok))
  close <- switch(d, "(" = ")", "[" = "]", "{" = "}", "|" = "|", "\\langle" = "\\rangle",
                  .l2g_stop("parse", paste0("unsupported delimiter \\left", d)))
  e <- .l2g_parse_expr(P)
  r <- .l2g_peek(P)
  if (!.l2g_is(r, "right")) {
    .l2g_stop("parse", paste0("expected \\right", close, " at position ", r$pos))
  }
  .l2g_advance(P)
  if (d == "|") return(call("abs", e))
  e
}

.l2g_parse_identifier <- function(P) {
  start <- P$i
  tok <- .l2g_peek(P)
  # Run of letters typed without spaces: `xy`, `sin`.
  run <- tok$value
  k <- 1L
  while (TRUE) {
    nx <- .l2g_peek(P, k)
    if (nx$type == "ident" && !isTRUE(nx$space)) {
      run <- paste0(run, nx$value)
      k <- k + 1L
    } else break
  }
  if (nchar(run) > 1L && P$i > P$run_until &&
      !(P$in_integral > 0L && tok$value == "d")) {
    fun_words <- c("sin", "cos", "tan", "exp", "log", "ln", "sqrt", "sinh",
                   "cosh", "tanh", "arcsin", "arccos", "arctan")
    if (run %in% fun_words) {
      opt <- .l2g_decide(P, paste0("word@", tok$pos), "function-name",
                         paste0("`", run, "` read as the function \\", run),
                         c("function", "product of letters"))
      if (opt == "function") {
        for (j in seq_len(nchar(run))) .l2g_advance(P)
        if (run == "sqrt") {
          args <- .l2g_parse_call_args(P)
          return(call("sqrt", args[[1L]]))
        }
        return(.l2g_apply_function_cmd(P, run, tok))
      }
    } else if (!any(c(run) %in% P$declared)) {
      opt <- .l2g_decide(P, paste0("letters@", tok$pos), "implicit-multiplication",
                         paste0("`", run, "` read as a product of single-letter variables"),
                         c("product of letters", "single name"))
      if (opt == "single name") {
        for (j in seq_len(nchar(run))) .l2g_advance(P)
        return(.l2g_finish_symbol(P, run))
      }
      P$run_until <- P$i + nchar(run) - 1L
    } else {
      for (j in seq_len(nchar(run))) .l2g_advance(P)
      return(.l2g_finish_symbol(P, run))
    }
  }
  .l2g_advance(P)
  L <- tok$value
  # Student-t quantile: t_{nu}^{-1}(p)
  if (L %in% c("t", "T") && .l2g_is(.l2g_peek(P), "op", "_")) {
    q <- .l2g_try_quantile(P, L, tok)
    if (!is.null(q)) return(q)
  }
  if (L == "e" && !.l2g_is(.l2g_peek(P), "op", "_") && !("e" %in% P$declared)) {
    return(.l2g_euler(P, tok))
  }
  sym <- .l2g_finish_symbol(P, L)
  name <- as.character(sym)
  if (.l2g_followed_by_paren(P, 0L) && !isTRUE(.l2g_peek(P)$space) &&
      !(name %in% P$declared)) {
    .l2g_decide(P, paste0("call@", tok$pos), "multiplication",
                paste0("`", name, "(...)` read as multiplication, not function application"),
                c("multiplication"))
  }
  sym
}

# Symbol with optional subscript.
.l2g_finish_symbol <- function(P, base) {
  if (.l2g_is(.l2g_peek(P), "op", "_")) {
    .l2g_advance(P)
    return(as.name(paste0(base, "_", .l2g_parse_subscript_text(P))))
  }
  as.name(base)
}

# `t_{nu}^{-1}(p)` -> qt(p, nu). Returns NULL (cursor untouched) on no match.
.l2g_try_quantile <- function(P, L, tok) {
  save <- P$i
  save_toks <- P$toks
  ok <- tryCatch({
    .l2g_advance(P)  # _
    df <- .l2g_parse_script_arg(P)
    if (!.l2g_is(.l2g_peek(P), "op", "^")) stop("no")
    .l2g_advance(P)
    ex <- .l2g_parse_script_arg(P)
    if (!identical(ex, call("-", 1))) stop("no")
    if (!.l2g_followed_by_paren(P, 0L)) stop("no")
    list(df = df)
  }, error = function(e) NULL)
  if (is.null(ok)) { P$i <- save; P$toks <- save_toks; return(NULL) }
  .l2g_decide(P, paste0("quantile@", tok$pos), "special-function",
              paste0("`", L, "_{\\cdot}^{-1}(\\cdot)` read as the Student-t quantile function"),
              c("Student-t quantile"))
  args <- .l2g_parse_call_args(P)
  if (length(args) != 1L) .l2g_stop("parse", "t-quantile takes one argument")
  call("qt", args[[1L]], ok$df)
}

.l2g_parse_command <- function(P) {
  tok <- .l2g_peek(P)
  v <- tok$value
  if (v %in% c("frac", "dfrac", "tfrac", "cfrac")) {
    .l2g_advance(P)
    num <- .l2g_parse_group_arg(P)
    den <- .l2g_parse_group_arg(P)
    return(call("/", num, den))
  }
  if (v == "binom") {
    .l2g_advance(P)
    n <- .l2g_parse_group_arg(P)
    k <- .l2g_parse_group_arg(P)
    return(call("choose", n, k))
  }
  if (v == "sqrt") {
    .l2g_advance(P)
    if (.l2g_is(.l2g_peek(P), "op", "[")) {
      .l2g_advance(P)
      idx <- .l2g_parse_expr(P)
      .l2g_expect(P, "op", "]")
      arg <- .l2g_parse_group_arg(P)
      return(call("^", arg, call("/", 1, idx)))
    }
    return(call("sqrt", .l2g_parse_group_arg(P)))
  }
  if (v == "pi") { .l2g_advance(P); return(quote(pi)) }
  if (v == "infty") { .l2g_advance(P); return(Inf) }
  if (v %in% .l2g_greek) return(.l2g_parse_greek(P))
  if (v %in% names(.l2g_fun_cmds)) {
    .l2g_advance(P)
    return(.l2g_apply_function_cmd(P, v, tok))
  }
  if (v %in% c("max", "min")) {
    .l2g_advance(P)
    args <- .l2g_parse_call_args(P)
    return(as.call(c(list(as.name(v)), args)))
  }
  if (v == "int") return(.l2g_parse_integral(P))
  if (v %in% c("sum", "prod")) return(.l2g_parse_bigop(P))
  if (v == "partial") {
    .l2g_stop("parse", paste0("derivative operators in the input are only supported in ",
                              "evaluation-bar form \\left.\\frac{\\partial}{\\partial s} f\\right|_{s=a}"))
  }
  .l2g_stop("parse", paste0("unsupported command \\", v, " at position ", tok$pos))
}

# `{...}` group or a single token (\frac12).
.l2g_parse_group_arg <- function(P) {
  tok <- .l2g_peek(P)
  if (.l2g_is(tok, "op", "{")) {
    .l2g_advance(P)
    e <- .l2g_parse_expr(P)
    .l2g_expect(P, "op", "}")
    return(e)
  }
  .l2g_parse_script_arg(P)
}

.l2g_parse_greek <- function(P) {
  tok <- .l2g_advance(P)
  g <- tok$value
  declared <- g %in% P$declared
  # \Phi^{-1}(p)
  if (g == "Phi" && !declared && .l2g_is(.l2g_peek(P), "op", "^")) {
    save <- P$i; save_toks <- P$toks
    .l2g_advance(P)
    ex <- tryCatch(.l2g_parse_script_arg(P), latex2grad_error = function(e) NULL)
    if (identical(ex, call("-", 1)) && .l2g_followed_by_paren(P, 0L)) {
      .l2g_decide(P, paste0("greekfun@", tok$pos), "special-function",
                  "`\\Phi^{-1}(\\cdot)` read as the standard normal quantile function",
                  c("function", "variable"))
      args <- .l2g_parse_call_args(P)
      return(call("qnorm", args[[1L]]))
    }
    P$i <- save; P$toks <- save_toks
  }
  # \psi_1(x) / \psi_{1}(x): trigamma
  if (g == "psi" && !declared && .l2g_is(.l2g_peek(P), "op", "_")) {
    save <- P$i; save_toks <- P$toks
    .l2g_advance(P)
    sub <- tryCatch(.l2g_parse_subscript_text(P), latex2grad_error = function(e) NULL)
    if (identical(sub, "1") && .l2g_followed_by_paren(P, 0L)) {
      .l2g_decide(P, paste0("greekfun@", tok$pos), "special-function",
                  "`\\psi_{1}(\\cdot)` read as the trigamma function",
                  c("function", "variable"))
      args <- .l2g_parse_call_args(P)
      return(call("trigamma", args[[1L]]))
    }
    P$i <- save; P$toks <- save_toks
  }
  if (g %in% names(.l2g_greek_funs) && !declared && .l2g_followed_by_paren(P, 0L)) {
    spec <- .l2g_greek_funs[[g]]
    opt <- .l2g_decide(P, paste0("greekfun@", tok$pos), "special-function",
                       paste0("`\\", g, "(\\cdot)` read as ", spec$text),
                       c("function", "variable"))
    if (opt == "function") {
      args <- .l2g_parse_call_args(P)
      return(as.call(c(list(as.name(spec$fun)), args)))
    }
  }
  .l2g_finish_symbol(P, g)
}

# Parenthesised, comma-separated argument list.
.l2g_parse_call_args <- function(P) {
  tok <- .l2g_peek(P)
  if (.l2g_is(tok, "op", "(")) {
    close <- function(t) .l2g_is(t, "op", ")")
  } else if (.l2g_is(tok, "left", "(")) {
    close <- function(t) .l2g_is(t, "right")
  } else {
    .l2g_stop("parse", paste0("expected `(` at position ", tok$pos))
  }
  .l2g_advance(P)
  args <- list(.l2g_parse_expr(P))
  repeat {
    t <- .l2g_peek(P)
    if (.l2g_is(t, "op", ",")) { .l2g_advance(P); args[[length(args) + 1L]] <- .l2g_parse_expr(P); next }
    if (close(t)) { .l2g_advance(P); break }
    .l2g_stop("parse", paste0("expected `,` or `)` at position ", t$pos))
  }
  args
}

# \sin, \log_2, \sin^2, \sin^{-1}, \ln\Gamma(...) ...
.l2g_apply_function_cmd <- function(P, v, tok) {
  base <- NULL
  power <- NULL
  fun <- .l2g_fun_cmds[[v]]
  if (v %in% c("log", "lg", "ln") && .l2g_is(.l2g_peek(P), "op", "_")) {
    .l2g_advance(P)
    base <- .l2g_parse_script_arg(P)
    .l2g_note(P, "log-base", "`\\log_b x` expanded as \\ln x / \\ln b.")
  }
  if (.l2g_is(.l2g_peek(P), "op", "^")) {
    .l2g_advance(P)
    power <- .l2g_parse_script_arg(P)
    if (identical(power, call("-", 1)) && v %in% names(.l2g_inverse)) {
      .l2g_note(P, "inverse", paste0("`\\", v, "^{-1}` read as the inverse function, not a reciprocal."))
      fun <- .l2g_inverse[[v]]
      power <- NULL
    }
  }
  if (v %in% c("ln", "log") && is.null(base) && is.null(power)) {
    nx <- .l2g_peek(P)
    if (.l2g_is(nx, "cmd", "Gamma") && .l2g_followed_by_paren(P, 1L)) {
      .l2g_advance(P)
      args <- .l2g_parse_call_args(P)
      return(call("lgamma", args[[1L]]))
    }
    if (.l2g_is(nx, "name") && nx$value %in% c("B", "Beta") && .l2g_followed_by_paren(P, 1L)) {
      .l2g_advance(P)
      args <- .l2g_parse_call_args(P)
      if (length(args) != 2L) .l2g_stop("parse", "the beta function takes two arguments")
      return(call("lbeta", args[[1L]], args[[2L]]))
    }
  }
  if (v == "log" && is.null(base)) {
    opt <- .l2g_decide(P, paste0("log@", tok$pos), "log-base",
                       "`\\log` read as the natural logarithm",
                       c("natural logarithm", "base-10 logarithm"))
    if (opt == "base-10 logarithm") base <- 10
  }
  if (.l2g_followed_by_paren(P, 0L)) {
    args <- .l2g_parse_call_args(P)
    if (length(args) != 1L) {
      .l2g_stop("parse", paste0("\\", v, " takes one argument, got ", length(args)))
    }
    arg <- args[[1L]]
  } else {
    arg <- .l2g_parse_implicit_arg(P, v, tok)
  }
  out <- if (v %in% names(.l2g_reciprocal)) {
    call("/", 1, call(.l2g_reciprocal[[v]], arg))
  } else if (!is.null(base)) {
    call("/", call("log", arg), call("log", base))
  } else {
    call(fun, arg)
  }
  if (!is.null(power)) out <- call("^", out, power)
  out
}

# Unparenthesised argument: `\sin x`, `\sin 2x`, `\sin x y` (ambiguous).
.l2g_parse_implicit_arg <- function(P, v, tok) {
  if (.l2g_is(.l2g_peek(P), "op", "-")) {
    .l2g_advance(P)
    return(call("-", .l2g_parse_implicit_arg(P, v, tok)))
  }
  if (!.l2g_starts_factor(P)) {
    .l2g_stop("parse", paste0("missing argument for \\", v, " at position ", tok$pos))
  }
  arg <- .l2g_parse_power(P)
  more <- function() .l2g_starts_factor(P, allow_functions = FALSE) &&
    !.l2g_followed_by_paren(P, 0L)
  if (more()) {
    # Parse the longest reading tentatively to show its extent, then decide.
    k <- P$i
    before <- list(i = P$i, toks = P$toks, amb = P$ambiguities, notes = P$notes)
    whole_arg <- arg
    while (more()) whole_arg <- call("*", whole_arg, .l2g_parse_power(P))
    after <- list(i = P$i, toks = P$toks, amb = P$ambiguities, notes = P$notes)
    text <- paste0("`", .l2g_snippet(P, k - 1L, P$i),
                   "`: argument of \\", v, " extends over the whole product")
    P$i <- before$i; P$toks <- before$toks
    P$ambiguities <- before$amb; P$notes <- before$notes
    opt <- .l2g_decide(P, paste0("fnarg@", tok$pos), "function-scope", text,
                       c("whole product", "first factor only"))
    if (opt == "whole product") {
      rec <- P$ambiguities[[length(P$ambiguities)]]
      P$i <- after$i; P$toks <- after$toks
      P$ambiguities <- c(after$amb, list(rec)); P$notes <- after$notes
      arg <- whole_arg
    }
  }
  arg
}

# \mathrm{...}, \operatorname{...}, \text{...}
.l2g_parse_name <- function(P) {
  tok <- .l2g_advance(P)
  t <- tok$value
  if (t == "d" && P$in_integral > 0L) {
    .l2g_stop("parse", paste0("unexpected differential at position ", tok$pos))
  }
  is_fun <- tok$cmd == "operatorname" ||
    (nchar(t) > 1L && .l2g_followed_by_paren(P, 0L) &&
       (t %in% names(.l2g_named_funs) || exists(t, envir = asNamespace("stats"), mode = "function") ||
          exists(t, envir = baseenv(), mode = "function")))
  if (t %in% c("B", "Beta") && .l2g_followed_by_paren(P, 0L)) is_fun <- TRUE
  if (is_fun) {
    fun <- if (t %in% names(.l2g_named_funs)) .l2g_named_funs[[t]] else t
    if (t != fun) {
      .l2g_note(P, "named-function", paste0("`", t, "` read as the R function `", fun, "`."))
    }
    if (.l2g_followed_by_paren(P, 0L)) {
      args <- .l2g_parse_call_args(P)
    } else {
      args <- list(.l2g_parse_implicit_arg(P, t, tok))
    }
    return(as.call(c(list(as.name(fun)), args)))
  }
  if (t == "e" && !.l2g_is(.l2g_peek(P), "op", "_")) return(quote(exp(1)))
  if (!nzchar(t)) .l2g_stop("parse", paste0("empty \\", tok$cmd, "{} at position ", tok$pos))
  .l2g_finish_symbol(P, t)
}

# ---- integrals, sums, evaluation bars, matrices ---------------------------

.l2g_is_differential <- function(P, k) {
  tok <- .l2g_peek(P, k)
  if (tok$type == "name" && tok$value == "d") return(TRUE)
  if (tok$type == "ident" && tok$value == "d") {
    nx <- .l2g_peek(P, k + 1L)
    return(nx$type == "ident" || (nx$type == "cmd" && nx$value %in% .l2g_greek))
  }
  FALSE
}

.l2g_consume_differential <- function(P) {
  .l2g_advance(P)
  .l2g_read_symbol_name(P)
}

.l2g_parse_integral <- function(P) {
  tok <- .l2g_advance(P)
  lo <- hi <- NULL
  for (k in 1:2) {
    t <- .l2g_peek(P)
    if (.l2g_is(t, "op", "_")) { .l2g_advance(P); lo <- .l2g_parse_script_arg(P) }
    else if (.l2g_is(t, "op", "^")) { .l2g_advance(P); hi <- .l2g_parse_script_arg(P) }
  }
  if (is.null(lo) || is.null(hi)) {
    .l2g_stop("parse", paste0("indefinite integral at position ", tok$pos,
                              ": both bounds are required"))
  }
  P$in_integral <- P$in_integral + 1L
  on.exit(P$in_integral <- P$in_integral - 1L)
  if (.l2g_is_differential(P, 0L)) {
    var <- .l2g_consume_differential(P)
    body <- .l2g_parse_term(P)
  } else {
    body <- .l2g_parse_expr(P)
    if (!.l2g_is_differential(P, 0L)) {
      .l2g_stop("parse", paste0("integral at position ", tok$pos,
                                " has no differential (d<variable>)"))
    }
    var <- .l2g_consume_differential(P)
  }
  call("integral", body, as.name(var), lo, hi)
}

.l2g_parse_bigop <- function(P) {
  tok <- .l2g_advance(P)
  op <- tok$value
  .l2g_expect(P, "op", "_", what = paste0("`_{i=...}` after \\", op))
  .l2g_expect(P, "op", "{")
  idx <- .l2g_read_symbol_name(P)
  .l2g_expect(P, "op", "=")
  lo <- .l2g_parse_expr(P)
  .l2g_expect(P, "op", "}")
  .l2g_expect(P, "op", "^", what = paste0("an upper bound for \\", op))
  hi <- .l2g_parse_script_arg(P)
  const <- function(x) if (is.call(x) && !length(all.vars(x))) eval(x, baseenv()) else x
  lo <- const(lo); hi <- const(hi)
  int_ok <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x)
  if (!int_ok(lo) || !int_ok(hi)) {
    .l2g_stop("parse", paste0("\\", op, " at position ", tok$pos,
                              ": only integer-literal bounds are supported (symbolic bounds ",
                              "need vector semantics, outside this grammar)"))
  }
  if (hi < lo || hi - lo > 200) .l2g_stop("parse", paste0("\\", op, " bounds out of range"))
  body <- .l2g_parse_term(P)
  terms <- lapply(seq(lo, hi), function(k) .l2g_subst_index(body, idx, k))
  .l2g_note(P, "expansion", paste0("\\", op, "_{", idx, "=", lo, "}^{", hi, "} expanded into ",
                                   length(terms), " terms."))
  Reduce(function(a, b) call(if (op == "sum") "+" else "*", a, b), terms)
}

# Replace index `idx` by integer k: in bare symbols and in subscripts.
.l2g_subst_index <- function(expr, idx, k) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (identical(nm, idx)) return(as.numeric(k))
    pos <- regexpr("_", nm, fixed = TRUE)
    if (pos > 0L) {
      base <- substr(nm, 1L, pos - 1L)
      sub <- substring(nm, pos + 1L)
      pieces <- regmatches(sub, gregexpr(paste0(paste(.l2g_greek, collapse = "|"), "|."), sub))[[1L]]
      pieces[pieces == idx] <- as.character(k)
      return(as.name(paste0(base, "_", paste(pieces, collapse = ""))))
    }
    return(expr)
  }
  if (is.call(expr)) {
    for (j in seq_along(expr)[-1L]) expr[[j]] <- .l2g_subst_index(expr[[j]], idx, k)
  }
  expr
}

# \left. \frac{\partial}{\partial s} body \right|_{s = a}  ->  numd(body, s, a)
.l2g_parse_eval_bar <- function(P, tok) {
  if (!.l2g_is(.l2g_peek(P), "cmd", c("frac", "dfrac", "tfrac"))) {
    .l2g_stop("parse", paste0("\\left. at position ", tok$pos,
                              " must open a derivative evaluation bar"))
  }
  .l2g_advance(P)
  is_d <- function(t) .l2g_is(t, "cmd", "partial") || (t$type %in% c("name", "ident") && t$value == "d")
  .l2g_expect(P, "op", "{")
  if (!is_d(.l2g_advance(P))) .l2g_stop("parse", "expected \\partial in the evaluation bar")
  .l2g_expect(P, "op", "}")
  .l2g_expect(P, "op", "{")
  if (!is_d(.l2g_advance(P))) .l2g_stop("parse", "expected \\partial s in the evaluation bar")
  s <- .l2g_read_symbol_name(P)
  .l2g_expect(P, "op", "}")
  body <- .l2g_parse_expr(P)
  r <- .l2g_peek(P)
  if (!.l2g_is(r, "right", "|")) .l2g_stop("parse", "expected \\right| closing the evaluation bar")
  .l2g_advance(P)
  .l2g_expect(P, "op", "_")
  .l2g_expect(P, "op", "{")
  s2 <- .l2g_read_symbol_name(P)
  if (!identical(s, s2)) .l2g_stop("parse", "evaluation-bar variable does not match")
  .l2g_expect(P, "op", "=")
  at <- .l2g_parse_expr(P)
  .l2g_expect(P, "op", "}")
  call("numd", body, as.name(s), at)
}

.l2g_parse_matrix <- function(P) {
  tok <- .l2g_advance(P)
  env <- tok$value
  if (!(env %in% c("pmatrix", "bmatrix", "Bmatrix", "matrix", "smallmatrix"))) {
    .l2g_stop("parse", paste0("unsupported environment ", env))
  }
  rows <- list()
  cells <- list()
  repeat {
    t <- .l2g_peek(P)
    if (.l2g_is(t, "end")) break
    cells[[length(cells) + 1L]] <- .l2g_parse_expr(P)
    t <- .l2g_peek(P)
    if (.l2g_is(t, "op", "&")) { .l2g_advance(P); next }
    if (.l2g_is(t, "newline")) {
      .l2g_advance(P)
      rows[[length(rows) + 1L]] <- cells
      cells <- list()
      next
    }
    if (!.l2g_is(t, "end")) .l2g_stop("parse", paste0("unexpected `", .l2g_tok_text(t), "` in matrix"))
  }
  if (length(cells)) rows[[length(rows) + 1L]] <- cells
  end <- .l2g_advance(P)
  if (!identical(end$value, env)) .l2g_stop("parse", paste0("\\begin{", env, "} closed by \\end{", end$value, "}"))
  widths <- vapply(rows, length, integer(1))
  comps <- if (all(widths == 1L)) lapply(rows, `[[`, 1L)
           else if (length(rows) == 1L) rows[[1L]]
           else .l2g_stop("parse", "only vectors (one row or one column) are supported")
  as.call(c(list(as.name("c")), comps))
}
