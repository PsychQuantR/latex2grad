## lexer.R
## LaTeX math-mode tokenizer. Produces a list of tokens, each a list with
## `type`, `value`, `pos` (1-based character offset) and `space` (TRUE when
## whitespace or a spacing command preceded the token).
##
## Token types:
##   num     numeric literal ("3.14")
##   ident   a single letter (math mode treats `xy` as two letters)
##   cmd     backslash command, value without the backslash ("frac", "alpha")
##   name    braced text of \mathrm / \operatorname / \text ...; `cmd` field
##           records which command produced it
##   op      one of + - * / ^ _ ( ) [ ] { } | , ; = ! ' & < >
##   lbrace / rbrace for `\{` `\}` (delimiters, unlike grouping `{ }`)
##   left / right for `\left<delim>` / `\right<delim>`; value is the delimiter
##   begin / end for `\begin{env}` / `\end{env}`
##   newline for `\\`
##   eof

.l2g_lex <- function(src) {
  chars <- strsplit(src, "")[[1L]]
  n <- length(chars)
  i <- 1L
  toks <- list()
  space <- FALSE

  emit <- function(type, value, pos, ...) {
    toks[[length(toks) + 1L]] <<- c(list(type = type, value = value, pos = pos,
                                         space = space), list(...))
    space <<- FALSE
  }
  is_letter <- function(ch) grepl("^[A-Za-z]$", ch)
  is_digit <- function(ch) grepl("^[0-9]$", ch)

  # Read a `{...}` argument starting at index j (skipping blanks); returns the
  # raw inner text and the index after the closing brace.
  read_braced <- function(j, what) {
    while (j <= n && grepl("\\s", chars[[j]])) j <- j + 1L
    if (j > n || chars[[j]] != "{") {
      .l2g_stop("parse", paste0("expected `{` after \\", what, " at position ", j))
    }
    depth <- 0L
    start <- j + 1L
    while (j <= n) {
      ch <- chars[[j]]
      if (ch == "\\" && j < n) { j <- j + 2L; next }
      if (ch == "{") depth <- depth + 1L
      if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) {
          inner <- if (j > start) paste(chars[start:(j - 1L)], collapse = "") else ""
          return(list(text = inner, next_i = j + 1L))
        }
      }
      j <- j + 1L
    }
    .l2g_stop("parse", paste0("unbalanced braces after \\", what))
  }

  while (i <= n) {
    ch <- chars[[i]]
    if (grepl("\\s", ch) || ch == "~") { space <- TRUE; i <- i + 1L; next }
    if (ch == "%") {
      while (i <= n && chars[[i]] != "\n") i <- i + 1L
      next
    }
    if (ch == "$") { i <- i + 1L; next }
    if (is_digit(ch) || (ch == "." && i < n && is_digit(chars[[i + 1L]]))) {
      j <- i
      while (j <= n && is_digit(chars[[j]])) j <- j + 1L
      if (j <= n && chars[[j]] == "." && j < n && is_digit(chars[[j + 1L]])) {
        j <- j + 1L
        while (j <= n && is_digit(chars[[j]])) j <- j + 1L
      }
      emit("num", paste(chars[i:(j - 1L)], collapse = ""), i)
      i <- j
      next
    }
    if (is_letter(ch)) { emit("ident", ch, i); i <- i + 1L; next }
    if (ch == "\\") {
      if (i == n) .l2g_stop("parse", "dangling backslash at end of input")
      nx <- chars[[i + 1L]]
      if (!is_letter(nx)) {
        if (nx == "\\") { emit("newline", "\\\\", i); i <- i + 2L; next }
        if (nx == "{") { emit("lbrace", "{", i); i <- i + 2L; next }
        if (nx == "}") { emit("rbrace", "}", i); i <- i + 2L; next }
        if (nx == "|") { emit("op", "||", i); i <- i + 2L; next }
        if (nx %in% .l2g_ignored_cmds) { space <- TRUE; i <- i + 2L; next }
        .l2g_stop("parse", paste0("unsupported command \\", nx, " at position ", i))
      }
      j <- i + 1L
      while (j <= n && is_letter(chars[[j]])) j <- j + 1L
      name <- paste(chars[(i + 1L):(j - 1L)], collapse = "")
      star <- j <= n && chars[[j]] == "*"
      if (star && name == "operatorname") j <- j + 1L
      if (name %in% .l2g_ignored_cmds) { space <- TRUE; i <- j; next }
      if (name %in% c("left", "right")) {
        k <- j
        while (k <= n && grepl("\\s", chars[[k]])) k <- k + 1L
        if (k > n) .l2g_stop("parse", paste0("\\", name, " without a delimiter"))
        d <- chars[[k]]
        if (d == "\\") {
          m <- k + 1L
          while (m <= n && is_letter(chars[[m]])) m <- m + 1L
          if (m == k + 1L) { d <- paste0("\\", chars[[k + 1L]]); m <- m + 1L }
          else d <- paste0("\\", paste(chars[(k + 1L):(m - 1L)], collapse = ""))
          k <- m
        } else {
          k <- k + 1L
        }
        d <- switch(d, "\\{" = "{", "\\}" = "}", "\\lvert" = "|", "\\rvert" = "|",
                    "\\vert" = "|", "\\lbrack" = "[", "\\rbrack" = "]",
                    "\\lparen" = "(", "\\rparen" = ")", d)
        emit(name, d, i)
        i <- k
        next
      }
      if (name %in% c("begin", "end")) {
        b <- read_braced(j, name)
        emit(name, b$text, i)
        i <- b$next_i
        next
      }
      if (name %in% .l2g_text_cmds) {
        b <- read_braced(j, name)
        txt <- gsub("\\\\_", "_", b$text)
        txt <- gsub("\\\\,|\\\\;|\\\\!|\\s+", "", txt)
        emit("name", txt, i, cmd = name)
        i <- b$next_i
        next
      }
      if (name %in% c("lvert", "rvert", "vert", "mid")) {
        emit("op", "|", i, hint = name)
        i <- j
        next
      }
      emit("cmd", name, i)
      i <- j
      next
    }
    if (ch == ":" && i < n && chars[[i + 1L]] == "=") {
      emit("op", "=", i)
      i <- i + 2L
      next
    }
    if (ch %in% c("+", "-", "*", "/", "^", "_", "(", ")", "[", "]", "{", "}",
                  "|", ",", ";", "=", "!", "'", "&", "<", ">")) {
      emit("op", ch, i)
      i <- i + 1L
      next
    }
    if (ch == ".") {
      # A trailing full stop (sentence punctuation after a display formula).
      rest <- if (i < n) paste(chars[(i + 1L):n], collapse = "") else ""
      if (!grepl("\\S", gsub("[$]", "", rest))) { i <- i + 1L; next }
    }
    .l2g_stop("parse", paste0("unexpected character `", ch, "` at position ", i))
  }
  # Drop trailing sentence punctuation (`f(x) = x^2,`).
  while (length(toks) && identical(toks[[length(toks)]]$type, "op") &&
         toks[[length(toks)]]$value %in% c(",", ";")) {
    toks[[length(toks)]] <- NULL
  }
  toks[[length(toks) + 1L]] <- list(type = "eof", value = "", pos = n + 1L,
                                    space = space)
  toks
}

# Typed condition used throughout the package.
.l2g_stop <- function(kind, message, ...) {
  cond <- structure(
    class = c(paste0("latex2grad_", kind, "_error"), "latex2grad_error",
              "error", "condition"),
    list(message = message, call = NULL, ...)
  )
  stop(cond)
}
