## tex_tokenize.R
## LaTeX math-mode tokenizer (DefTeX interim layer, design D1).
##
## Produces a list of tokens `list(type, value, start, end)` with character
## offsets into the source. Types:
##   num     digits with an optional decimal part ("3", "3.14", ".5")
##   letter  one Latin letter
##   cmd     a control sequence without the backslash ("frac", "alpha", "\\")
##   name    the braced argument of \mathrm / \operatorname / \mathit / \text,
##           with `value` = the content and `kind` = the command
##   op      any other single character: + - * / = , ! ' & | ^ _ ( ) [ ] < > .
##   lbrace / rbrace
## Whitespace and spacing commands are dropped.

.spacing_cmds <- c(",", ";", ":", "!", " ", "quad", "qquad", "displaystyle",
                   "textstyle", "scriptstyle", "limits", "nolimits", "nonumber",
                   "big", "Big", "bigg", "Bigg", "bigl", "bigr", "Bigl", "Bigr",
                   "biggl", "biggr", "Biggl", "Biggr", "middle")

.name_cmds <- c("mathrm", "operatorname", "mathit", "text", "textrm", "mathsf")

.tex_tokenize <- function(src) {
  if (!is.character(src) || length(src) != 1L || is.na(src)) {
    .parse_error("input must be a single LaTeX string")
  }
  chars <- strsplit(src, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  tokens <- vector("list", n)
  k <- 0L
  push <- function(type, value, start, end, kind = NULL) {
    k <<- k + 1L
    tokens[[k]] <<- list(type = type, value = value, start = start, end = end, kind = kind)
  }
  is_letter <- function(ch) grepl("^[A-Za-z]$", ch)
  is_digit <- function(ch) grepl("^[0-9]$", ch)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (grepl("^\\s$", ch) || ch == "~") {
      i <- i + 1L
      next
    }
    if (ch == "%") {
      # LaTeX comment: skip to end of line
      while (i <= n && chars[i] != "\n") i <- i + 1L
      next
    }
    if (ch == "\\") {
      start <- i
      i <- i + 1L
      if (i > n) .parse_error("dangling backslash at end of input", start)
      if (is_letter(chars[i])) {
        j <- i
        while (j <= n && is_letter(chars[j])) j <- j + 1L
        cmd <- paste(chars[i:(j - 1L)], collapse = "")
        i <- j
      } else {
        cmd <- chars[i]
        i <- i + 1L
      }
      if (cmd %in% .spacing_cmds) next
      if (cmd %in% .name_cmds) {
        # Read the braced argument verbatim (no nesting expected).
        while (i <= n && grepl("^\\s$", chars[i])) i <- i + 1L
        if (i > n || chars[i] != "{") {
          .parse_error(paste0("\\", cmd, " must be followed by a braced name"), start)
        }
        j <- i + 1L
        while (j <= n && chars[j] != "}") j <- j + 1L
        if (j > n) .parse_error(paste0("unclosed brace after \\", cmd), start)
        content <- gsub("\\s+", "", paste(chars[seq_len(j - i - 1L) + i], collapse = ""))
        push("name", content, start, j, kind = cmd)
        i <- j + 1L
        next
      }
      push("cmd", cmd, start, i - 1L)
      next
    }
    if (is_digit(ch) || (ch == "." && i < n && is_digit(chars[i + 1L]))) {
      start <- i
      j <- i
      while (j <= n && is_digit(chars[j])) j <- j + 1L
      if (j <= n && chars[j] == "." && j < n && is_digit(chars[j + 1L])) {
        j <- j + 1L
        while (j <= n && is_digit(chars[j])) j <- j + 1L
      }
      push("num", paste(chars[start:(j - 1L)], collapse = ""), start, j - 1L)
      i <- j
      next
    }
    if (is_letter(ch)) {
      push("letter", ch, i, i)
      i <- i + 1L
      next
    }
    if (ch == "{") {
      push("lbrace", ch, i, i)
    } else if (ch == "}") {
      push("rbrace", ch, i, i)
    } else if (ch %in% c("+", "-", "*", "/", "=", ",", "!", "'", "&", "|", "^",
                         "_", "(", ")", "[", "]", "<", ">", ".")) {
      push("op", ch, i, i)
    } else {
      .parse_error(paste0("unexpected character '", ch, "'"), i)
    }
    i <- i + 1L
  }
  tokens[seq_len(k)]
}
