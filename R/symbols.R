## symbols.R
## Symbol-name conventions shared by the parser and the printer.
##
## A LaTeX symbol becomes an R name `<base>` or `<base>_<sub>`: `x_1`, `x_12`,
## `nu`, `sigma_x`. Greek letters are stored by their command name. The
## printer inverts the mapping; round-trip is guaranteed at the name level.

.greek_lower <- c(
  "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta",
  "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu", "xi", "pi",
  "varpi", "rho", "varrho", "sigma", "varsigma", "tau", "upsilon", "phi",
  "varphi", "chi", "psi", "omega"
)
.greek_upper <- c(
  "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi",
  "Psi", "Omega"
)
.greek <- c(.greek_lower, .greek_upper)

# Symbols that denote constants rather than variables.
.constant_symbols <- c("pi")

# Split a symbol name into base and subscript at the first underscore.
.split_symbol <- function(name) {
  pos <- regexpr("_", name, fixed = TRUE)
  if (pos < 1L) return(list(base = name, sub = NULL))
  list(base = substr(name, 1L, pos - 1L), sub = substr(name, pos + 1L, nchar(name)))
}

.symbol_base_latex <- function(base) {
  if (base %in% .greek) return(paste0("\\", base))
  if (nchar(base) == 1L) return(base)
  paste0("\\mathit{", base, "}")
}

.symbol_to_latex <- function(name) {
  parts <- .split_symbol(name)
  out <- .symbol_base_latex(parts$base)
  if (!is.null(parts$sub)) {
    sub <- if (parts$sub %in% .greek) paste0("\\", parts$sub) else parts$sub
    out <- paste0(out, "_{", sub, "}")
  }
  out
}

# Free symbols of a tree: variables, excluding constants. Strings inside
# numeric_partial() nodes are variable names, not symbols, and are skipped by
# all.vars() already.
.free_symbols <- function(expr) {
  setdiff(all.vars(expr), .constant_symbols)
}
