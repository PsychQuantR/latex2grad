## symbols.R
## Shared vocabulary between the parser and the printer: Greek letters,
## LaTeX function commands, and the R names they map to.

# Greek letters usable as variable names. `\pi` is the constant, not a variable.
.l2g_greek <- c(
  "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta",
  "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu", "xi", "rho",
  "varrho", "sigma", "varsigma", "tau", "upsilon", "phi", "varphi", "chi",
  "psi", "omega",
  "Gamma", "Delta", "Theta", "Lambda", "Xi", "Sigma", "Upsilon", "Phi",
  "Psi", "Omega", "ell"
)

# Elementary function commands: LaTeX command name -> R function name. These
# take an argument with or without parentheses (`\sin x`, `\sin(x)`).
.l2g_fun_cmds <- c(
  sin = "sin", cos = "cos", tan = "tan",
  arcsin = "asin", arccos = "acos", arctan = "atan",
  sinh = "sinh", cosh = "cosh", tanh = "tanh",
  arsinh = "asinh", arcsinh = "asinh", arcosh = "acosh", arccosh = "acosh",
  artanh = "atanh", arctanh = "atanh",
  exp = "exp", ln = "log", log = "log", lg = "log10",
  cot = "cot", sec = "sec", csc = "csc", coth = "coth"
)

# Reciprocal functions R does not have: expanded to 1 / f(x) at parse time.
.l2g_reciprocal <- c(cot = "tan", sec = "cos", csc = "sin", coth = "tanh")

# Inverse readings of `\f^{-1}`.
.l2g_inverse <- c(sin = "asin", cos = "acos", tan = "atan",
                  sinh = "asinh", cosh = "acosh", tanh = "atanh")

# Greek commands that denote a special function when immediately followed by
# a parenthesised argument. Each reading is recorded as an assumption.
.l2g_greek_funs <- list(
  Gamma  = list(fun = "gamma",   text = "the gamma function"),
  psi    = list(fun = "digamma", text = "the digamma function"),
  Phi    = list(fun = "pnorm",   text = "the standard normal CDF"),
  phi    = list(fun = "dnorm",   text = "the standard normal density"),
  varphi = list(fun = "dnorm",   text = "the standard normal density")
)

# `\operatorname{...}` / `\mathrm{...}` names with a conventional R function.
.l2g_named_funs <- c(
  erf = "erf", erfc = "erfc", B = "beta", Beta = "beta", sgn = "sign",
  sign = "sign", lgamma = "lgamma", digamma = "digamma",
  trigamma = "trigamma", lbeta = "lbeta",
  arsinh = "asinh", arcosh = "acosh", artanh = "atanh"
)

# R function -> LaTeX head, for one-argument functions printed as `\f(x)`.
.l2g_print_fun1 <- c(
  sin = "\\sin", cos = "\\cos", tan = "\\tan",
  asin = "\\arcsin", acos = "\\arccos", atan = "\\arctan",
  sinh = "\\sinh", cosh = "\\cosh", tanh = "\\tanh",
  asinh = "\\operatorname{arsinh}", acosh = "\\operatorname{arcosh}",
  atanh = "\\operatorname{artanh}",
  log = "\\ln", log10 = "\\lg",
  gamma = "\\Gamma", lgamma = "\\ln\\Gamma", digamma = "\\psi",
  trigamma = "\\psi_{1}",
  pnorm = "\\Phi", dnorm = "\\phi", qnorm = "\\Phi^{-1}",
  erf = "\\operatorname{erf}", erfc = "\\operatorname{erfc}",
  sign = "\\operatorname{sgn}"
)

# Two-argument functions printed with a dedicated head.
.l2g_print_fun2 <- c(beta = "\\mathrm{B}", lbeta = "\\ln\\mathrm{B}")

# Spacing / sizing commands the lexer drops.
.l2g_ignored_cmds <- c(
  ",", ";", ":", "!", " ", ">", "quad", "qquad", "displaystyle",
  "textstyle", "scriptstyle", "limits", "nolimits", "big", "Big", "bigg",
  "Bigg", "bigl", "bigr", "Bigl", "Bigr", "biggl", "biggr", "Biggl",
  "Biggr", "bigm", "Bigm", "mathop", "nonumber", "notag"
)

# Commands whose braced argument is read as raw text (a name).
.l2g_text_cmds <- c("mathrm", "operatorname", "text", "textrm", "mathit",
                    "mathsf", "mathbf", "boldsymbol", "bm", "mathbb",
                    "mathcal", "textit")

# LaTeX text for an R symbol name: `nu` -> `\nu`, `x_1` -> `x_{1}`,
# `sigma_xy` -> `\sigma_{xy}`, `rate` -> `\mathrm{rate}`.
.l2g_symbol_tex <- function(name) {
  if (identical(name, "pi")) return("\\pi")
  parts <- regmatches(name, regexpr("_", name), invert = TRUE)[[1L]]
  base <- parts[[1L]]
  sub <- if (length(parts) > 1L) parts[[2L]] else NULL
  base_tex <- .l2g_name_tex(base)
  if (is.null(sub)) return(base_tex)
  paste0(base_tex, "_{", .l2g_subscript_tex(sub), "}")
}

.l2g_name_tex <- function(base) {
  if (base %in% .l2g_greek) return(paste0("\\", base))
  if (nchar(base) == 1L) return(base)
  paste0("\\mathrm{", gsub("_", "\\\\_", base), "}")
}

# A subscript is stored as plain text; Greek names inside it are re-escaped.
.l2g_subscript_tex <- function(sub) {
  if (sub %in% .l2g_greek) return(paste0("\\", sub))
  sub
}

# R name of a LaTeX-ish variable spelling, for user-facing `wrt` / `domain`
# arguments: "\\nu" -> "nu", "u_{1}" -> "u_1", "x" -> "x".
.l2g_r_name <- function(s) {
  s <- gsub("\\s+", "", s)
  s <- gsub("[{}]", "", s)
  gsub("\\\\", "", s)
}
