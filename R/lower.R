## lower.R
## Definitional lowering (design D4): exact identities that move common
## functions into DefDiff's catalog before differentiation. Each rewrite that
## fires is recorded and shown in the report.

.lower_rules <- list(
  tan = list(rule = "tan(a) = sin(a)/cos(a)",
             fn = function(a) call("/", call("sin", a), call("cos", a))),
  cot = list(rule = "cot(a) = cos(a)/sin(a)",
             fn = function(a) call("/", call("cos", a), call("sin", a))),
  sec = list(rule = "sec(a) = 1/cos(a)", fn = function(a) call("/", 1, call("cos", a))),
  csc = list(rule = "csc(a) = 1/sin(a)", fn = function(a) call("/", 1, call("sin", a))),
  sinh = list(rule = "sinh(a) = (exp(a) - exp(-a))/2",
              fn = function(a) call("/", call("-", call("exp", a), call("exp", call("-", a))), 2)),
  cosh = list(rule = "cosh(a) = (exp(a) + exp(-a))/2",
              fn = function(a) call("/", call("+", call("exp", a), call("exp", call("-", a))), 2)),
  asin = list(rule = "asin(a) = atan(a/sqrt(1 - a^2)), valid on (-1, 1)",
              fn = function(a) call("atan", call("/", a, call("sqrt", call("-", 1, call("^", a, 2)))))),
  acos = list(rule = "acos(a) = pi/2 - atan(a/sqrt(1 - a^2)), valid on (-1, 1)",
              fn = function(a) call("-", call("/", quote(pi), 2),
                                    call("atan", call("/", a, call("sqrt", call("-", 1, call("^", a, 2))))))),
  dnorm = list(rule = "dnorm(a) = exp(-a^2/2)/sqrt(2*pi)",
               fn = function(a) call("/", call("exp", call("-", call("/", call("^", a, 2), 2))),
                                     call("sqrt", call("*", 2, quote(pi)))))
)

# Returns list(expr = lowered canonical tree, rules = rewrites applied).
.lower <- function(expr) {
  used <- character()
  walk <- function(e) {
    if (!is.call(e) || !is.symbol(e[[1L]])) return(e)
    head <- as.character(e[[1L]])
    if (head == "numeric_partial") return(e)
    if (length(e) > 1L) for (j in seq_along(e)[-1L]) e[[j]] <- walk(e[[j]])
    r <- .lower_rules[[head]]
    if (!is.null(r) && length(e) == 2L) {
      used <<- union(used, r$rule)
      return(r$fn(e[[2L]]))
    }
    e
  }
  list(expr = normalize_tree(walk(expr)), rules = used)
}
