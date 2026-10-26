## Worked example: density of the bivariate Student-t copula, d/d nu.
##
## c(u1, u2; rho, nu) =
##   Gamma((nu+2)/2) Gamma(nu/2) / (Gamma((nu+1)/2)^2 sqrt(1 - rho^2))
##   * (1 + (x1^2 + x2^2 - 2 rho x1 x2) / (nu (1 - rho^2)))^(-(nu+2)/2)
##   / ((1 + x1^2/nu)^(-(nu+1)/2) (1 + x2^2/nu)^(-(nu+1)/2)),
## with x_i = t_nu^{-1}(u_i) the Student-t quantile.
##
## The nu-dependence through the quantile is outside DefDiff's language and
## has no closed form: latex2grad names it in "Out of scope" and evaluates it
## numerically; everything else is symbolic.

library(latex2grad)

copula <- paste0(
  "c(u_1, u_2; \\rho, \\nu) = ",
  "\\frac{\\Gamma\\left(\\frac{\\nu+2}{2}\\right) \\Gamma\\left(\\frac{\\nu}{2}\\right)}",
  "{\\Gamma\\left(\\frac{\\nu+1}{2}\\right)^{2} \\sqrt{1-\\rho^{2}}}",
  "\\frac{\\left(1 + \\frac{t_{\\nu}^{-1}(u_1)^{2} + t_{\\nu}^{-1}(u_2)^{2}",
  " - 2\\rho\\, t_{\\nu}^{-1}(u_1)\\, t_{\\nu}^{-1}(u_2)}{\\nu (1-\\rho^{2})}\\right)^{-\\frac{\\nu+2}{2}}}",
  "{\\left(1 + \\frac{t_{\\nu}^{-1}(u_1)^{2}}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}",
  " \\left(1 + \\frac{t_{\\nu}^{-1}(u_2)^{2}}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}}"
)

report <- latex2grad(
  copula, wrt = "nu",
  domain = list(u_1 = c(0.05, 0.95), u_2 = c(0.05, 0.95),
                rho = c(-0.8, 0.8), nu = c(2, 30))
)
print(report)
write_report(report, "student_t_copula.tex")
write_report(report, "student_t_copula.md")
