## Worked example: derivative of the bivariate Student-t copula density with
## respect to the degrees of freedom nu.
##
## c(u_1, u_2; rho, nu) =
##   Gamma((nu+2)/2) Gamma(nu/2) / (sqrt(1 - rho^2) Gamma((nu+1)/2)^2)
##   * (1 + (x_1^2 - 2 rho x_1 x_2 + x_2^2) / (nu (1 - rho^2)))^(-(nu+2)/2)
##   / ((1 + x_1^2/nu)^(-(nu+1)/2) (1 + x_2^2/nu)^(-(nu+1)/2)),
## with x_i = t_nu^{-1}(u_i), the Student-t quantile.
##
## The Gamma terms and the quantile are outside DefDiff's language: they are
## listed under "Out of scope" and their nu-derivatives are taken numerically;
## everything else is symbolic.

library(latex2grad)

t_copula <- paste0(
  "c(u_1, u_2) = \\frac{\\Gamma\\left(\\frac{\\nu+2}{2}\\right)\\Gamma\\left(\\frac{\\nu}{2}\\right)}",
  "{\\sqrt{1-\\rho^2}\\,\\Gamma\\left(\\frac{\\nu+1}{2}\\right)^2}\\,",
  "\\frac{\\left(1+\\frac{x_1^2-2\\rho x_1 x_2+x_2^2}{\\nu(1-\\rho^2)}\\right)^{-\\frac{\\nu+2}{2}}}",
  "{\\left(1+\\frac{x_1^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}\\left(1+\\frac{x_2^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}}"
)

res <- latex2grad(
  t_copula, wrt = "nu",
  where = c(x_1 = "t_\\nu^{-1}(u_1)", x_2 = "t_\\nu^{-1}(u_2)"),
  domain = list(nu = c(3, 12), rho = c(-0.8, 0.8), u_1 = c(0.1, 0.9), u_2 = c(0.1, 0.9))
)
print(res)
res$out_of_scope[, c("latex", "head", "treatment")]

# Evaluate the derivative at a point
d_nu <- as_function(res)
d_nu(nu = 5, rho = 0.3, u_1 = 0.2, u_2 = 0.7)

# Write the LaTeX report
report <- l2g_report(res, file = file.path(tempdir(), "t-copula-report.tex"))
