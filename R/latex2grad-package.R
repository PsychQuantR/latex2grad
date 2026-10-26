#' latex2grad: verifiable differentiation of formulas written in LaTeX
#'
#' Pipeline: LaTeX (or an image, via an OCR hook) -> deterministic parse ->
#' canonical form -> DefDiff derivative -> certificate -> LaTeX report.
#' Start with [latex2grad()] and [l2g_report()].
#'
#' @keywords internal
#' @importFrom stats runif qt pt dt pnorm qnorm dnorm
"_PACKAGE"
