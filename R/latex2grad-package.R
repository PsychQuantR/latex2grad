#' latex2grad: verifiable differentiation of formulas written in LaTeX
#'
#' The pipeline is [latex2grad()]: LaTeX (or an image, through an optional
#' entry assistant such as [claude_entry()]) is parsed by [tex_parse()],
#' normalized by [tex_normalize()], differentiated by [l2g_differentiate()]
#' through DefDiff, printed back by [tex_print()], and certified by a
#' round trip ([l2g_round_trip()]) plus a numeric comparison. The result is a
#' report with the sections Source, Parsed form, Assumptions, Derivative,
#' Verification and Out of scope; see [write_report()].
#'
#' @keywords internal
"_PACKAGE"
