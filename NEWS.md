# latex2grad 0.1.0

First runnable release (Spectra change `add-latex2grad-pipeline`).

* `parse_latex()`: deterministic LaTeX grammar with recorded assumptions and a
  `strict` mode (interim DefTeX layer).
* `normalize_tree()` / `to_latex()`: canonical form and printer with the
  round-trip property `normalize(parse(print(e))) == e`.
* `latex2grad()`: gradient, Hessian, and Jacobian through DefDiff, with
  definitional lowering, `where` definitions, out-of-scope sub-terms handled by
  explicit `numeric_partial()` nodes, and a certificate (round-trip, closure
  level, seeded random-point comparison).
* `l2g_report()`: LaTeX report with Source, Parsed form, Assumptions,
  Derivative, Verification, Out of scope, and Conclusion sections.
* `llm_claude()` / `ocr_claude()`: optional Claude-backed entry hooks for
  disambiguation and image OCR; their decisions are recorded as assumptions.
* Worked example: Student-t copula density, derivative in the degrees of
  freedom (`inst/examples/t-copula.R`).
