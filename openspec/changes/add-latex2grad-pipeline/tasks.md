## 1. Package skeleton

- [x] 1.1 `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`, `LICENSE` stub + `LICENSE.md`, `tests/testthat.R` — Imports DefDiff (>= 0.2.0), stats; Suggests testthat, httr2, jsonlite
- [x] 1.2 `CLAUDE.md` — Spectra instruction block + project notes

## 2. Entry: LaTeX grammar (latex-entry)

- [x] 2.1 `R/tex_tokenize.R` — tokenizer (commands, letters, numbers, groups, `\left`/`\right`, spacing)
- [x] 2.2 `R/symbols.R` — Greek/symbol name mapping shared by parser and printer
- [x] 2.3 `R/tex_parse.R` — precedence parser, function table, `\frac`, `\sqrt[n]`, `|·|`, powers, subscripts, `pmatrix`/`bmatrix`, finite `\sum`/`\prod` expansion, `t_\nu^{-1}`, `\Phi`, numeric-partial notation; assumption recording; `strict` mode; `latex2grad_parse_error` / `latex2grad_ambiguity` conditions
- [x] 2.4 `R/llm.R` — `llm_claude()` disambiguation hook and `ocr_claude()` via httr2 raw HTTP with injectable transport

## 3. Canonical form (canonical-form)

- [x] 3.1 `R/normalize.R` — fixpoint normalizer (folding, identities, signs, associativity, DefDiff artifact cleanup)
- [x] 3.2 `R/tex_print.R` — precedence-aware printer, exact number printing, matrices, numeric-partial nodes

## 4. Differentiation (differentiation, out-of-scope-boundary)

- [x] 4.1 `R/lower.R` — definitional lowering table with record of applied rewrites
- [x] 4.2 `R/differentiate.R` — scalar lifting into `DefDiff::grad`, opaque-symbol abstraction, chain rule with `numeric_partial`, grad / hessian / jacobian assembly, out-of-scope table
- [x] 4.3 `R/numeric.R` — Richardson central differences and `numeric_partial()` evaluation (plus `cot`/`sec`/`csc`, which base R lacks)
- [x] 4.4 `R/differentiate.R` — constant freezing and exp-log retry for DefDiff 0.2.0 power-rule failures (design D7a)

## 5. Certificate and report (verification-certificate, latex-report)

- [x] 5.1 `R/certify.R` — round-trip checks, closure levels, seeded random-point comparison
- [x] 5.2 `R/report.R` — `l2g_report()` with the seven-section layout and conclusion; display-only folding of definitions and abbreviation of repeated sub-trees
- [x] 5.3 `R/latex2grad.R` — `latex2grad()` pipeline, result object, `print()`, `as_function()`

## 6. Tests, examples, docs

- [x] 6.1 `tests/testthat/test-parse.R`, `test-normalize-print.R`, `test-differentiate.R`, `test-boundary.R`, `test-certify.R`, `test-report.R`, `test-llm.R`
- [x] 6.2 `inst/examples/t-copula.R` + `tests/testthat/test-t-copula.R` — worked example ∂/∂ν
- [x] 6.3 roxygen docs (`man/`), `README.md` usage + status, `NEWS.md`
- [x] 6.4 `R CMD check` clean; t-copula report compiles with `pdflatex`
