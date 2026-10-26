# add-latex2grad-pipeline

**GitHub-side tracker**: #1 (design source: PsychQuant/definable_algebra#22, private)

## Why

The repository holds only the design README ("Design stage. Nothing runnable yet."). The
product it describes — differentiate a formula written in LaTeX (or captured as an image)
and hand back a *certified* derivative — does not exist yet, so the DAT stack has no
user-facing entry point. DefDiff already supplies closure-preserving symbolic
differentiation; what is missing is everything around it: reading LaTeX, normalizing it,
printing it back, proving the result, and being honest about what falls outside the
language.

## What Changes

A first runnable release (v0.1.0) of the R package `latex2grad`:

1. **Entry** — a deterministic LaTeX grammar (tokenizer + precedence parser) that turns a
   formula into an R expression tree and records every convention it applied as an
   *assumption*. Optional LLM hooks (`llm_claude()`, `ocr_claude()`) are invoked only at
   the entry: to rewrite LaTeX the grammar rejects (or, in strict mode, flags as
   ambiguous) and to transcribe a formula image. Their decisions are recorded verbatim.
2. **Canonical form** — a deterministic normalizer (constant folding, identity
   elimination, sign and associativity normalization, DefDiff artifact cleanup) and a
   LaTeX printer such that `normalize(parse(print(e))) == e`.
3. **Differentiation** — `grad`, `hessian`, `jacobian` computed by DefDiff. Scalar
   formulas are lifted into DefDiff's vector language as `sum(f)` over length-1
   variables; exact definitional rewrites (`tan`, `sinh`, `cosh`, `asin`, `acos`,
   `dnorm`, …) lower functions into DefDiff's catalog before differentiation.
4. **Out-of-scope boundary** — maximal sub-terms whose head is not a DefDiff generator are
   abstracted to opaque symbols; the chain rule is completed across the boundary with an
   explicit numeric-partial node, and each such term is reported with its reason.
5. **Certificate** — round-trip status for the source and every derivative entry,
   DefDiff's syntactic closure level check, and a seeded random-point comparison against
   Richardson-extrapolated central differences (max relative error, pass/fail).
6. **Report** — `l2g_report()` writes a LaTeX document with the README layout: Source ·
   Parsed form · Assumptions · Derivative · Verification · Out of scope · Conclusion. A
   failed check still produces a report.

## Non-Goals

- Re-implementing derivative rules that belong in DefDiff (catalog extensions go
  upstream, not here).
- Symbolic-bound sums (`\sum_{i=1}^{n}`), matrix calculus, integrals in the LaTeX input —
  rejected with an explicit error and still reported.
- Simplification beyond canonical cleanup (no like-term collection, no factoring).
- Calling an LLM anywhere after the entry.

## Capabilities

### New Capabilities

- `latex-entry`: LaTeX → expression tree with recorded assumptions; optional LLM
  disambiguation and image OCR hooks confined to the entry.
- `canonical-form`: deterministic normalization and LaTeX printing with the round-trip
  property.
- `differentiation`: grad / hessian / jacobian through DefDiff, including scalar lifting,
  definitional lowering, and artifact cleanup.
- `out-of-scope-boundary`: detection, reporting, and numeric handling of sub-terms outside
  DefDiff's language.
- `verification-certificate`: round-trip, closure-level, and random-point numeric checks.
- `latex-report`: the LaTeX report and its fixed section layout.

### Modified Capabilities

(none)

## Impact

- Affected specs: all six capabilities above (new)
- Affected code: `DESCRIPTION`, `NAMESPACE`, `R/*.R` (new), `tests/testthat/*` (new),
  `man/*` (generated), `inst/examples/t-copula.R`, `README.md`, `NEWS.md`, `LICENSE`
  (R `MIT + file LICENSE` stub; full text moves to `LICENSE.md`), `CLAUDE.md`
- Dependencies: Imports DefDiff (>= 0.2.0, `Remotes: PsychQuantR/DefDiff`), stats;
  Suggests testthat, httr2, jsonlite
