# latex2grad

End-to-end, verifiable differentiation of formulas written in LaTeX (or captured as images):

```
image ──OCR──▶ LaTeX ──parse──▶ R expression tree ──DefDiff──▶ derivative tree
          ▲                          │                              │
          │                     normalize (canonical form)     numeric verification
          │                          │                              │
          └──────────────── LaTeX report (formula · assumptions · derivative · certificate)
```

Design tracked in `PsychQuant/definable_algebra#22` (private); this repo is the
public product layer. It depends on:

- [DefDiff](https://github.com/PsychQuantR/DefDiff) — symbolic, closure-preserving
  differentiation over the L_3 expression language
- DefTeX — LaTeX ↔ expression-tree parser / normalizer / printer (public mirror
  to follow). Until it ships, an interim implementation lives in this package
  (`parse_latex()`, `normalize_tree()`, `to_latex()`), shaped so those three
  functions can become thin delegations to DefTeX.

## Principles

1. **An LLM is used only at the entry**, to disambiguate LaTeX the grammar cannot
   parse (implicit multiplication, scope of `\sin x y`, index conventions) and,
   optionally, to OCR a formula image. Every decision it makes is written into
   the report's *assumptions* section.
2. **Everything after the entry is deterministic**: parsing, canonical-form
   normalization, differentiation, printing.
3. **Nothing is reported without a certificate**: round-trip
   (`parse(print(e)) == e`) plus random-point numeric comparison. A failed check
   still produces a report, with the failure stated in the conclusion.
4. **Boundaries are reported, not hidden.** Terms outside the supported language
   (e.g. the ν-dependence of a Student-t quantile) are named, with the reason,
   and handled numerically where possible.

## Report layout

| Section | Content |
|---|---|
| Source | the input LaTeX verbatim (and the image, if OCR was used) |
| Parsed form | the R expression and the canonical tree printed back to LaTeX |
| Assumptions | every disambiguation / OCR decision, one per line |
| Derivative | `grad` / `hessian` / `jacobian` result as LaTeX |
| Verification | round-trip status; number of test points, max relative error, pass/fail |
| Out of scope | sub-terms outside the language and why |
| Conclusion | CERTIFIED / NOT CERTIFIED, naming every failed check |

## Install

```r
# install.packages("remotes")
remotes::install_github("PsychQuantR/DefDiff")
remotes::install_github("PsychQuantR/latex2grad")
```

DefDiff currently builds on macOS only (its `src/Makevars` links Accelerate and
Metal). Optional: `httr2` and `jsonlite` for the Claude entry hooks, `pdflatex` to
compile reports.

## Usage

```r
library(latex2grad)

r <- latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"))
r
#> <latex2grad> gradient w.r.t. x, y
#>   source:      x^2 y + \sin(x)
#>   parsed:      x^{2} y + \sin\left(x\right)
#>   assumptions: 0
#>   derivative:
#>     \frac{\partial f}{\partial x} = y \cdot 2 x + \cos\left(x\right)
#>     \frac{\partial f}{\partial y} = x^{2}
#>   out of scope: none
#>   certificate: CERTIFIED: every printed tree round-trips, and the derivative agrees
#>   with Richardson central differences at 25 random points (max relative error ...).

as_function(r)(x = 1, y = 2)          # evaluate the derivative
l2g_report(r, file = "report.tex")    # LaTeX report (pdflatex report.tex)

latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"), type = "hessian")
latex2grad("\\begin{pmatrix} x y \\\\ x^2 \\end{pmatrix}", wrt = c("x", "y"), type = "jacobian")
```

Other arguments: `where` (LaTeX definitions substituted into the formula),
`domain` (sampling boxes for the numeric check), `strict` (ambiguities become
errors), `disambiguate = llm_claude()` and `image = , ocr = ocr_claude()` (the
optional LLM entry; decisions land in *Assumptions*).

### Worked example: Student-t copula density, ∂/∂ν

[`inst/examples/t-copula.R`](inst/examples/t-copula.R) differentiates the
bivariate t-copula density with respect to ν, with `x_i = t_\nu^{-1}(u_i)`
supplied through `where`. The three Γ terms and the two quantiles are outside
DefDiff's language: they are listed under *Out of scope* and their ν-derivatives
are taken numerically, while the rest of the chain rule is symbolic. The result is
certified (max relative error ≈ 1e-12 at 25 random points) and agrees with an
independent implementation of the density.

## How it works

- **Grammar** (`parse_latex()`): a precedence parser. Conventions it applies
  (`2xy`, `\sin x y → \sin(x y)`, `a/bc → a/(b c)`, `\sin^2 x`, Euler's `e`,
  `\Phi`, `t_\nu^{-1}`, finite `\sum`/`\prod` expansion, …) are recorded as
  assumptions; `strict = TRUE` turns the ambiguous ones into errors.
- **Canonical form** (`normalize_tree()`, `to_latex()`): a fixpoint of local
  rewrites (exact rational folding, identities, signs, associativity, DefDiff
  artifact cleanup) and a printer such that `normalize(parse(print(e))) == e`.
- **Differentiation**: every derivative is a `DefDiff::grad()` call on the formula
  lifted as `sum(f)` over length-1 variables. `tan`, `sinh`, `asin`, `dnorm`, …
  are first lowered into DefDiff's catalog by exact identities. Hessians are
  iterated gradients (both triangles computed); Jacobians are per-component
  gradients.
- **Boundary**: sub-terms whose head is not a DefDiff generator become opaque
  symbols; the chain rule is completed with explicit `numeric_partial()` nodes
  (Richardson-extrapolated central differences), printed as
  `\left.\frac{\partial}{\partial v}[\,\cdot\,]\right|_{\mathrm{num}}`.
- **Certificate**: round-trip of every printed tree, DefDiff closure levels, and a
  seeded random-point comparison (`|a − b| / max(1, |b|)` ≤ `tol`). An entry that
  DefDiff refused entirely is never certified, since comparing a numeric
  derivative with finite differences would be circular.

## Known DefDiff 0.2.0 limitations (worked around, to be fixed upstream)

- `grad(sum(a^b), v)` with `v` only in the base and a symbolic exponent fails
  ("non-numeric argument to binary operator"); latex2grad retries such powers as
  `exp(b log a)` and records a note.
- A symbolic power that does not contain the variable makes the walker fail
  ("shim with no pullback"); latex2grad freezes variable-free sub-trees into
  constant symbols before calling DefDiff.
- `hessian()` builds `diag(<scalar>)` blocks, which is an n×n identity for
  length-1 variables; latex2grad does not use it.
- `extend_language()` L_3 rules are not consulted inside `sum()`.

## Development

Specs and change proposals are managed with Spectra (`openspec/`); the first
change is `openspec/changes/add-latex2grad-pipeline/`.

## Status

v0.1.0: first runnable release (see `NEWS.md`).

## License

MIT
