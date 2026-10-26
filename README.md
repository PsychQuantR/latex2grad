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
  differentiation over the L_3 expression language (plus the L_4 integral and
  implicit nodes)
- a LaTeX ↔ expression-tree parser / normalizer / printer. DefTeX's public
  mirror is not out yet, so latex2grad ships its own (`tex_parse()`,
  `tex_normalize()`, `tex_print()`); the pipeline only needs those three
  functions, so it can switch to DefTeX once DefTeX is published.

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

## Install

```r
# install.packages("remotes")
remotes::install_github("PsychQuantR/DefDiff")
remotes::install_github("PsychQuantR/latex2grad")
```

DefDiff currently builds on macOS (its `src/Makevars` links Accelerate and
Metal). latex2grad itself is pure R. The optional Claude entry needs `httr2`
and `jsonlite`.

## Usage

```r
library(latex2grad)

r <- latex2grad(
  "f(\\nu) = \\ln\\Gamma\\left(\\frac{\\nu+1}{2}\\right) - \\ln\\Gamma\\left(\\frac{\\nu}{2}\\right)
            - \\frac{\\nu+1}{2} \\ln\\left(1 + \\frac{x^2}{\\nu}\\right)",
  domain = list(nu = c(1, 30))
)
r
#> latex2grad report (grad)
#>   ...
#>   Derivative
#>     d/dnu = digamma((nu + 1)/2)/2 - digamma(nu/2)/2 - log(1 + x^2/nu)/2 + (nu + 1) * x^2/(2 * (1 + x^2/nu) * nu^2)
#>   Verification
#>     round-trip   formula ok; derivatives 1/1
#>     numeric      20 point(s) (20 drawn), max rel. error 2.61e-11, tol 1e-06 -> pass
#>   Out of scope
#>     - lgamma, its argument: `lgamma` is not in DefDiff's L_0-L_4 generator catalog; closed form: digamma(a)
#>   Conclusion   Verified. ...

write_report(r, "report.tex")   # LaTeX document; .md for Markdown
```

- `op = "hessian"` gives all second partials; a vector
  (`\begin{pmatrix} ... \end{pmatrix}`) gives the Jacobian.
- `wrt` takes R names (`"nu"`, `"u_1"`) or LaTeX spellings (`"\\nu"`). The
  default is the left-hand side's variables (`c(u_1, u_2; \rho, \nu) = ...`:
  the ones before `;`), otherwise every free symbol.
- `domain` sets sampling ranges for the numeric check. Points where the
  formula is undefined are redrawn.
- `choices` overrides a grammar decision by its id, which the report shows
  next to each assumption, e.g. `choices = list("fnarg@1" = "first factor only")`.

The worked example from the design issue (bivariate Student-t copula density,
∂/∂ν) is in [`inst/examples/student_t_copula.R`](inst/examples/student_t_copula.R).
Everything is symbolic except the ν-dependence of t<sub>ν</sub><sup>-1</sup>(u),
which is reported as out of scope and evaluated numerically. In this
session's run, the result agreed with central differences to 6e-11 at 20 random
points.

## Report layout

| Section | Content |
|---|---|
| Source | the input LaTeX verbatim (and the image, if OCR was used) |
| Parsed form | the R expression and the canonical tree printed back to LaTeX |
| Assumptions | every disambiguation / OCR decision, one per line |
| Derivative | `grad` / `hessian` / `jacobian` result as LaTeX |
| Verification | round-trip status; number of test points, max relative error, pass/fail |
| Out of scope | sub-terms outside the language and why |

Long results are broken across lines, and repeated sub-expressions are
abbreviated (*where A₁ = …*). This affects display only: the certificate is
computed on the full expressions.

## How it works

**Parsing** (`tex_parse()`) is a recursive-descent grammar over LaTeX math. It
covers numbers; letters with subscripts; Greek letters; `+ - \cdot \times /
^`; `\frac`, `\sqrt`, `\sqrt[n]`; `( )`, `\left( \right)`, `|x|`; elementary
functions with or without parentheses (`\sin x`, `\sin^2 x`, `\sin^{-1} x`,
`\log_b x`); `\Gamma`, `\psi`, `\psi_1`, `\Phi`, `\phi`, `\Phi^{-1}`,
`t_\nu^{-1}`, `\mathrm{B}`, `\binom`, `n!`; `\operatorname{f}(…)` for any R
function; definite integrals `\int_a^b … \,\mathrm{d}t`; `\sum` / `\prod` with
integer bounds (expanded); evaluation bars
`\left.\frac{\partial}{\partial s} f\right|_{s=a}`; and an optional
left-hand side `f(x, y; params) =`. Wherever the input admits more than one
reading (`xy`, `\sin x y`, `e`, `\log`, `\Phi(·)`, `1/2x`, …), the grammar
takes a default and records the ambiguity with its alternatives.

**Normalization** (`tex_normalize()`) produces the canonical tree: no
parentheses, signs outside products, numeric coefficients first, like
factors combined, identities folded. `parse → normalize` inverts
`tex_print()` on canonical trees, and this is the round-trip certificate.

**Differentiation** (`l2g_differentiate()`) calls DefDiff on every
sub-expression it can handle. A scalar formula goes to DefDiff as
`grad(sum(e), v)`, and the vector-API artifacts in the result (`rep(1,
length(·))`, fast-path kernels) are removed by the normalizer. Where DefDiff
refuses a node, latex2grad builds the chain rule around it and takes each
local partial from, in order:

1. DefDiff itself, on the node with fresh symbols as arguments;
2. a supplementary closed-form table (the gamma family, normal and Student-t
   functions, `tan`, `abs`, …, and a power with a symbolic exponent);
3. a numeric derivative node `numd(body, s, at)` (Richardson extrapolation).

Cases 2 and 3 are listed under *Out of scope* with DefDiff's reason.

**Verification** prints and re-parses every expression, then compares every
derivative with a Richardson-extrapolated central difference at random
points: relative error `|s − n| / max(1, |n|)`, tolerance `1e-6`, widened to
`1e-4` when integrals or numeric nodes are involved. Hessian entries are
checked against numeric derivatives of the (separately verified) gradient.
The global RNG state is restored afterwards.

## LLM entry (optional)

```r
r <- latex2grad(image = "formula.png", llm = claude_entry())   # OCR
r <- latex2grad("\\sin x y + xy", llm = claude_entry())         # resolve ambiguities
```

`claude_entry()` uses the Messages API (`claude-opus-5-5`, JSON-schema
output, `ANTHROPIC_API_KEY`). It has three jobs, all at the entry:
transcribe an image, rewrite input the grammar rejects, and choose among
the readings the grammar recorded as ambiguous. Each decision appears in the
report's assumptions, attributed to the model. A refused or truncated answer
is an error. Any list with `ocr` / `rewrite` / `resolve` functions can be used
as `llm` in its place.

## Known DefDiff 0.2.0 boundaries found while building this

Reproduced against DefDiff 0.2.0 (the latex2grad engine works around each one):

- **Symbolic exponents.** A power with a symbolic exponent, differentiated
  with respect to its base, is refused. `grad(quote(sum(x^n)), "x")` fails
  with a plain `simpleError` ("non-numeric argument to binary operator"), not
  a typed condition; `(1 + x^2/ν)^{-(ν+1)/2}` w.r.t. `x` fails with "walker
  returned a shim with no pullback". latex2grad applies the power rule from
  its supplementary table.
- **`extend_language()` rules.** Inside `sum(·)` the elementwise walker never
  consults the catalog, so a registered L_3 function stays "unknown". At top
  level the rule is called as `rule(expr, var)` while the documented signature
  is `function(arg_expr, arg_grad)`. With the documented `erf` example,
  `grad(quote(erf(sum(v^2))), "v")` returns
  `2/sqrt(pi) * exp(-(erf(sum(v^2)))^2) * "v"`, which is a wrong result, not
  an error.
- **Variable-free unknown functions.** An out-of-catalog function that does
  not depend on the variable still makes the walker fail:
  `grad(quote(sum(sin(x) + lgamma(y))), "x")`. latex2grad short-circuits
  variable-free sub-expressions before calling DefDiff.

## Development

```r
devtools::test()            # parser, normalizer/printer, engine, pipeline, LLM entry (no network)
source("inst/examples/student_t_copula.R")
```

## License

MIT
