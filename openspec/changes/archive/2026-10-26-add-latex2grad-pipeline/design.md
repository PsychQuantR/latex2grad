## Context

`latex2grad` is the public product layer over two DAT components:

- **DefDiff 0.2.0** (PsychQuantR/DefDiff) — closure-preserving symbolic differentiation.
  Its language is vector-first: a variable is a numeric vector and scalar-valued formulas
  are reductions (`sum`, `crossprod`) of elementwise L_3 expressions. Catalog:
  L_0 `+ - * / rep`, L_1 `sum crossprod ^`, L_2 `%*% t`, L_3
  `sin cos exp log sqrt tanh atan`, L_4 `integral implicit`.
- **DefTeX** — the planned LaTeX ↔ tree parser/normalizer/printer. No public mirror yet.

Facts established by probing DefDiff 0.2.0 on this branch (Linux, Accelerate/Metal stubbed):

| Probe | Result |
|---|---|
| `grad(quote(x^2), "x")` | error — bare variable at scalar position |
| `grad(quote(sum(x^2 * y)), "x")` | `rep(1, length(x^2 * y)) * y * 2 * x^1 + rep(0, length(x))` |
| `grad(quote(sum(lgamma(nu))), "nu")` | `DefDiff_not_definable` (unknown generator inside `sum`) |
| `level(quote(sum(lgamma(x))))` | `"unknown"` |
| `hessian(function(x, y) sum(x^2*y + sin(x)))(1, 2)$x$x` | 3×3 identity (`diag(3.16)`), expected 1×1 `3.16` |
| `extend_language("L_3", "erf", …)` then `grad(quote(sum(erf(v))), "v")` | rule ignored: `.grad_inner` has a hard-coded L_3 switch |
| `grad(quote(sum(cos(x) * sin(x))), "x")` | returns `fast_vec_*` kernel calls even through `grad.call` |
| `grad(quote(sum(x^y)), "x")` | `simpleError`: non-numeric argument to binary operator (found during apply) |
| `grad(quote(sum(a / q^m)), "a")` | `DefDiff_not_definable`: walker returned a shim with no pullback; `sum(a / q^2)` and `sum(a * (1 / q^m))` work (found during apply) |

## Goals / Non-Goals

### Goals

- A runnable, tested v0.1.0 that satisfies the README's four principles.
- Every number in the report is reproducible (seeded) and every convention is visible.
- DefDiff does all symbolic differentiation; latex2grad adapts, never re-derives.

### Non-Goals

- Matrix calculus, symbolic-bound sums, integrals/limits in the input language.
- A CAS-grade simplifier.
- Fixing DefDiff from here (findings are filed upstream instead).

## Decisions

### D1: DefTeX functionality lives in an internal `tex_*` layer (interim)

The parser, printer, and normalizer are implemented in latex2grad
(`R/tex_tokenize.R`, `R/tex_parse.R`, `R/tex_print.R`, `R/normalize.R`) behind three
exported entry points (`parse_latex()`, `to_latex()`, `normalize_tree()`).

hard-code exception: timing decoupling — the DefTeX public mirror is announced in the
README but not shipped. When it ships, these three functions become thin delegations to
DefTeX and the internal layer is deleted; the exported signatures are chosen so callers
do not change.

**Alternatives considered**: block v0.1.0 on DefTeX (rejected — no ETA); vendor a
private DefTeX snapshot (rejected — not available to this repo).

### D2: Scalar lifting — `sum(f)` over length-1 variables

DefDiff rejects a bare variable at a scalar position, but `sum(·)` of a length-1 vector is
the identity, so a scalar formula `f(x, y, …)` is differentiated as
`DefDiff::grad(call("sum", f), v)`. The result is specialized back to scalars by the
canonical cleanup (`rep(c, length(·)) → c`, `sum(e) → e`, `as.numeric(e) → e`,
`fast_vec_mul(a, b) → a * b`, … — see D5).

### D3: Hessian by iterated `grad`; Jacobian by per-component `grad`

`DefDiff::hessian()` wraps second derivatives in `diag(<expr>)`, which for a length-1
variable evaluates `diag(3.16)` → a 3×3 identity. The Hessian is therefore built as
`H[i, j] = grad(grad(f, x_j), x_i)` using `DefDiff::grad` twice; the symmetric partner is
computed independently (not copied) so the certificate can check both. Jacobian rows are
per-component gradients of a `c(f_1, …, f_m)` tree.

### D4: Definitional lowering before differentiation

Exact identities move common functions into DefDiff's catalog and are listed in the
report (Parsed form → "Lowered for DefDiff"):

| Source | Lowered |
|---|---|
| `tan(a)` / `cot` / `sec` / `csc` | `sin(a)/cos(a)` / `cos/sin` / `1/cos` / `1/sin` |
| `sinh(a)`, `cosh(a)` | `(exp(a) - exp(-a))/2`, `(exp(a) + exp(-a))/2` |
| `asin(a)`, `acos(a)` | `atan(a/sqrt(1 - a^2))`, `pi/2 - atan(a/sqrt(1 - a^2))` (valid on (−1, 1)) |
| `dnorm(a)` | `exp(-a^2/2)/sqrt(2*pi)` |

### D5: Canonical form = fixpoint of local, meaning-preserving rewrites

Bottom-up to a fixpoint: integer → double, numeric constant folding of `+ - * / ^`
(negative results represented as unary minus of a positive literal), identity
elimination (`0 + a`, `a * 1`, `a^1`, …), sign normalization (`a + (-b) → a - b`,
`(-a) * b → -(a * b)`, …), left-associative flattening of `+`/`-` and `*`, and
DefDiff-artifact cleanup. Operand order is never sorted, so the canonical form stays close
to the source. Function applications on constants (`sqrt(2)`, `log(2)`, `pi`) are kept
symbolic.

The printer is precedence-aware, always writes division as `\frac`, function arguments as
`\left( … \right)`, and numbers as integers, small exact rationals (`\frac{p}{q}`,
q ≤ 1000) or the shortest decimal that parses back to the same double. Together these
make `normalize(parse(print(e)))` identical to `e`.

### D6: Out-of-scope boundary = opaque symbols + numeric-partial nodes

A sub-term is out of scope when its head is not a DefDiff generator (checked against
`DefDiff::language_catalog()`; `DefDiff::level()` is `"unknown"`). Maximal such sub-terms
`g_k` are replaced by fresh symbols `.o<k>`; then

```
∂f/∂v = DefDiff(f[o], v) + Σ_k DefDiff(f[o], .o<k>) · numeric_partial(g_k, "v")
```

and `.o<k>` is substituted back. `numeric_partial(g, "v", …)` is an explicit tree node:
it prints as `\left.\frac{\partial}{\partial v}\left[g\right]\right|_{\mathrm{num}}`,
evaluates by Richardson-extrapolated central differences, and differentiating it again
extends its variable list (so Hessians stay well-defined). If DefDiff still refuses a
lifted formula, the whole formula is treated as one out-of-scope term for that variable.
Every term is reported with head, variables, reason, and treatment.

### D7: Certificate

1. **Round-trip** — `normalize(parse(print(e))) == e` for the source tree and every
   derivative entry.
2. **Closure level** — `DefDiff::level()` of the lowered source and of each derivative
   entry; reported (informational — numeric-partial nodes are outside the language by
   construction and are counted separately).
3. **Numeric** — `n_points` (default 25) seeded points drawn uniformly from per-symbol
   `domain` boxes (default `[0.5, 1.5]`); non-finite points are rejected and redrawn (bounded
   attempts). Gradient/Jacobian entries are compared to Richardson central differences of
   the source; Hessian entries to central differences of the (separately verified) symbolic
   gradient. Error metric: `|a − b| / max(1, |b|)`. Pass iff every entry's max error ≤
   `tol` (default 1e-6) and at least half the requested points were valid.

The certificate passes iff all round-trips hold and the numeric check passes. The report's
conclusion states which part failed.

### D7a: Adapting formulas to DefDiff 0.2.0 (found during apply)

Two DefDiff failures surfaced on the t-copula example, and both are handled
without adding derivative rules to latex2grad:

- **Constant freezing**: before every DefDiff call, maximal sub-trees free of the
  differentiation variable become constant symbols `.c<k>` and are substituted back
  afterwards. A constant is a constant, so this is exact, and DefDiff's walker never
  meets a variable-free power with a symbolic exponent (it builds no pullback for
  one, which breaks the enclosing division).
- **exp-log retry**: if DefDiff still fails and the formula has a power `a^b` with the
  variable in `a` and a symbolic `b` free of it, that power is retried as
  `exp(b * log(a))` (valid for `a > 0`); the result is turned back into `a^b` and a
  note is recorded in the result and the report.

Both behaviours are reported upstream: PsychQuantR/DefDiff#2 (power rule) and
PsychQuantR/DefDiff#3 (division by a symbolic power). The `hessian()` identity-block bug
is PsychQuantR/DefDiff#4, `extend_language()` inside `sum()` is PsychQuantR/DefDiff#5,
and the Linux build is PsychQuantR/DefDiff#6.

### D7b: An entirely numeric entry is never certified

If DefDiff refuses a formula outright, the entry becomes `numeric_partial(f, "v")`.
Comparing it with central differences of `f` checks nothing (it passed with error 0
in the first implementation), so such entries are a certificate failure, named in
the conclusion.

### D8: LLM only at the entry; decisions are data

`latex2grad(..., disambiguate = llm_claude())`: when the grammar raises a parse error, or
(with `strict = TRUE`) an ambiguity, the hook receives the LaTeX and the problem and
returns `list(latex = <rewritten>, decisions = <character>)`; the rewritten LaTeX is parsed
deterministically, and each decision is recorded as an assumption tagged with the model
id. `ocr = ocr_claude()` transcribes `image =` to LaTeX and records the transcription.
`llm_claude()` / `ocr_claude()` use raw HTTP via httr2 (R has no official Anthropic SDK):
`claude-opus-5-5`, `anthropic-version: 2026-10-21`, structured JSON output
(`output_config.format`), server-side refusal fallback (`fallbacks: "default"`), and an
explicit `stop_reason == "refusal"` check. The transport is injectable for tests; httr2 is
Suggests-only.

### D9: Deterministic grammar conventions (each recorded as an assumption when used)

Implicit multiplication (`2xy`); an unbraced function argument absorbs the following
implicit product (`\sin x y → sin(x y)`); implicit products bind tighter than an explicit
`/` (`a/bc → a/(b c)`); `\sin^2 x → (\sin x)^2`; `e` → Euler's number unless declared a
variable; `\Phi(·)` → standard normal CDF; `t_\nu^{-1}(u)` → Student-t quantile `qt(u, ν)`;
`f(x)` for an undeclared single-letter `f` → multiplication; finite sums/products with
integer bounds are expanded; plain-letter `sin`/`cos`/`exp`/`log`/`ln` runs → functions.

## Risks / Trade-offs

- **Interim parser drift from DefTeX** → mitigated by D1's thin-delegation plan and by
  keeping the exported surface small.
- **DefDiff output is verbose for scalars** → canonical cleanup (D5) handles the observed
  artifact shapes; anything it misses still evaluates correctly and is still verified.
- **Numeric partials make the numeric check partially circular** → the report states how
  many derivative entries contain numeric-partial nodes; an entry that is *entirely*
  numeric fails the certificate (D7b).
- **Long derivatives are unreadable** → the report folds `where` definitions back into
  their symbols and names repeated large sub-trees `A_1, A_2, …` (display only; the
  certified trees stay fully expanded).
- **DefDiff builds only on macOS** → Linux CI needs an upstream portability fix
  (PsychQuantR/DefDiff#6).

## Migration Plan

New package; nothing to migrate. When DefTeX ships: replace the bodies of
`parse_latex()`, `to_latex()`, `normalize_tree()` with DefTeX calls, keep the tests.

## Open Questions

- Should DefDiff's catalog grow a closed Γ family (`lgamma → digamma → psigamma(·, k)`)?
  That would move the t-copula Γ terms in-scope (upstream decision).
- Should the numeric tolerance adapt when numeric-partial nodes are present?
