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
  to follow)

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

## Status

Design stage. Nothing runnable yet. Worked example (Student's t copula density,
∂/∂ν) is in the design issue.

## License

MIT
