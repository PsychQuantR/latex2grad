## ADDED Requirements

### Requirement: Round-trip certificate

The system SHALL check, for the source tree and for every derivative entry, that
`normalize_tree(parse_latex(to_latex(e))$expr)` is identical to `e`, and SHALL record the
status of each check.

#### Scenario: round-trip passes for a derivative

- **WHEN** `latex2grad("x^2 y", wrt = c("x", "y"))` completes
- **THEN** the certificate SHALL report round-trip `TRUE` for the source and both entries

### Requirement: Random-point numeric certificate

The system SHALL evaluate the symbolic derivative at `n_points` seeded random points drawn
from per-symbol `domain` intervals and compare it against Richardson-extrapolated central
differences, reporting the number of valid and rejected points, the maximum relative error
`|a − b| / max(1, |b|)`, the tolerance, and pass/fail. Hessian entries SHALL be compared
against central differences of the symbolic gradient.

#### Scenario: correct derivative passes

- **WHEN** `latex2grad("\\exp(x y)", wrt = c("x", "y"), n_points = 20, seed = 1)` completes
- **THEN** the numeric certificate SHALL pass with max relative error at most `1e-6`
- **AND** repeating the call SHALL reproduce the identical error value

#### Scenario: corrupted derivative fails and still reports

- **WHEN** a derivative entry is replaced by an incorrect tree and the certificate is
  recomputed
- **THEN** the numeric certificate SHALL fail
- **AND** the report SHALL still be produced with the failure stated in its conclusion

#### Scenario: an entirely numeric entry is never certified

- **WHEN** DefDiff refused the formula for an entry, so the entry is a single
  numeric-partial node of the whole formula
- **THEN** the certificate SHALL fail and name that entry, because comparing it with
  central differences of the same formula is circular

#### Scenario: points outside the formula's domain are rejected

- **WHEN** the formula is non-finite at a sampled point (for example `\log(x)` sampled at a
  negative `x` from a user domain)
- **THEN** the point SHALL be rejected and redrawn, and the rejected count SHALL be reported
- **AND** when too few points are valid, the failure SHALL quote the first evaluation problem

### Requirement: Closure level is reported

The system SHALL report `DefDiff::level()` of the lowered source and of each derivative
entry, and the number of entries that contain numeric-partial nodes.

#### Scenario: closure preserved for an in-language formula

- **WHEN** `latex2grad("\\sin(x)^2", wrt = "x")` completes
- **THEN** the level of the derivative SHALL be at most the level of the source
