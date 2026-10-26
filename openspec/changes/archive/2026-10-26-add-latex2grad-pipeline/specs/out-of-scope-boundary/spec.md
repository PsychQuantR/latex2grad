## ADDED Requirements

### Requirement: Sub-terms outside the DefDiff language are named, not hidden

The system SHALL identify every maximal sub-term whose head is not a generator in
`DefDiff::language_catalog()` and SHALL list each one in the result's out-of-scope table
with its LaTeX, head function, affected variables, reason, and treatment.

#### Scenario: Student-t quantile in a density

- **WHEN** a formula contains `t_\nu^{-1}(u_1)` and is differentiated with respect to `nu`
- **THEN** the out-of-scope table SHALL contain an entry with head `qt`, variable `nu`, a
  reason stating that the quantile's ν-derivative has no closed form in the language, and
  treatment `numeric`

#### Scenario: out-of-scope term that does not depend on the variable

- **WHEN** a formula contains `\Gamma(a)` and is differentiated with respect to `x` only
- **THEN** the term SHALL be listed with treatment `constant w.r.t. x`
- **AND** no numeric-partial node SHALL be created for it

### Requirement: Chain rule across the boundary with numeric-partial nodes

The system SHALL abstract each out-of-scope sub-term `g` into an opaque symbol,
differentiate the abstracted formula with DefDiff (including with respect to the opaque
symbols), and complete the chain rule with an explicit `numeric_partial(g, "v", ...)` node.
Numeric-partial nodes SHALL evaluate by Richardson-extrapolated central differences, print
as a marked partial derivative, and SHALL be differentiable again by extending their
variable list.

#### Scenario: mixed symbolic and numeric derivative

- **WHEN** `latex2grad("x^2 \\Gamma(x)", wrt = "x")` is called
- **THEN** the derivative tree SHALL contain `2 * x * gamma(x)` and
  `x^2 * numeric_partial(gamma(x), "x")`
- **AND** it SHALL agree with central differences of the source within tolerance

#### Scenario: DefDiff refuses the lifted formula

- **WHEN** DefDiff signals an error other than an unknown generator for a variable
- **THEN** the whole formula SHALL be treated as one out-of-scope term for that variable
- **AND** DefDiff's message SHALL be the reported reason
