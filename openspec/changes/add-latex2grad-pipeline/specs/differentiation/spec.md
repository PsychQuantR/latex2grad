## ADDED Requirements

### Requirement: Derivatives are computed by DefDiff

The system SHALL compute every symbolic derivative with `DefDiff::grad()` and MUST NOT
contain its own derivative rules. A scalar formula SHALL be lifted as `sum(f)` over
length-1 variables before the call, and the result SHALL be passed through the canonical
cleanup.

#### Scenario: gradient of a polynomial

- **WHEN** `latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"))` is called
- **THEN** the derivative entries SHALL evaluate to `2xy + cos(x)` and `x^2` at every test point
- **AND** each entry SHALL be printed as LaTeX

#### Scenario: wrt is checked against the formula

- **WHEN** a `wrt` entry is written as a LaTeX command such as `"\\nu"`
- **THEN** it SHALL be read as the symbol `nu`, with a note
- **AND WHEN** a `wrt` variable does not occur in the formula (for example because `where`
  replaced it)
- **THEN** the system SHALL warn and record a note, and its derivative SHALL be 0

#### Scenario: free symbols not in wrt are parameters

- **WHEN** `latex2grad("a x^2", wrt = "x")` is called
- **THEN** `a` SHALL be treated as a constant parameter and listed as such in the result

### Requirement: Hessian and Jacobian types

The system SHALL support `type = "hessian"`, computed entrywise as the DefDiff gradient of
each DefDiff gradient entry (both triangles computed independently), and
`type = "jacobian"` for vector-valued input written as `pmatrix`/`bmatrix`, computed as
per-component gradients. A scalar input with `type = "jacobian"` SHALL yield a 1×n row.

#### Scenario: Hessian of a two-variable formula

- **WHEN** `latex2grad("x^2 y + \\sin(x)", wrt = c("x", "y"), type = "hessian")` is called
- **THEN** the result SHALL be a 2×2 matrix of trees equal to `2y − sin(x)`, `2x`, `2x`, `0`

#### Scenario: Jacobian of a vector-valued formula

- **WHEN** `latex2grad("\\begin{pmatrix} x y \\\\ x^2 \\end{pmatrix}", wrt = c("x", "y"), type = "jacobian")`
  is called
- **THEN** the result SHALL be the 2×2 matrix `[[y, x], [2x, 0]]`

### Requirement: Adapting formulas to DefDiff without own derivative rules

Before each DefDiff call the system SHALL freeze maximal sub-trees that do not contain
the differentiation variable into constant symbols, and SHALL substitute them back
afterwards. When DefDiff fails on a power `a^b` whose base contains the variable and
whose exponent is symbolic and free of it, the system SHALL retry that power as
`exp(b * log(a))`, convert the result back to `a^b`, and record a note stating the
`a > 0` validity condition.

#### Scenario: symbolic exponent in the base variable

- **WHEN** `latex2grad("x^{y}", wrt = c("x", "y"))` is called
- **THEN** the derivative with respect to `x` SHALL evaluate to `y x^{y-1}`
- **AND** the result SHALL carry a note about the exp-log rewrite
- **AND** no term SHALL be listed as out of scope

### Requirement: Definitional lowering into the DefDiff catalog

Before differentiation, the system SHALL rewrite `tan`, `cot`, `sec`, `csc`, `sinh`,
`cosh`, `asin`, `acos`, and `dnorm` by exact identities into DefDiff catalog functions,
and SHALL record each rewrite applied.

#### Scenario: tangent is lowered

- **WHEN** `latex2grad("\\tan x", wrt = "x")` is called
- **THEN** the lowered tree SHALL be `sin(x)/cos(x)`
- **AND** the rewrite SHALL be listed in the result's lowering record
- **AND** the derivative SHALL be computed without any out-of-scope term
