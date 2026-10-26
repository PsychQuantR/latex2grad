# canonical-form Specification

## Purpose
Give every expression tree one canonical form and a LaTeX printing, so that printing and re-parsing reproduce the tree exactly (the round-trip half of the certificate). This is the interim DefTeX layer.

## Requirements

### Requirement: Deterministic canonical normalization

The system SHALL provide `normalize_tree(expr)` that rewrites an expression tree to a
canonical form by applying local, meaning-preserving rewrites to a fixpoint: integer to
double, numeric constant folding, identity elimination, sign normalization,
left-associative flattening of sums and products, and removal of DefDiff evaluation
artifacts. The function MUST be idempotent and MUST NOT reorder operands.

#### Scenario: identities and constants are folded

- **WHEN** `normalize_tree(quote(0 + 1 * x^1 + (2 - 2) * y))` is called
- **THEN** the result SHALL be `x`

#### Scenario: DefDiff artifacts are removed for scalar variables

- **WHEN** `normalize_tree(quote(rep(1, length(x^2 * y)) * y * 2 * x^1 + rep(0, length(x))))`
  is called
- **THEN** the result SHALL be `y * 2 * x`

#### Scenario: kernel calls are mapped back to arithmetic

- **WHEN** the tree contains `fast_vec_mul(a, b)`, `fast_vec_add(a, b)`, `fast_vec_sub(a, b)`,
  `fast_scalar_mul(s, v)`, `fast_scalar_div(a, b)`, or `fast_vv_<f>(a)`
- **THEN** they SHALL be rewritten to `a * b`, `a + b`, `a - b`, `s * v`, `a / b`, and `f(a)`

#### Scenario: idempotence

- **WHEN** `normalize_tree` is applied to its own output
- **THEN** the result SHALL be identical to the input

### Requirement: LaTeX printing with the round-trip property

The system SHALL provide `to_latex(expr)` that prints a canonical tree as LaTeX such that
`normalize_tree(parse_latex(to_latex(e))$expr)` is identical to `e` for every canonical
tree the package produces, including derivative trees and numeric-partial nodes.

#### Scenario: precedence-aware printing

- **WHEN** `to_latex(quote(-(a + b) * c^(x + 1)))` is called
- **THEN** the output SHALL parenthesize `a + b` and brace the exponent `x + 1`
- **AND** re-parsing and normalizing the output SHALL reproduce the tree

#### Scenario: numbers print exactly

- **WHEN** a tree contains the double `1/3` or `0.123`
- **THEN** `to_latex` SHALL print `\frac{1}{3}` and `0.123` respectively
- **AND** re-parsing SHALL yield the identical double
