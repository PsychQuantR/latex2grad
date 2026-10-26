## ADDED Requirements

### Requirement: LaTeX report with the fixed section layout

The system SHALL provide `l2g_report(x, file = NULL)` that renders a `latex2grad` result as
a standalone LaTeX document containing, in order, the sections Source, Parsed form,
Assumptions, Derivative, Verification, Out of scope, and Conclusion, and SHALL write it to
`file` when given.

#### Scenario: sections are present and ordered

- **WHEN** `l2g_report(latex2grad("x^2", wrt = "x"))` is called
- **THEN** the output SHALL contain the seven section headings in the order above

#### Scenario: source is reproduced verbatim

- **WHEN** the result came from LaTeX input
- **THEN** the Source section SHALL contain the input LaTeX verbatim (in a verbatim block)
  and its rendering
- **AND** when the result came from an image, the section SHALL include the image and name
  the OCR model

#### Scenario: empty sections are explicit

- **WHEN** there are no assumptions or no out-of-scope terms
- **THEN** the corresponding section SHALL say so explicitly instead of being omitted

#### Scenario: report compiles

- **WHEN** the report of the t-copula worked example is compiled with `pdflatex`
- **THEN** compilation SHALL succeed using only `amsmath`, `amssymb`, `graphicx`, and
  `booktabs`

### Requirement: Conclusion states the certificate outcome

The Conclusion SHALL state whether the derivative is certified, and on failure SHALL name
each failed check (round-trip of which entries, numeric error versus tolerance, or
insufficient valid points).

#### Scenario: failed certificate

- **WHEN** the numeric certificate failed
- **THEN** the Conclusion SHALL begin with "NOT CERTIFIED" and give the maximum relative
  error and the tolerance
