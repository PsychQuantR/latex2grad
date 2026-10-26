## ADDED Requirements

### Requirement: Deterministic LaTeX parsing into an expression tree

The system SHALL provide `parse_latex(latex, ...)` that converts a LaTeX math string into
an R expression tree using a deterministic grammar, and SHALL return the tree together
with the list of assumptions applied and the free symbols found. Parsing the same input
with the same options MUST always produce the identical tree and assumption list.

#### Scenario: explicit formula parses without assumptions

- **WHEN** `parse_latex("\\frac{1}{2} \\cdot x^{2} + \\sin\\left(y\\right)")` is called
- **THEN** the tree SHALL be equivalent to `(1/2) * x^2 + sin(y)`
- **AND** the assumption list SHALL be empty
- **AND** the free symbols SHALL be `x` and `y`

#### Scenario: Greek letters and subscripts become symbol names

- **WHEN** the input contains `\nu`, `x_1`, `x_{12}`, or `\sigma_{x}`
- **THEN** they SHALL become the symbols `nu`, `x_1`, `x_12`, and `sigma_x`

#### Scenario: unsupported construct is rejected with a typed error

- **WHEN** the input contains a sum with a symbolic upper bound such as `\sum_{i=1}^{n}`
- **THEN** the system SHALL signal a condition of class `latex2grad_parse_error` whose
  message names the construct

#### Scenario: Leibniz notation is rejected in every spelling

- **WHEN** the input contains `\frac{\partial}{\partial x}`, `\frac{d}{dx}`, or
  `\frac{\mathrm{d}}{\mathrm{d}x}`
- **THEN** the system SHALL signal `latex2grad_parse_error` instead of reading a quotient

### Requirement: Grammar conventions are recorded as assumptions

Whenever the grammar resolves a notational ambiguity by convention, the system SHALL
append one human-readable assumption describing the reading it chose. Covered
conventions: implicit multiplication, unbraced function-argument scope, implicit product
binding tighter than `/`, `\sin^2 x`, Euler's `e` (braced or not, including in scripts),
`\Phi` and `\Phi^{-1}`, `t_\nu^{-1}`, single-letter `f(x)`, finite sum/product expansion,
and plain-letter function names.

#### Scenario: superscripted function symbols stay functions

- **WHEN** `parse_latex("\\Phi^{-1}(u)")` or `parse_latex("\\Gamma^2(x)")` is called
- **THEN** the trees SHALL be `qnorm(u)` and `gamma(x)^2`, each with a recorded assumption

#### Scenario: implicit multiplication

- **WHEN** `parse_latex("2xy")` is called
- **THEN** the tree SHALL be equivalent to `2 * x * y`
- **AND** an assumption SHALL state that the juxtaposed letters `xy` were read as a product

#### Scenario: function argument scope

- **WHEN** `parse_latex("\\sin x y")` is called
- **THEN** the tree SHALL be `sin(x * y)`
- **AND** an assumption SHALL state that the unbraced argument absorbed the implicit product

#### Scenario: strict mode turns conventions into errors

- **WHEN** `parse_latex("\\sin x y", strict = TRUE)` is called
- **THEN** the system SHALL signal a condition of class `latex2grad_ambiguity`

### Requirement: LLM involvement is confined to the entry and recorded

The system SHALL call a user-supplied `disambiguate` hook only when deterministic parsing
fails or, in strict mode, raises an ambiguity. The hook's rewritten LaTeX SHALL be parsed
deterministically, and every decision it returns SHALL be recorded as an assumption that
names the model that served the request (the fallback model when a server-side fallback
answered). `where` definitions SHALL go through the same entry. No component after
parsing MAY call an LLM.

#### Scenario: disambiguation hook rewrites failing LaTeX

- **WHEN** `latex2grad("\\sin x y", wrt = "x", strict = TRUE, disambiguate = hook)` is
  called and `hook` returns `list(latex = "\\sin(x) y", decisions = "sin applies to x only")`
- **THEN** the parsed tree SHALL be `sin(x) * y`
- **AND** the assumptions SHALL contain `sin applies to x only` tagged with the hook's model

#### Scenario: definitions use the same entry

- **WHEN** `latex2grad(..., where = c(x_1 = "t_\\nu^{-1}(u_1)"), strict = TRUE, disambiguate = hook)`
  is called
- **THEN** the hook SHALL be consulted for the definition, and its decisions SHALL be
  recorded as assumptions prefixed with the defined symbol

#### Scenario: OCR entry records the transcription

- **WHEN** `latex2grad(image = "f.png", ocr = ocr_fn, wrt = "x")` is called
- **THEN** `ocr_fn("f.png")` SHALL supply the LaTeX source
- **AND** the assumptions SHALL record that the source was transcribed from the image and by
  which model

#### Scenario: Claude hooks are optional

- **WHEN** httr2 is not installed or no API key is available
- **THEN** `llm_claude()` and `ocr_claude()` SHALL fail with an actionable error only when
  called, and the rest of the package SHALL work without them
