<!-- SPECTRA:START v1.0.2 -->

# Spectra Instructions

This project uses Spectra for Spec-Driven Development(SDD). Specs live in `openspec/specs/`, change proposals in `openspec/changes/`.

## Use `/spectra-*` skills when:

- A discussion needs structure before coding → `/spectra-discuss`
- User wants to plan, propose, or design a change → `/spectra-propose`
- Tasks are ready to implement → `/spectra-apply`
- There's an in-progress change to continue → `/spectra-ingest`
- User asks about specs or how something works → `/spectra-ask`
- Implementation is done → `/spectra-archive`
- Commit only files related to a specific change → `/spectra-commit`

## Workflow

discuss? → propose → apply ⇄ ingest → archive

- `discuss` is optional — skip if requirements are clear
- Requirements change mid-work? Plan mode → `ingest` → resume `apply`

## Parked Changes

Changes can be parked（暫存）— temporarily moved out of `openspec/changes/`. Parked changes won't appear in `spectra list` but can be found with `spectra list --parked`. To restore: `spectra unpark <name>`. The `/spectra-apply` and `/spectra-ingest` skills handle parked changes automatically.

<!-- SPECTRA:END -->

## Project notes

- R package; DefDiff (PsychQuantR/DefDiff) does all symbolic differentiation.
  Never add derivative rules here — extend DefDiff upstream instead.
- The `tex_*` / `normalize.R` layer is the interim DefTeX implementation; keep
  `parse_latex()`, `normalize_tree()`, `to_latex()` signatures stable.
- LLM calls are allowed only at the entry (`llm.R`); everything after parsing is
  deterministic.
- Check: `R CMD build . && NOT_CRAN=true R CMD check --no-manual latex2grad_*.tar.gz`.
- Commits reference issues as `(#N)` or `Refs #N` (never close keywords).
