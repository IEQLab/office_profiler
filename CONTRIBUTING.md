# Contributing to office_profiler

Thanks for your interest. This repository is the public companion to a
SocialSys'26 workshop paper, so the scope is intentionally narrow: code,
synthetic demo data, and the paper source for one specific contribution
(LLM capacity for structured extraction from occupant feedback).

## Reporting issues

Bug reports and reproducibility issues are very welcome. When opening an
issue, please include:

- Your operating system and R version (`sessionInfo()`).
- The Ollama version and the exact model tags you have pulled.
- A minimal reproducer (the smallest code block or `tar_make(<target>)` that
  shows the problem).
- The full error message, including the traceback if you have one
  (`rlang::last_trace()`).

## Pull requests

Before opening a PR, please open an issue first so we can agree on the scope
of the change. Substantive changes that go beyond the SocialSys'26 paper's
scope are unlikely to be merged here — they would belong in a follow-up
project.

If you do open a PR:

- Keep the change focused and small.
- Match the existing style (tidyverse, native pipe `|>`, snake_case).
- Run the synthetic pipeline (`targets::tar_make()`) to confirm nothing
  broke. Mention this in the PR description.

## Reproducing the published results

The published numerical results require the licensed CBE Occupant Survey
database, which we cannot redistribute. See `data/raw/README.md` for how to
request access.

## Code of conduct

Be kind. We follow the
[Contributor Covenant Code of Conduct, v2.1](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
