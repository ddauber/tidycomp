<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycomp

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/github/ddauber/tidycomp/graph/badge.svg?token=WA8GG1ECZ7)](https://codecov.io/github/ddauber/tidycomp)
<!-- badges: end -->

**tidycomp** provides a tidy, auditable pipeline for group comparisons.
A single grammar covers two-sample tests, paired analyses, and multi-group designs – all with consistent output and effect sizes.

```
comp_spec() → set_*() → diagnose() → prepare() → test() → effects() → report()/tidy()/autoplot()
```

## Why tidycomp?

- **Transparent**: inspect your data before acting.
- **Standardised**: engines return the same tidy schema.
- **Pragmatic**: sensible defaults like Welch’s t-test.
- **Composable**: mix and match engines without changing surrounding code.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ddauber/tidycomp")
```

## Quick example

``` r
library(tidycomp)

spec <- comp_spec(mtcars) |>
  set_roles(outcome = mpg, group = am) |>
  set_design("independent") |>
  set_outcome_type("numeric") |>
  diagnose() |>
  prepare(steps = list(step_trim_outliers(mpg, method = "mad"))) |>
  test() |>
  effects()

report(spec)
```

## Features

- Two-sample and paired comparisons, plus one-way and repeated-measures designs.
- Effect sizes for numeric and categorical tests (Hedges’ g, phi, Cramer’s V, odds ratio).
- Flexible outlier handling via `step_trim_outliers()` with IQR, MAD or SD fences.
- Friendly CLI messaging with `{cli}`.

## Core workflow

| Step | Function | Purpose |
|------|----------|---------|
| Create spec | `comp_spec()` | Declare data & meta information |
| Roles & design | `set_roles()`, `set_design()`, `set_outcome_type()` | Describe analysis |
| Optional prep | `prepare()` | Apply logged preprocessing steps |
| Run test | `test()` | Select and execute engine |
| Effect size & output | `effects()`, `report()`, `tidy()` | Summaries, tables, plots |

## Contributing

Bug reports and pull requests are welcome. By contributing, you agree to the [Code of Conduct](https://www.contributor-covenant.org/version/3/0/code_of_conduct/).

## License

MIT © [authors](LICENSE)

## Citation

If you use **tidycomp**, please cite the package and version.
