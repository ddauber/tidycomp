
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycomp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/github/ddauber/tidycomp/graph/badge.svg?token=WA8GG1ECZ7)](https://codecov.io/github/ddauber/tidycomp)
<!-- badges: end -->

**tidycomp** is a small, composable grammar for **group comparisons** in
R.  
Think *tidymodels*, but for t-tests, rank tests, and (later) ANOVA/χ² —
with a **transparent, auditable pipeline**:

    comp_spec() → set_*() → diagnose() → prepare() → test() → effects() → report()/autoplot()/tidy()

The MVP focuses on the most common comparison: the **two-sample
(unpaired) numeric** case with **Welch’s t-test** as the robust default.
There will be more engines in the future, including **paired** and
**rank tests**.

## Why tidycomp?

- **Transparent**: inspect first (`diagnose()`), then **explicitly**
  prepare data (`prepare()`).
- **Standardised**: engines return a single **tidy** result schema so
  downstream reporting is consistent.
- **Pragmatic by default**: **Welch’s t-test** handles variance
  heterogeneity well, with minimal cost.
- **Composable**: new engines slot in without changing user code
  before/after the test.
- **Friendly CLI messages**: all console notes and warnings use `{cli}`,
  providing consistent and enhanced feedback.

## Installation

You can install the development version of tidycomp from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ddauber/tidycomp")

# OR

# install.packages("pak")
pak::pak("ddauber/tidycomp")
```

## Quick start

``` r
library(tidycomp)

# Classic example: compare mpg by transmission (0 = auto, 1 = manual)
spec <- comp_spec(mtcars) |>
  set_roles(outcome = mpg, group = am) |>
  set_design("independent") |>
  set_outcome_type("numeric") |>
  set_strategy("auto") |>
  diagnose()

# Optional, explicit preparation (logged):
spec <- prepare(
  spec,
  steps = list(
    step_trim_outliers(mpg, k = 3, action = "winsorize")
  )
)

# Run the test (Welch default), add effect size, and report
spec <- test(spec) |> effects()
report(spec)
```

Outlier detection defaults to the IQR\*3 rule, but you can also set
`method = "mad"` or `method = "sd"` in `step_trim_outliers()` for median
absolute deviation or standard deviation fences, respectively (see
[r4np, dealing with
outliers](https://r4np.com/09_sources_of_bias.html#sec-dealing-with-outliers)).

> **Notes:** - If equal variances look plausible, tidycomp will inform
> you that Student’s t would likely agree.  
> - If n is very small *and* non-normality is severe, tidycomp will
> nudge you to consider Mann–Whitney.  
> - `diagnose()` never changes your data; `prepare()` is explicit and
> fully logged.

## Core grammar

| Step | Function | What it does |
|----|----|----|
| Create spec | `comp_spec()` | Container for data and analysis metadata |
| Declare roles | `set_roles()` | Map columns to roles (`outcome`, `group`, …) |
| Choose design | `set_design()` | `independent`, `paired`, `repeated`, `factorial` |
| Outcome family | `set_outcome_type()` | `numeric`, `binary`, `ordered`, `count` |
| Strategy (policy) | `set_strategy()` | `"auto"`, `"pragmatic"`, `"parametric"`, `"robust"`, `"permutation"` |
| (Optional) Force engine | `set_engine()` | `"welch_t"`, `"student_t"`, `"mann_whitney"`, … |
| Inspect | `diagnose()` | Size, variance proxy (BF), Shapiro flags, SDs, outlier counts |
| Prepare | `prepare()` | Apply explicit steps with a prep log |
| Test | `test()` | Choose + run engine; store standardised result |
| Effect size | `effects()` | Add Hedges’ g + CI via `{effectsize}` |
| Results | `tidy()` | Tidy tibble (standard schema) |
| Reporting | `report()` | Concise CLI summary |
| Plotting | `autoplot()` | Estimation (now) and diagnostics (future) |
| Compare engines | `compare_engines()` | Quick sensitivity check across engines |
| Show policy | `decision_tree()` | Print selection rules for transparency |

## Engines (MVP)

- `welch_t` (default, robust to variance heterogeneity)  
- `student_t` (equal-variance t-test; choose via `set_engine()` or
  `set_strategy("parametric")`)  
- `mann_whitney` (rank-sum; manual alternative for very small n + severe
  non-normality)

All engines **return the same tidy columns**, e.g.:

    test, method, engine, n, statistic, df, p.value,
    estimate, conf.low, conf.high, metric,
    es_value, es_conf_low, es_conf_high, es_metric, notes

## Relationship to tidymodels

| tidycomp | Purpose | tidymodels analogue | Comment |
|----|----|----|----|
| `comp_spec()` | comparison container | `parsnip::model_spec()`, `workflows::workflow()` | comp-first; not predictive |
| `set_roles()` | map variables to roles | `recipes::recipe()` + `update_role()` | minimal roles for comparisons |
| `set_design()` / `set_outcome_type()` | declare design/family | pick model class/family | declarative; drives engine choice |
| `set_strategy()` / `set_engine()` | choose policy or backend | `parsnip::set_engine()` | strategy selects; engine forces |
| `diagnose()` | inspect only | — | recipes is preprocessing, not diagnostics |
| `prepare()` | explicit changes + log | `recipes::prep()`/`bake()` | lightweight, audit-friendly |
| `test()` | choose + run engine | `parsnip::fit()` | selection + execution combined |
| `effects()` | effect sizes | — (use `{effectsize}`) | standardised ES |
| `tidy()` / `report()` / `autoplot()` | results & viz | `broom`, `ggplot2` | unified schema & defaults |

## Design principles

- **Transparency first**: inspect before you modify; log every change.  
- **Safe defaults**: Welch’s t-test as a robust baseline.  
- **Standardised I/O**: consistent input structure to engines and
  consistent output schema.  
- **Separation of concerns**:
  roles/design/diagnostics/prep/test/effects/report are distinct
  steps.  
- **Composable & extensible**: adding new engines shouldn’t change
  surrounding code.

## Roadmap

- Additional engines: Brunner–Munzel, permutation t, one-way ANOVA
  (Welch), Kruskal–Wallis.  
- Effect sizes for rank tests (e.g., **Cliff’s δ** + CI).  
- Integration with `gtsummary`/`gt` for publication tables.  
- `compare_means()` one-liner wrapper that uses the same pipeline
  internally.
- Diagnostics plot (`autoplot(..., type = "diagnostics")`: QQ per group;
  variance hint).

## Contributing

Bug reports and pull requests are welcome! Please open an issue to
discuss major changes.  
By participating, you agree to abide by the [Code of
Conduct](https://www.contributor-covenant.org/version/3/0/code_of_conduct/).

## License

MIT (see `LICENSE` file).

## Citation

If you use **tidycomp**, please cite the package and version. A CITATION
file will be added before CRAN release.

------------------------------------------------------------------------

### Reproducible example (copy & run)

``` r
library(tidycomp)

spec <- comp_spec(mtcars) |>
  set_roles(mpg, am) |>
  set_design("independent") |>
  set_outcome_type("numeric") |>
  diagnose()

spec <- prepare(
  spec,
  steps = list(step_trim_outliers(mpg, method = "mad", action = "winsorize"))
)
spec <- test(spec) |> effects()

report(spec)
tidy(spec$fitted)
# autoplot(spec, type = "estimation")
```
