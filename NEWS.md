# tidycomp 0.4.0

-   Added 6 new engines for categorical comparisons:

    -   `fisher_exact`,
    -   `chisq_yates`,
    -   `chisq_nxn`,
    -   `mcnemar_chi2`,
    -   `mcnemar_chi2_cc`,
    -   `mcnemar_exact`

-   Added effect size computation for all new categorical comparison engines (phi, Cramer's V, and odds ratio) using the `effectsize` package.

-   `set_effects()` gains a `correction` argument for controlling the Haldane-Anscombe adjustment when running `mcnemar_exact` with odds ratios; `effects()` warns when the correction is applied or skipped.

-   `diagnose()` warns about zero cells in categorical comparisons and suggests inspecting `diagnostics$table` for details.

-   Additional tests were added for all new engines.

# tidycomp 0.3.1

-   Added more tests to improve coverage, especially for `effects.R`.

-   Fixing wrong parsing of `conf_level`◔ to `effecsize` package functions. `conf_level` passes now to `ci`

-   Fixed failing tests and note about 'tails' not being imported or specified.

# tidycomp 0.3.0

-   Package supports unpaird 2-group comparisons using: student's t-test, Welch's t-test, and Mann-Whitney U test.

-   Package supports multiple unpaired group comparisons using: ANOVA, Welch's ANOVA, and Kruskal-Wallis test.

-   Package supports repeated measures Anova via `afex::aov_ez()` and Friedman's test.

-   Added engine-specific effect size calculations.

-   Added multiple outlier detection methods, e.g. `step_trim_outliers()` now supports `method = "iqr"`, `"mad"`, or `"sd"` (all default to `k = 3`), following guidance from the r4np book.

-   Documentation for all functions has been added/updated/tidied.

-   `comp_spec()` fixed issues with missing specifications.

-   `report_comp_result()` returns correct results.

-   `README.md` has been updated to include a link to the Contributor Code of Conduct.

# tidycomp 0.2.0

-   Added paired 2-group comparisons: `engine_paired_t` and `engine_wilcoxon_signed_rank`.

-   Amended tests to account for new paired engines.

-   Added multi-group comparisons: `engine_anova_oneway_equal`, `engine_anova_oneway_welch`, `engine_kruskal_wallis`, `engine_anova_repeated`, and `engine_friedman` with corresponding diagnostics.

# tidycomp 0.1.1

-   Added unit tests for all functions.

-   Added `covr` and corresponding github action.

-   `README.md` has been updated to include covr badge.

# tidycomp 0.1.0

-   Package supports unpaird 2-group comparisons using: student's t-test, Welch's t-test, and Mann-Whitney U test.

-   Supports computation of all effect sizes for the above tests.

-   Documentation for all functions has been added/updated/tidied.

-   `README.md` has been updated to include a link to the Contributor Code of Conduct.

-   `step_trim_outliers()` now supports `method = "iqr"`, `"mad"`, or `"sd"` (all default to `k = 3`), following guidance from the [R For Non-Programmers Book](https://r4np.com).

# tidycomp 0.0.0.9000

-   Initial commit: tidycomp MVP structure, README and NEWS.