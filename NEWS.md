# tidycomp (development version)

* `engine_anova_repeated` now determines sphericity internally and selects an
  appropriate correction (none, GG, or HF) before fitting a single
  `afex::aov_ez()` model.

# tidycomp 0.2.0

* Added paired 2-group comparisons: `engine_paired_t` and `engine_wilcoxon_signed_rank`.
* Amended tests to account for new paired engines.
* Added multi-group comparisons: `engine_anova_oneway_equal`,
  `engine_anova_oneway_welch`, `engine_kruskal_wallis`,
  `engine_anova_repeated`, and `engine_friedman` with corresponding diagnostics.

# tidycomp 0.1.1

* Added unit tests for all functions.
* Added `covr` and corresponding github action.
* `README.md` has been updated to include covr badge.

# tidycomp 0.1.0

* Package supports unpaird 2-group comparisons using: student's t-test,
  Welch's t-test, and Mann-Whitney U test.
* Supports computation of all effect sizes for the above tests.
* Documentation for all functions has been added/updated/tidied.
* `README.md` has been updated to include a link to the Contributor Code of Conduct.
* `step_trim_outliers()` now supports `method = "iqr"`, `"mad"`, or `"sd"`
  (all default to `k = 3`), following guidance from the [R For Non-Programmers Book](https://r4np.com).

# tidycomp 0.0.0.9000

* Initial commit: tidycomp MVP structure, README and NEWS.
