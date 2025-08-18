#' Diagnose a comparison specification (non-destructive)
#'
#' Run lightweight checks on a `comp_spec`: for numeric outcomes this includes
#' group sizes, variance heterogeneity (Brown–Forsythe proxy), within-group
#' normality (Shapiro), and simple IQR-based outlier counts. For binary
#' outcomes, it inspects the contingency table between outcome and group to
#' determine whether assumptions for chi-squared tests are met or if an exact
#' test is warranted. The function **does not** alter the raw data; it attaches
#' a `diagnostics` list to the spec.
#'
#' @param spec A `comp_spec` with roles/design/outcome type already set.
#'   Must include at least an outcome variable and a group variable in
#'   `spec$roles`.
#'
#' @details
#' For numeric outcomes, the following summaries are stored in
#' `spec$diagnostics`:
#'
#' - `group_sizes`: a tibble with counts per group.
#' - `var_bf_p`: Brown-Forsythe proxy p-value for variance heterogeneity.
#' - `normality`: per-group Shapiro-Wilk p-values (flagged only; not enforced).
#' - `sphericity`: Mauchly p-values for repeated-measures designs.
#' - `notes`: human-readable notes highlighting potential issues
#'   (e.g., small groups, variance heterogeneity, non-normality, outliers).
#'
#' For binary outcomes, the diagnostics include:
#'
#' - `table`: the observed contingency table.
#' - `expected`: expected counts (when available).
#' - `engine`: recommended engine based on cell counts.
#' - `notes`: notes on small cells or other issues.
#'
#' @return The input `spec`, updated with a `diagnostics` list.
#'
#' @examples
#' spec <- comp_spec(mtcars)
#' spec$roles <- list(outcome = "am", group = "cyl")
#' spec$design <- "independent"
#' spec$outcome_type <- "binary"
#' spec <- diagnose(spec)
#' spec$diagnostics$engine
#' @export
diagnose <- function(spec) {
  stopifnot(inherits(spec, "comp_spec"))
  roles <- spec$roles
  req <- c(roles$outcome, roles$group)
  .validate_cols(spec$data_raw, req)

  df <- tibble::as_tibble(spec$data_raw)
  outcome <- roles$outcome
  group <- roles$group

  if (spec$outcome_type == "numeric") {
    if (!is.numeric(df[[outcome]])) {
      cli::cli_abort("`diagnose()` currently supports numeric outcomes.")
    }
    if (!is.factor(df[[group]])) {
      df[[group]] <- factor(df[[group]])
    }
    g_n <- dplyr::count(df, .data[[group]], name = "n")
    small_n <- any(g_n$n < 30)
    norm <- dplyr::group_by(df, .data[[group]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        p_shapiro = ifelse(
          n >= 3,
          tryCatch(
            stats::shapiro.test(.data[[outcome]])$p.value,
            error = function(e) NA_real_
          ),
          NA_real_
        ),
        .groups = "drop"
      )
    p_bf <- .brown_forsythe(df[[outcome]], df[[group]])
    var_hetero <- is.finite(p_bf) && !is.na(p_bf) && p_bf < 0.05
    out_counts <- dplyr::group_by(df, .data[[group]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        out_lo = {
          lim <- .flag_outliers(.data[[outcome]], method = "iqr")
          sum(.data[[outcome]] < lim$lo, na.rm = TRUE)
        },
        out_hi = {
          lim <- .flag_outliers(.data[[outcome]], method = "iqr")
          sum(.data[[outcome]] > lim$hi, na.rm = TRUE)
        },
        .groups = "drop"
      )
    out_total <- sum(out_counts$out_lo + out_counts$out_hi)
    notes <- c()
    if (small_n) {
      notes <- c(
        notes,
        "Small group sizes (< 30): CLT less reliable; prefer robust/Welch or permutation."
      )
    }
    if (isTRUE(var_hetero)) {
      notes <- c(notes, "Variance heterogeneity flagged (Brown-Forsythe proxy).")
    }
    if (any(norm$p_shapiro < 0.05, na.rm = TRUE)) {
      notes <- c(
        notes,
        "Shapiro normality flagged in at least one group (interpret with caution)."
      )
    }
    if (out_total > 0) {
      notes <- c(
        notes,
        sprintf("Potential outliers detected (IQR*3): %d total.", out_total)
      )
    }
    sphericity <- NULL
    if (identical(spec$design, "repeated") && !is.null(roles$id)) {
      id <- roles$id
      .validate_cols(df, id)
      if (rlang::is_installed(c("afex", "performance"))) {
        fit <- tryCatch(
          afex::aov_ez(id = id, dv = outcome, within = group, data = df),
          error = function(e) NULL
        )
        if (!is.null(fit)) {
          sp <- tryCatch(performance::check_sphericity(fit), error = function(e) NULL)
          if (!is.null(sp)) {
            if (is.data.frame(sp)) {
              sphericity <- tibble::as_tibble(sp)
            } else {
              eff <- names(sp)
              if (is.null(eff)) {
                eff <- NA_character_
              }
              sphericity <- tibble::tibble(Effect = eff, p = as.numeric(sp))
            }
            p_mauchly <- .extract_sphericity_p(sphericity)
            if (!is.na(p_mauchly) && is.finite(p_mauchly) && p_mauchly < 0.05) {
              notes <- c(notes, "Sphericity violation flagged (Mauchly p < .05).")
            }
          }
        }
      }
    }
    spec$diagnostics <- list(
      group_sizes = g_n,
      normality = norm,
      var_bf_p = p_bf,
      sphericity = sphericity,
      notes = notes
    )
  } else if (spec$outcome_type == "binary") {
    if (spec$design == "paired") {
      diag <- .diagnose_paired_contingency(df, outcome, group, roles$id)
    } else if (spec$design == "independent") {
      if (!is.factor(df[[group]])) {
        df[[group]] <- factor(df[[group]])
      }
      if (!is.factor(df[[outcome]])) {
        df[[outcome]] <- factor(df[[outcome]])
      }
      diag <- .diagnose_contingency(df[[group]], df[[outcome]])
    } else {
      cli::cli_abort("`diagnose()` supports binary outcomes for independent or paired designs.")
    }
    spec$diagnostics <- diag
  } else {
    cli::cli_abort("`diagnose()` does not support outcome type '{spec$outcome_type}'.")
  }
  notes <- spec$diagnostics$notes %||% character()
  cli::cli_inform("Diagnostics complete. {length(notes)} note{?s} recorded.")
  spec
}
