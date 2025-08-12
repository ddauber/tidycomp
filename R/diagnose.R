#' Diagnose a comparison specification (non-destructive)
#'
#' Runs basic checks: group counts, variance heterogeneity (Brown–Forsythe proxy),
#' Shapiro normality (per group, flagged only), and simple IQR outlier counts.
#'
#' @param spec A `comp_spec` with roles/design/outcome_type set.
#' @return The updated `comp_spec` with a `diagnostics` list.
#' @export
diagnose <- function(spec) {
  stopifnot(inherits(spec, "comp_spec"))
  roles <- spec$roles
  req <- c(roles$outcome, roles$group)
  .validate_cols(spec$data_raw, req)

  df <- tibble::as_tibble(spec$data_raw)
  outcome <- roles$outcome
  group <- roles$group

  if (!is.numeric(df[[outcome]])) {
    cli::cli_abort("`diagnose()` currently supports numeric outcomes.")
  }
  if (!is.factor(df[[group]])) {
    df[[group]] <- factor(df[[group]])
  }

  g_n <- dplyr::count(df, .data[[group]], name = "n")
  small_n <- any(g_n$n < 30)

  # normality flags per group (only when n >= 3)
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

  # variance heterogeneity (Brown–Forsythe proxy)
  p_bf <- .brown_forsythe_2g(df[[outcome]], df[[group]])
  var_hetero <- is.finite(p_bf) && !is.na(p_bf) && p_bf < 0.05

  # outliers (IQR rule), per group
  out_counts <- dplyr::group_by(df, .data[[group]]) |>
    dplyr::summarise(
      n = dplyr::n(),
      out_lo = {
        lim <- .flag_outliers_iqr(.data[[outcome]])
        sum(.data[[outcome]] < lim$lo, na.rm = TRUE)
      },
      out_hi = {
        lim <- .flag_outliers_iqr(.data[[outcome]])
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
    notes <- c(notes, "Variance heterogeneity flagged (Brown–Forsythe proxy).")
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

  spec$diagnostics <- list(
    group_sizes = g_n,
    normality = norm,
    var_bf_p = p_bf,
    notes = notes
  )
  cli::cli_inform("Diagnostics complete. {length(notes)} note{?s} recorded.")
  spec
}
