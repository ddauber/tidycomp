#' Mauchly's Test of Sphericity from Long Data
#'
#' Compute Mauchly's test of sphericity directly from long-format
#' repeated-measures data without first fitting a model.
#'
#' @param data A data frame containing at least the columns referenced in
#'   `outcome`, `group`, and `id`.
#' @param outcome The outcome column.
#' @param group The within-subject factor column.
#' @param id The subject identifier column.
#' @param effect_label Optional character label for the effect. Defaults to the
#'   name of `group`.
#'
#' @return A tibble with columns `Effect`, `W`, and `p`.
#' @examples
#' df <- tibble::tibble(
#'   id = rep(1:4, each = 3),
#'   condition = factor(rep(c("A", "B", "C"), times = 4)),
#'   score = rnorm(12)
#' )
#' diag_mauchly(df, score, condition, id)
#' @export

diag_mauchly <- function(data, outcome, group, id, effect_label = NULL) {
  outcome <- rlang::ensym(outcome)
  group   <- rlang::ensym(group)
  id      <- rlang::ensym(id)

  df <- dplyr::transmute(
    data,
    .id = factor(!!id),
    .g  = factor(!!group),
    .y  = !!outcome
  )

  wide <- tryCatch(
    tidyr::pivot_wider(df, names_from = .g, values_from = .y) |>
      dplyr::arrange(.id),
    error = function(e) NULL
  )
  if (is.null(wide)) {
    return(tibble::tibble(
      Effect = effect_label %||% rlang::as_name(group),
      W = NA_real_,
      p = NA_real_
    ))
  }

  Y <- as.matrix(wide[, setdiff(names(wide), ".id"), drop = FALSE])
  if (any(!stats::complete.cases(Y))) {
    return(tibble::tibble(
      Effect = effect_label %||% rlang::as_name(group),
      W = NA_real_,
      p = NA_real_
    ))
  }

  mlm_fit <- tryCatch(stats::lm(Y ~ 1), error = function(e) NULL)
  if (is.null(mlm_fit)) {
    return(tibble::tibble(
      Effect = effect_label %||% rlang::as_name(group),
      W = NA_real_,
      p = NA_real_
    ))
  }

  within_df <- data.frame(
    .g = factor(colnames(Y), levels = colnames(Y))
  )
  mt <- tryCatch(
    stats::mauchly.test(mlm_fit, X = ~ .g, data = within_df),
    error = function(e) NULL
  )
  if (is.null(mt)) {
    return(tibble::tibble(
      Effect = effect_label %||% rlang::as_name(group),
      W = NA_real_,
      p = NA_real_
    ))
  }

  tibble::tibble(
    Effect = effect_label %||% rlang::as_name(group),
    W      = unname(mt$statistic),
    p      = unname(mt$p.value)
  )
}

