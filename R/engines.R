#' Engine registry (internal)
#' @keywords internal
.tidycomp_engines <- function() {
  list(
    welch_t = engine_welch_t,
    student_t = engine_student_t,
    mann_whitney = engine_mann_whitney
  )
}

#' Welch two-sample t-test engine
#' @keywords internal
engine_welch_t <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::t.test(outcome ~ group, data = df, var.equal = FALSE)
  tibble::tibble(
    test = "t",
    method = "Welch t-test",
    engine = "welch_t",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = diff(rev(fit$estimate)), # group2 - group1 (consistent with t.test)
    conf.low = fit$conf.int[1],
    conf.high = fit$conf.int[2],
    metric = "mean_diff",
    notes = list(meta$diagnostics$notes %||% character())
  )
}

#' Student t-test (equal variances)
#' @keywords internal
engine_student_t <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::t.test(outcome ~ group, data = df, var.equal = TRUE)
  tibble::tibble(
    test = "t",
    method = "Student's t-test",
    engine = "student_t",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = diff(rev(fit$estimate)),
    conf.low = fit$conf.int[1],
    conf.high = fit$conf.int[2],
    metric = "mean_diff",
    notes = list(meta$diagnostics$notes %||% character())
  )
}

#' Mann–Whitney (Wilcoxon rank-sum) engine
#' @keywords internal
engine_mann_whitney <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::wilcox.test(outcome ~ group, data = df, exact = FALSE)
  # No CI for mean diff; we return median diff = NA here (MVP)
  tibble::tibble(
    test = "wilcox",
    method = "Mann–Whitney U (Wilcoxon rank-sum)",
    engine = "mann_whitney",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = NA_real_,
    p.value = unname(fit$p.value),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = "stochastic_ordering",
    notes = list(c(
      meta$diagnostics$notes %||% character(),
      "Median/ES CI not computed (MVP)."
    ))
  )
}
