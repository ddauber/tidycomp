#' Set post-hoc options
#'
#' Stores preferences for later computation of pairwise group contrasts via
#' [post_hoc()].
#'
#' @param x A model specification created with [comp_spec()].
#' @param method Post-hoc method to use. Use "auto" to allow
#'   [post_hoc()] to choose a sensible default based on the engine and data.
#'   Possible values include "tukey", "bonferroni", "pairwise.wilcox",
#'   "pairwise.prop", "games_howell", "dunn", and "regwq".
#' @param alpha Significance level used to gate computation. If the
#'   omnibus p-value is greater than or equal to `alpha`, post-hoc tests are
#'   skipped unless `force` is `TRUE`.
#' @param force Logical; if `TRUE`, compute post-hoc tests even when the
#'   omnibus test is not significant.
#' @param compute Logical; if `TRUE`, [test()] will compute post-hoc
#'   contrasts automatically after fitting the main test.
#'
#' @return The updated model specification.
#' @export
set_post_hoc <- function(
  x,
  method = "auto",
  alpha = 0.05,
  force = FALSE,
  compute = FALSE
) {
  x$post_hoc_args <- utils::modifyList(
    x$post_hoc_args %||% list(),
    list(method = method, alpha = alpha, force = force, compute = compute)
  )
  x
}

#' Compute pairwise post-hoc comparisons
#'
#' Computes pairwise contrasts between groups following a significant
#' omnibus test. The chosen method depends on the analysis engine, design and
#' outcome type, and can be overridden via [set_post_hoc()].
#'
#' The result is a tibble with one row per group contrast. If the omnibus
#' test is not significant (p ≥ `alpha`) and `force = FALSE`, a zero-row tibble
#' is returned with attributes `skipped = TRUE` and a `reason` message.
#'
#' @param x A `comp_spec` that has been fit with [test()]. If not yet fit,
#'   `post_hoc()` will call [test()] automatically.
#' @param method Post-hoc method (see [set_post_hoc()]). Use "auto" to pick a
#'   default based on the engine and data.
#' @param alpha Significance level used for gating (default 0.05).
#' @param force Logical; compute even if omnibus test is not significant.
#'
#' @return If `x` is a spec, the updated spec with `$post_hoc`; otherwise a
#'   tibble of pairwise contrasts.
#' @export
post_hoc <- function(x, method = "auto", alpha = 0.05, force = FALSE) {
  is_spec <- inherits(x, "comp_spec")
  if (!is_spec) {
    cli::cli_abort("`post_hoc()` currently works on a `comp_spec`.")
  }
  if (is.null(x$fitted)) {
    x <- test(x)
  }

  user <- x$post_hoc_args %||% list()
  method <- if (!identical(method, "auto")) {
    method
  } else {
    (user$method %||% "auto")
  }
  alpha <- if (!missing(alpha)) alpha else (user$alpha %||% 0.05)
  force <- if (!missing(force)) force else (user$force %||% FALSE)

  if (identical(method, "auto")) {
    method <- .default_post_hoc_method(
      engine = x$fitted$engine,
      design = x$design,
      outcome_type = x$outcome_type
    )
  }

  p_omni <- x$fitted$p.value %||% NA_real_
  if (!force && is.finite(p_omni) && p_omni >= alpha) {
    res <- tibble::tibble()
    attr(res, "skipped") <- TRUE
    attr(res, "reason") <- "omnibus p >= alpha"
    x$post_hoc <- res
    return(x)
  }

  data <- .get_spec_data(x)
  roles <- x$roles

  res <- switch(
    method,
    tukey = .ph_tukey(data, roles$outcome, roles$group),
    bonferroni = .ph_pairwise_t(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni"
    ),
    `pairwise.t.test` = .ph_pairwise_t(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni"
    ),
    `pairwise.wilcox` = .ph_pairwise_wilcox(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni"
    ),
    `pairwise.wilcox.test` = .ph_pairwise_wilcox(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni"
    ),
    `pairwise.prop` = .ph_pairwise_prop(
      data,
      roles$outcome,
      roles$group,
      "bonferroni"
    ),
    `pairwise.prop.test` = .ph_pairwise_prop(
      data,
      roles$outcome,
      roles$group,
      "bonferroni"
    ),
    games_howell = .ph_games_howell(
      data,
      roles$outcome,
      roles$group
    ),
    dunn = .ph_dunn(
      data,
      roles$outcome,
      roles$group,
      "holm"
    ),
    regwq = .ph_regwq(
      data,
      roles$outcome,
      roles$group
    ),
    cli::cli_abort("Unknown post-hoc method `{method}`.")
  )

  x$post_hoc_args <- utils::modifyList(
    user,
    list(method = method, alpha = alpha, force = force)
  )
  x$post_hoc <- res
  x
}

# choose default method based on engine/outcome
.default_post_hoc_method <- function(engine, design, outcome_type) {
  if (outcome_type == "binary") {
    return("pairwise.prop.test")
  }
  if (engine == "kruskal_wallis") {
    if (requireNamespace("rstatix", quietly = TRUE)) {
      return("dunn")
    }
    return("pairwise.wilcox.test")
  }
  if (
    engine == "anova_oneway_welch" &&
      requireNamespace("rstatix", quietly = TRUE)
  ) {
    return("games_howell")
  }
  "bonferroni"
}

# convert lower-triangular matrix to tibble
.matrix_to_tibble <- function(mat, method) {
  df <- as.data.frame(as.table(mat))
  df <- df[!is.na(df$Freq), , drop = FALSE]
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }
  tibble::tibble(
    group1 = as.character(df$Var1),
    group2 = as.character(df$Var2),
    p.value = as.numeric(df$Freq),
    method = method
  )
}

.ph_pairwise_t <- function(data, outcome, group, design, p.adjust) {
  g <- data[[group]]
  o <- data[[outcome]]
  fit <- stats::pairwise.t.test(
    o,
    g,
    p.adjust.method = p.adjust,
    paired = design == "paired"
  )
  .matrix_to_tibble(fit$p.value, "pairwise.t.test")
}

.ph_pairwise_wilcox <- function(data, outcome, group, design, p.adjust) {
  g <- data[[group]]
  o <- data[[outcome]]
  fit <- stats::pairwise.wilcox.test(
    o,
    g,
    p.adjust.method = p.adjust,
    paired = design == "paired"
  )
  .matrix_to_tibble(fit$p.value, "pairwise.wilcox.test")
}

.ph_pairwise_prop <- function(data, outcome, group, p.adjust) {
  g <- data[[group]]
  o <- data[[outcome]]
  tbl <- table(g, o)
  if (ncol(tbl) < 2) {
    tbl <- cbind(tbl, 0)
  }
  success <- tbl[, 2]
  n <- rowSums(tbl)
  fit <- stats::pairwise.prop.test(success, n, p.adjust.method = p.adjust)
  .matrix_to_tibble(fit$p.value, "pairwise.prop.test")
}

.ph_tukey <- function(data, outcome, group) {
  f <- stats::as.formula(paste(outcome, "~", group))
  fit <- stats::aov(f, data = data)
  ph <- stats::TukeyHSD(fit)
  df <- tibble::as_tibble(ph[[1]], rownames = "comparison")
  comps <- tidyr::separate(
    df,
    comparison,
    into = c("group1", "group2"),
    sep = "-"
  )
  tibble::tibble(
    group1 = comps$group1,
    group2 = comps$group2,
    p.value = df$`p adj`,
    method = "TukeyHSD"
  )
}

.ph_games_howell <- function(data, outcome, group) {
  if (!requireNamespace("rstatix", quietly = TRUE)) {
    cli::cli_abort("Package 'rstatix' is required for games_howell_test().")
  }
  f <- stats::as.formula(paste(outcome, "~", group))
  res <- rstatix::games_howell_test(data, f)
  tibble::tibble(
    group1 = res$group1,
    group2 = res$group2,
    p.value = res$p.adj,
    method = "games_howell_test"
  )
}

.ph_dunn <- function(data, outcome, group, p.adjust) {
  if (!requireNamespace("rstatix", quietly = TRUE)) {
    cli::cli_abort("Package 'rstatix' is required for dunn_test().")
  }
  f <- stats::as.formula(paste(outcome, "~", group))
  res <- rstatix::dunn_test(data, f, p.adjust.method = p.adjust)
  tibble::tibble(
    group1 = res$group1,
    group2 = res$group2,
    p.value = res$p.adj,
    method = "dunn_test"
  )
}

.ph_regwq <- function(data, outcome, group) {
  if (!requireNamespace("mutoss", quietly = TRUE)) {
    cli::cli_abort("Package 'mutoss' is required for regwq().")
  }
  g <- data[[group]]
  o <- data[[outcome]]
  fit <- mutoss::regwq(o, g)
  mat <- fit$p.value
  .matrix_to_tibble(mat, "regwq")
}
