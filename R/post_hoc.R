#' Set post-hoc options
#'
#' Stores preferences for later computation of pairwise group contrasts via
#' [post_hoc()].
#'
#' @param x A model specification created with [comp_spec()].
#' @param method Post-hoc method to use. Use "auto" to allow
#'   [post_hoc()] to choose a sensible default based on the engine and data.
#'   Possible values include "tukey", "bonferroni", "holm",
#'   "pairwise_wilcox_test", "pairwise_prop_test", "games_howell",
#'   and "dunn".
#' @param alpha Significance level used to gate computation. If the
#'   omnibus p-value is greater than or equal to `alpha`, post-hoc tests are
#'   skipped unless `force` is `TRUE`.
#' @param force Logical; if `TRUE`, compute post-hoc tests even when the
#'   omnibus test is not significant.
#' @param compute Logical; if `TRUE`, [test()] will compute post-hoc
#'   contrasts automatically after fitting the main test.
#' @param conf_level Confidence level for intervals (default 0.95).
#'
#' @return The updated model specification.
#' @export
set_post_hoc <- function(
  x,
  method = "auto",
  alpha = 0.05,
  force = FALSE,
  compute = FALSE,
  conf_level = 0.95
) {
  x$post_hoc_args <- utils::modifyList(
    x$post_hoc_args %||% list(),
    list(
      method = method,
      alpha = alpha,
      force = force,
      compute = compute,
      conf_level = conf_level
    )
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
#' @param conf_level Confidence interval level (default 0.95).
#'
#' @return If `x` is a spec, the updated spec with `$post_hoc`; otherwise a
#'   tibble of pairwise contrasts.
#' @export
post_hoc <- function(x, method = "auto", alpha = 0.05, force = FALSE,
                     conf_level = 0.95) {
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
  conf_level <- if (!missing(conf_level)) {
    conf_level
  } else {
    user$conf_level %||% 0.95
  }

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
    tukey = .ph_tukey(data, roles$outcome, roles$group, conf_level),
    bonferroni = .ph_pairwise_t(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni",
      conf_level
    ),
    holm = .ph_pairwise_t(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "holm",
      conf_level
    ),
    pairwise_t_test = .ph_pairwise_t(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "bonferroni",
      conf_level
    ),
    pairwise_wilcox_test = .ph_pairwise_wilcox(
      data,
      roles$outcome,
      roles$group,
      x$design,
      "holm",
      conf_level
    ),
    pairwise_prop_test = .ph_pairwise_prop(
      data,
      roles$outcome,
      roles$group,
      "bonferroni",
      conf_level
    ),
    games_howell = .ph_games_howell(
      data,
      roles$outcome,
      roles$group,
      conf_level
    ),
    dunn = .ph_dunn(
      data,
      roles$outcome,
      roles$group,
      "holm",
      conf_level
    ),
    cli::cli_abort("Unknown post-hoc method `{method}`.")
  )

  x$post_hoc_args <- utils::modifyList(
    user,
    list(method = method, alpha = alpha, force = force, conf_level = conf_level)
  )
  x$post_hoc <- res
  x
}

# choose default method based on engine/outcome
 .default_post_hoc_method <- function(engine, design, outcome_type) {
  if (outcome_type == "binary") {
    return("pairwise_prop_test")
  }
  if (engine == "kruskal_wallis") {
    if (requireNamespace("rstatix", quietly = TRUE)) {
      return("dunn")
    }
    return("pairwise_wilcox_test")
  }
  if (
    engine == "anova_oneway_welch" &&
      requireNamespace("rstatix", quietly = TRUE)
  ) {
    return("games_howell")
  }
  "tukey"
}

.ph_pairwise_t <- function(data, outcome, group, design, p.adjust, conf_level) {
  g <- factor(data[[group]])
  o <- data[[outcome]]
  combs <- utils::combn(levels(g), 2, simplify = FALSE)
  res <- purrr::map_dfr(combs, function(pair) {
    x1 <- o[g == pair[1]]
    x2 <- o[g == pair[2]]
    tt <- stats::t.test(
      x1,
      x2,
      paired = design == "paired",
      conf.level = conf_level
    )
    est <- if (length(tt$estimate) == 2) {
      unname(tt$estimate[1] - tt$estimate[2])
    } else {
      unname(tt$estimate)
    }
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      estimate = est,
      conf.low = tt$conf.int[1],
      conf.high = tt$conf.int[2],
      p.value = tt$p.value
    )
  })
  res$p.adj <- stats::p.adjust(res$p.value, method = p.adjust)
  res$p.adj.method <- p.adjust
  res$method <- "t.test"
  res
}

.ph_pairwise_wilcox <- function(data, outcome, group, design, p.adjust, conf_level) {
  g <- factor(data[[group]])
  o <- data[[outcome]]
  combs <- utils::combn(levels(g), 2, simplify = FALSE)
  res <- purrr::map_dfr(combs, function(pair) {
    x1 <- o[g == pair[1]]
    x2 <- o[g == pair[2]]
    wt <- stats::wilcox.test(
      x1,
      x2,
      paired = design == "paired",
      conf.int = TRUE,
      conf.level = conf_level,
      exact = FALSE
    )
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      estimate = unname(wt$estimate %||% NA_real_),
      conf.low = wt$conf.int[1],
      conf.high = wt$conf.int[2],
      p.value = wt$p.value
    )
  })
  res$p.adj <- stats::p.adjust(res$p.value, method = p.adjust)
  res$p.adj.method <- p.adjust
  res$method <- "wilcox.test"
  res
}

.ph_pairwise_prop <- function(data, outcome, group, p.adjust, conf_level) {
  g <- factor(data[[group]])
  o <- data[[outcome]]
  combs <- utils::combn(levels(g), 2, simplify = FALSE)
  res <- purrr::map_dfr(combs, function(pair) {
    x1 <- sum(o[g == pair[1]] == levels(o)[2])
    n1 <- sum(g == pair[1])
    x2 <- sum(o[g == pair[2]] == levels(o)[2])
    n2 <- sum(g == pair[2])
    pt <- stats::prop.test(c(x1, x2), c(n1, n2), conf.level = conf_level)
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      estimate = unname(pt$estimate[1] - pt$estimate[2]),
      conf.low = pt$conf.int[1],
      conf.high = pt$conf.int[2],
      p.value = pt$p.value
    )
  })
  res$p.adj <- stats::p.adjust(res$p.value, method = p.adjust)
  res$p.adj.method <- p.adjust
  res$method <- "prop.test"
  res
}

.ph_tukey <- function(data, outcome, group, conf_level) {
  f <- stats::as.formula(paste(outcome, "~", group))
  fit <- stats::aov(f, data = data)
  ph <- stats::TukeyHSD(fit, conf.level = conf_level)
  df <- tibble::as_tibble(ph[[1]], rownames = "comparison")
  comps <- tidyr::separate(
    df,
    comparison,
    into = c("group1", "group2"),
    sep = "-"
  )
  tuk <- tibble::tibble(
    group1 = comps$group1,
    group2 = comps$group2,
    estimate = df$diff,
    conf.low = df$lwr,
    conf.high = df$upr,
    p.adj = df$`p adj`
  )
  g <- factor(data[[group]])
  o <- data[[outcome]]
  combs <- utils::combn(levels(g), 2, simplify = FALSE)
  raw <- purrr::map_dfr(combs, function(pair) {
    tt <- stats::t.test(o[g == pair[1]], o[g == pair[2]], conf.level = conf_level)
    tibble::tibble(group1 = pair[1], group2 = pair[2], p.value = tt$p.value)
  })
  res <- dplyr::left_join(tuk, raw, by = c("group1", "group2"))
  res$p.adj.method <- "tukey"
  res$method <- "TukeyHSD"
  res
}

.ph_games_howell <- function(data, outcome, group, conf_level) {
  if (!requireNamespace("rstatix", quietly = TRUE)) {
    cli::cli_abort("Package 'rstatix' is required for games_howell().")
  }
  f <- stats::as.formula(paste(outcome, "~", group))
  res <- rstatix::games_howell_test(data, f, conf.level = conf_level)
  tibble::tibble(
    group1 = res$group1,
    group2 = res$group2,
    estimate = res$estimate %||% res$mean.difference %||% NA_real_,
    conf.low = res$conf.low,
    conf.high = res$conf.high,
    p.value = res$p,
    p.adj = res$p.adj,
    p.adj.method = "games_howell",
    method = "games_howell_test"
  )
}

.ph_dunn <- function(data, outcome, group, p.adjust, conf_level) {
  if (!requireNamespace("rstatix", quietly = TRUE)) {
    cli::cli_abort("Package 'rstatix' is required for dunn_test().")
  }
  f <- stats::as.formula(paste(outcome, "~", group))
  res <- rstatix::dunn_test(data, f, p.adjust.method = p.adjust)
  tibble::tibble(
    group1 = res$group1,
    group2 = res$group2,
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = res$p,
    p.adj = res$p.adj,
    p.adj.method = p.adjust,
    method = "dunn_test"
  )
}
