#' Set effect size options
#'
#' Records preferences for effect size computation to be used later by
#' \code{\link{effects}()}.
#'
#' @param x A model specification object created with \code{\link{comp_spec}()}.
#' @param type Effect size type to compute. Use \code{"auto"} to select the
#'   engine's recommended default (e.g., "ges" for repeated-measures ANOVA).
#'   Supported values include: \code{"ges"}, \code{"pes"}, \code{"eta2"},
#'   \code{"omega2"}, \code{"epsilon2"}, \code{"d"}, \code{"g"},
#'   \code{"rank_biserial"}, \code{"kendalls_w"}, \code{"r2"},
#'   \code{"phi"}, \code{"cramers_v"}, \code{"cohens_g"}, \code{"oddsratio"}.
#' @param conf_level Confidence interval level (e.g., 0.90). Use \code{NULL}
#'   to skip confidence intervals. Defaults to 0.95.
#' @param compute Logical; if \code{TRUE}, \code{\link{test}()} will compute
#'   effect sizes automatically (by calling \code{effects()}) after the test.
#'   Default is \code{FALSE}.
#' @return The updated model specification object with preferences stored in
#'   \code{$effects_args}.
#' @seealso \code{\link{effects}}, \code{\link{set_engine}}, \code{\link{set_engine_options}}, \code{\link{test}}
#' @export
set_effects <- function(x, type = "auto", conf_level = 0.95, compute = FALSE) {
  x$effects_args <- utils::modifyList(
    x$effects_args %||% list(),
    list(type = type, conf_level = conf_level, compute = compute)
  )
  x
}

#' Compute and store effect sizes
#'
#' Computes effect sizes for the fitted model and stores them on the spec.
#' Works on a spec (will call \code{test()} if needed) or a fitted result
#' (with an attached model). If no effect-size options were set with
#' \code{set_effects()}, defaults are chosen automatically based on engine
#' and/or model class.
#'
#' For non-ANOVA tests (t, Wilcoxon, Friedman), this function computes effect
#' sizes from the original data and roles (no conversion from test statistics).
#' Therefore, you must call \code{effects()} on a \emph{spec} for those models.
#'
#' @param x A spec produced by \code{comp_spec()} (optionally already tested),
#'   or a fitted result with an attached model (i.e., \code{attr(x, "model")}).
#' @param type Effect size type. Use \code{"auto"} to let the function choose
#'   an engine-/class-based default. Supported values: \code{"ges"}, \code{"pes"},
#'   \code{"eta2"}, \code{"omega2"}, \code{"epsilon2"}, \code{"d"}, \code{"g"},
#'   \code{"rank_biserial"}, \code{"kendalls_w"}, \code{"r2"}, \code{"phi"},
#'   \code{"cramers_v"}, \code{"cohens_g"}, \code{"oddsratio"}.
#' @param conf_level Confidence level (e.g., \code{0.90}); \code{NULL} for none. Defaults to
#'   \code{0.95}.
#' @return If \code{x} is a spec, the updated spec with \code{$effects};
#'   otherwise a tibble of effect sizes.
#' @export
effects <- function(
  x,
  type = c(
    "auto",
    "ges",
    "pes",
    "eta2",
    "omega2",
    "epsilon2",
    "d",
    "g",
    "rank_biserial",
    "kendalls_w",
    "r2",
    "phi",
    "cramers_v",
    "cohens_g",
    "oddsratio"
  ),
  conf_level = 0.95
) {
  type <- match.arg(type)
  is_spec <- inherits(x, "comp_spec")

  if (
    is_spec && (is.null(x$fitted) || is.null(attr(x$fitted, "model", TRUE)))
  ) {
    x <- test(x)
  }

  fitted <- if (is_spec) x$fitted else x
  mod <- attr(fitted, "model", exact = TRUE)
  if (is.null(mod)) {
    stop("No attached model found; cannot compute effect sizes.", call. = FALSE)
  }

  user_args <- if (is_spec) (x$effects_args %||% list()) else list()
  type_arg <- user_args$type %||% NULL
  ci_arg <- user_args$conf_level

  final_type <- if (!identical(type, "auto")) type else (type_arg %||% "auto")
  final_ci <- if (!missing(conf_level)) {
    conf_level
  } else if ("conf_level" %in% names(user_args)) {
    ci_arg
  } else {
    0.95
  }

  if (identical(final_type, "auto")) {
    final_type <- .default_effect_type(x, fitted, mod)
  }

  out <- .compute_effects(
    model = mod,
    type = final_type,
    conf_level = final_ci,
    parent_spec = if (is_spec) x else NULL
  )

  if (is_spec) {
    x$effects_args <- utils::modifyList(
      x$effects_args %||% list(),
      list(type = final_type, conf_level = final_ci)
    )
    x$effects <- out
    return(x)
  } else {
    attr(out, "model") <- mod
    return(out)
  }
}

# choose default ES type: engine hint first, then model class
.default_effect_type <- function(spec_or_fit, fitted, model) {
  hint <- if (inherits(spec_or_fit, "comp_spec")) {
    spec_or_fit$effects_hint
  } else {
    attr(fitted, "engine_hint", exact = TRUE)
  }
  if (!is.null(hint)) {
    return(hint)
  }
  cls <- class(model)

  if (any("glm" %in% cls)) {
    return("r2")
  }
  if (any(c("afex_aov", "Anova.mlm", "aovlist") %in% cls)) {
    return("ges")
  }
  if (any(c("aov", "lm") %in% cls)) {
    return("omega2")
  }
  if (
    inherits(model, "htest") &&
      !is.null(model$statistic) &&
      "t" %in% names(model$statistic)
  ) {
    return("d")
  }
  if (
    inherits(model, "htest") &&
      !is.null(model$statistic) &&
      "W" %in% names(model$statistic)
  ) {
    return("rank_biserial")
  }
  if (
    inherits(model, "htest") &&
      grepl("Friedman", model$method, ignore.case = TRUE)
  ) {
    return("kendalls_w")
  }
  "eta2"
}

# helpers to access data/roles from a spec
.get_spec_data <- function(spec) spec$data_prepared %||% spec$data_raw
.get_spec_roles <- function(spec) spec$roles

# core router: compute effect sizes using effectsize/performance ONLY from proper inputs
.compute_effects <- function(model, type, conf_level, parent_spec = NULL) {
  # --- Kruskal-Wallis: rank-based epsilon squared from raw data ---
  if (
    type == "epsilon2" &&
      inherits(model, "htest") &&
      !is.null(parent_spec) &&
      (parent_spec$engine == "kruskal_wallis" ||
        grepl("Kruskal-Wallis", model$method, ignore.case = TRUE))
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop(
        "Package 'effectsize' is required for epsilon squared.",
        call. = FALSE
      )
    }

    dat <- .get_spec_data(parent_spec)
    roles <- .get_spec_roles(parent_spec)
    fml <- stats::as.formula(paste(roles$outcome, "~", roles$group))

    es <- effectsize::rank_epsilon_squared(
      fml,
      data = dat,
      ci = conf_level
    )

    estimate <- es[[1]]
    ci_low <- if (!is.null(conf_level) && "CI_low" %in% names(es)) {
      es$CI_low
    } else {
      NA_real_
    }
    ci_high <- if (!is.null(conf_level) && "CI_high" %in% names(es)) {
      es$CI_high
    } else {
      NA_real_
    }

    return(tibble::tibble(
      effect = roles$group,
      type = "epsilon2",
      estimate = estimate,
      conf.low = ci_low,
      conf.high = ci_high
    ))
  }

  # --- ANOVA families via effectsize (model-based; no raw data needed) ---
  if (
    type %in%
      c("ges", "pes", "eta2", "omega2", "epsilon2") ||
      any(class(model) %in% c("afex_aov", "Anova.mlm", "aov", "aovlist", "lm"))
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop(
        "Package 'effectsize' is required for ANOVA effect sizes.",
        call. = FALSE
      )
    }

    # compute using the appropriate function
    if (type == "omega2") {
      if (
        inherits(model, "htest") &&
          grepl("Welch", model$method, ignore.case = TRUE)
      ) {
        if (is.null(parent_spec)) {
          stop(
            "Omega squared for Welch's ANOVA requires the original data; call effects() on a spec.",
            call. = FALSE
          )
        }
        dat <- .get_spec_data(parent_spec)
        roles <- .get_spec_roles(parent_spec)
        fml <- stats::as.formula(paste(roles$outcome, "~", roles$group))
        es <- effectsize::omega_squared(
          fml,
          data = dat,
          ci = conf_level
        )
      } else {
        es <- effectsize::omega_squared(model, ci = conf_level)
      }
      candidates <- c("Omega2", "omega.sq", "omega_sq")
    } else if (type == "epsilon2") {
      es <- effectsize::epsilon_squared(model, ci = conf_level)
      candidates <- c("Epsilon2", "epsilon.sq", "epsilon_sq")
    } else {
      es <- effectsize::eta_squared(
        model,
        generalized = identical(type, "ges"),
        partial = identical(type, "pes"),
        ci = conf_level
      )
      candidates <- switch(
        type,
        "ges" = c(
          "GES",
          "Eta2_G",
          "Eta2_generalized",
          "eta.sq.gen",
          "eta_sq_generalized",
          "Eta2"
        ),
        "pes" = c("Eta2_partial", "eta.sq.part", "eta_sq_partial", "Eta2"),
        "eta2" = c("Eta2", "eta.sq", "eta_sq")
      )
    }

    # pick the first estimate column that exists
    col <- intersect(candidates, names(es))
    if (length(col) == 0L) {
      stop(
        sprintf(
          "Could not find an estimate column for type '%s'. Available columns: %s",
          type,
          paste(names(es), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    col <- col[[1]]

    # --- pick the factor/parameter column robustly (Welch may have none) ---
    par_col <- NULL
    for (cand in c("Parameter", "Effect", "Term", "term", "Factor")) {
      if (cand %in% names(es)) {
        par_col <- cand
        break
      }
    }

    # if there's no factor column (e.g., omega_squared(oneway.test(...))),
    # fall back to the single available row and label the effect as "group"
    if (is.null(par_col)) {
      pick <- 1L
      effect_label <- "group"
    } else {
      # prefer explicit 'group' if present; else first non-residual row
      if (any(es[[par_col]] == "group")) {
        pick <- which(es[[par_col]] == "group")[1]
      } else {
        pick <- which(!grepl("Residual", es[[par_col]], ignore.case = TRUE))[1]
      }
      effect_label <- es[[par_col]][pick]
    }

    # ensure we pass a real value, not NULL, so the column isn't dropped
    estimate_val <- es[[col]][pick]
    if (length(estimate_val) == 0L) {
      estimate_val <- NA_real_
    }

    # --- conf_level columns robustly ---
    ci_low_col <- intersect(
      c("CI_low", "CI_low_", "CI_low..", "CI_low..CI."),
      names(es)
    )
    ci_high_col <- intersect(
      c("CI_high", "CI_high_", "CI_high..", "CI_high..CI."),
      names(es)
    )
    conf_low <- if (length(ci_low_col)) {
      es[[ci_low_col[[1]]]][pick]
    } else {
      NA_real_
    }
    conf_high <- if (length(ci_high_col)) {
      es[[ci_high_col[[1]]]][pick]
    } else {
      NA_real_
    }

    return(tibble::tibble(
      effect = effect_label,
      type = type,
      estimate = estimate_val,
      conf.low = conf_low,
      conf.high = conf_high
    ))
  }

  # --- t tests: Cohen's d / Hedges' g from raw data & roles (no stat conversions) ---
  if (
    type %in% c("d", "g") && inherits(model, "htest") && !is.null(parent_spec)
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop(
        "Package 'effectsize' is required for Cohen's d / Hedges' g.",
        call. = FALSE
      )
    }

    dat <- .get_spec_data(parent_spec)
    roles <- .get_spec_roles(parent_spec)

    if (!is.null(roles$id)) {
      df <- .standardize_paired_numeric(
        dat,
        roles$outcome,
        roles$group,
        roles$id
      )
      g <- names(df)
      d <- effectsize::cohens_d(
        df[[g[2]]],
        df[[g[1]]],
        ci = conf_level,
        hedges.correction = identical(type, "g"),
        paired = TRUE
      )
    } else {
      fml <- stats::as.formula(paste(roles$outcome, "~", roles$group))
      d <- effectsize::cohens_d(
        fml,
        data = dat,
        ci = conf_level,
        hedges.correction = identical(type, "g"),
        paired = FALSE
      )
    }

    est <- if ("Hedges_g" %in% names(d)) d$Hedges_g else d$Cohens_d
    return(tibble::tibble(
      effect = roles$group,
      type = type,
      estimate = est,
      conf.low = if (!is.null(conf_level)) d$CI_low else NA_real_,
      conf.high = if (!is.null(conf_level)) d$CI_high else NA_real_
    ))
  }
  if (
    type %in% c("d", "g") && inherits(model, "htest") && is.null(parent_spec)
  ) {
    stop(
      "Cohen's d/g require the original data and roles; call effects() on a spec.",
      call. = FALSE
    )
  }

  # --- Wilcoxon / Mann–Whitney: rank-biserial from raw data (no stat conversions) ---
  if (
    type == "rank_biserial" && inherits(model, "htest") && !is.null(parent_spec)
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop("Package 'effectsize' is required for rank-biserial.", call. = FALSE)
    }

    dat <- .get_spec_data(parent_spec)
    roles <- .get_spec_roles(parent_spec)

    if (!is.null(roles$id)) {
      df <- .standardize_paired_numeric(
        dat,
        roles$outcome,
        roles$group,
        roles$id
      )
      g <- names(df)
      rbs <- effectsize::rank_biserial(
        df[[g[2]]],
        df[[g[1]]],
        ci = conf_level,
        paired = TRUE
      )
    } else {
      fml <- stats::as.formula(paste(roles$outcome, "~", roles$group))
      rbs <- effectsize::rank_biserial(
        fml,
        data = dat,
        ci = conf_level,
        paired = FALSE
      )
    }

    return(tibble::tibble(
      effect = roles$group,
      type = "rank_biserial",
      estimate = rbs$r_rank_biserial %||% rbs$Rank_biserial %||% rbs[[1]],
      conf.low = if (!is.null(conf_level)) rbs$CI_low else NA_real_,
      conf.high = if (!is.null(conf_level)) rbs$CI_high else NA_real_
    ))
  }
  if (
    type == "rank_biserial" && inherits(model, "htest") && is.null(parent_spec)
  ) {
    stop(
      "Rank-biserial requires the original data and roles; call effects() on a spec.",
      call. = FALSE
    )
  }

  # --- Friedman: Kendall's W from raw data (no stat conversions) ---
  if (
    type == "kendalls_w" &&
      inherits(model, "htest") &&
      !is.null(parent_spec) &&
      (parent_spec$engine == "friedman" ||
        grepl("Friedman", model$method, ignore.case = TRUE))
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop("Package 'effectsize' is required for Kendall's W.", call. = FALSE)
    }

    dat <- .get_spec_data(parent_spec)
    roles <- .get_spec_roles(parent_spec)
    if (is.null(roles$id)) {
      stop(
        "Kendall's W for Friedman requires an 'id' role in the spec.",
        call. = FALSE
      )
    }
    fml <- stats::as.formula(paste(
      roles$outcome,
      "~",
      roles$group,
      "|",
      roles$id
    ))

    kw <- effectsize::kendalls_w(fml, data = dat, ci = conf_level)

    return(tibble::tibble(
      effect = roles$group,
      type = "kendalls_w",
      estimate = kw$Kendalls_W %||% kw$W %||% kw[[1]],
      conf.low = if (!is.null(conf_level)) kw$CI_low else NA_real_,
      conf.high = if (!is.null(conf_level)) kw$CI_high else NA_real_
    ))
  }
  if (
    type == "kendalls_w" &&
      inherits(model, "htest") &&
      is.null(parent_spec)
  ) {
    stop(
      "Kendall's W requires the original data and roles; call effects() on a spec.",
      call. = FALSE
    )
  }

  # --- Contingency tables: phi, Cramer's V, Cohen's g, odds ratio ---
  if (
    type %in% c("phi", "cramers_v", "cohens_g", "oddsratio") &&
      inherits(model, "htest") &&
      !is.null(parent_spec)
  ) {
    if (!rlang::is_installed("effectsize")) {
      stop("Package 'effectsize' is required for contingency-table effect sizes.", call. = FALSE)
    }

    dat <- .get_spec_data(parent_spec)
    roles <- .get_spec_roles(parent_spec)

    if (is.null(roles$id)) {
      tbl <- table(dat[[roles$group]], dat[[roles$outcome]])
    } else {
      wide <- .standardize_paired_categorical(
        dat,
        roles$outcome,
        roles$group,
        roles$id
      )
      tbl <- table(wide[[1]], wide[[2]])
    }

    hint <- .engine_effect_hint(parent_spec$engine)
    if (!is.null(hint) && !identical(hint, type)) {
      cli::cli_warn(
        "Engine `{parent_spec$engine}` recommends effect size `{hint}`; `{type}` was requested."
      )
    }

    if (type == "phi") {
      if (nrow(tbl) > 2 || ncol(tbl) > 2) {
        cli::cli_warn(
          "Phi is typically for 2x2 tables; consider `set_effects(type = 'cramers_v')`."
        )
      }
      es <- effectsize::phi(tbl, ci = conf_level)
      est <- es$Phi %||% es$phi %||% es[[1]]
    } else if (type == "cramers_v") {
      if (nrow(tbl) == 2 && ncol(tbl) == 2) {
        cli::cli_warn(
          "Cramer's V equals phi for 2x2 tables; consider `set_effects(type = 'phi')`."
        )
      }
      es <- effectsize::cramers_v(tbl, ci = conf_level)
      est <- es$Cramers_v %||% es$Cramers_V %||% es[[1]]
    } else if (type == "cohens_g") {
      es <- effectsize::cohens_g(tbl, ci = conf_level)
      est <- es$Cohens_g %||% es$cohens_g %||% es[[1]]
    } else {
      if (identical(parent_spec$engine, "mcnemar_exact")) {
        b <- tbl[1, 2]
        c <- tbl[2, 1]
        if (b == 0 || c == 0) {
          if (b == 0) tbl[1, 2] <- tbl[1, 2] + 0.5
          if (c == 0) tbl[2, 1] <- tbl[2, 1] + 0.5
        }
      }
      es <- effectsize::oddsratio(tbl, ci = conf_level)
      est <- es$Odds_ratio %||% es$OR %||% es[[1]]
    }

    return(tibble::tibble(
      effect = roles$group,
      type = type,
      estimate = est,
      conf.low = if (!is.null(conf_level)) es$CI_low else NA_real_,
      conf.high = if (!is.null(conf_level)) es$CI_high else NA_real_
    ))
  }
  if (
    type %in% c("phi", "cramers_v", "cohens_g", "oddsratio") &&
      inherits(model, "htest") &&
      is.null(parent_spec)
  ) {
    stop(
      "Contingency-table effect sizes require the original data and roles; call effects() on a spec.",
      call. = FALSE
    )
  }

  # --- GLM / Mixed: model R2 ---
  if (type == "r2" && (inherits(model, "glm") || inherits(model, "lmerMod"))) {
    if (!rlang::is_installed("performance")) {
      stop("Package 'performance' is required for R2 measures.", call. = FALSE)
    }
    r2 <- performance::r2(model)
    return(tibble::tibble(
      effect = "model",
      type = "r2",
      estimate = unname(r2$r2),
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }

  stop(
    sprintf(
      "No effect-size handler for model class: %s with type '%s'.",
      paste(class(model), collapse = "/"),
      type
    ),
    call. = FALSE
  )
}
