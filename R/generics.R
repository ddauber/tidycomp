#' Tidy output for comparison results
#'
#' Generic to return a tibble representation of a result. The implemented
#' method for `comp_result` coerces the object to a tibble and optionally
#' drops columns that contain only `NA` values.
#'
#' @param x An object to tidy. Currently, the method is defined for
#'   objects of class `comp_result`.
#' @param complete Logical. If `FALSE` (default), columns that are entirely
#'   `NA` are removed from the output. Set to `TRUE` to retain all columns.
#' @param ... Passed to methods.
#'
#' @return A tibble.
#' @export
tidy <- function(x, complete = FALSE, ...) {
  UseMethod("tidy")
}

#' @rdname tidy
#' @export
tidy.comp_result <- function(x, complete = FALSE, ...) {
  out <- tibble::as_tibble(x)
  if (!complete) {
    out <- dplyr::select(out, dplyr::where(~ !all(is.na(.x))))
  }
  out
}

#' Report summary
#'
#' Generic for printing a concise, human-readable summary of results.
#' Methods emit messages via **cli**.
#'
#' @param x An object to report. Methods are provided for `comp_spec`
#'   (delegates to its fitted result) and `comp_result`.
#' @param ... Passed to methods.
#'
#' @return The input object, invisibly (after printing messages).
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' @rdname report
#' @export
report.comp_spec <- function(x, ...) {
  if (is.null(x$fitted)) {
    cli::cli_abort("No fitted results; run `test()` first.")
  }
  report(x$fitted)
}

#' @rdname report
#' @export
report.comp_result <- function(x, ...) {
  res <- x$fitted %||% x
  cli::cli_inform("Test: {res$method} (engine = {res$engine})")
  if (!all(is.na(c(res$estimate, res$conf.low, res$conf.high)))) {
    cli::cli_inform(
      "Estimate (mean diff): {round(res$estimate, 3)} [{round(res$conf.low,3)}, {round(res$conf.high,3)}]"
    )
  }
  if (!is.null(res$es_value)) {
    cli::cli_inform(
      "Effect size: {res$es_metric} = {round(res$es_value, 3)} [{round(res$es_conf_low,3)}, {round(res$es_conf_high,3)}]"
    )
  }
  if (length(res$notes[[1]]) > 0) {
    cli::cli_warn("Notes: {paste(res$notes[[1]], collapse = ' | ')}")
  }
  invisible(x)
}

#' Autoplot
#'
#' Generic for producing a default plot. For `comp_spec`, this delegates
#' to its fitted result. For `comp_result`, type `"estimation"` draws a
#' point estimate with confidence interval; `"diagnostics"` is reserved.
#'
#' @param object Object to plot.
#' @param ... Passed to methods.
#'
#' @return A `ggplot` object, or `NULL` (invisibly) when not applicable.
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}

#' @rdname autoplot
#' @export
autoplot.comp_spec <- function(
  object,
  type = c("estimation", "diagnostics"),
  ...
) {
  if (is.null(object$fitted)) {
    cli::cli_abort("No fitted results; run `test()` first.")
  }
  autoplot(
    object$fitted,
    type = type,
    data = object$data_prepared %||% object$data_raw,
    roles = object$roles,
    ...
  )
}

#' @rdname autoplot
#' @param type Plot type. One of `"estimation"` or `"diagnostics"`.
#' @param data A data frame used for plotting (usually prepared/raw data).
#' @param roles A named list of roles, e.g., `list(outcome = , group = )`.
#' @export
autoplot.comp_result <- function(
  object,
  type = c("estimation", "diagnostics"),
  data,
  roles,
  ...
) {
  type <- rlang::arg_match(type)
  if (type == "estimation") {
    out <- roles$outcome
    grp <- roles$group
    df <- tibble::as_tibble(data[, c(out, grp)])
    names(df) <- c("outcome", "group")
    if (!is.factor(df$group)) {
      df$group <- factor(df$group)
    }
    est <- tibble::tibble(
      label = object$method,
      y = object$estimate,
      ymin = object$conf.low,
      ymax = object$conf.high
    )
    ggplot2::ggplot(est, ggplot2::aes(x = label, y = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ymin, ymax = ymax),
        width = 0.1
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(y = "Mean difference (group2 - group1)", x = NULL) +
      ggplot2::theme_minimal()
  } else {
    cli::cli_warn("Diagnostics plot not implemented in MVP.")
    invisible(NULL)
  }
}

#' Repeated-measures ANOVA engine via afex (internal)
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta  A list with roles/diagnostics metadata.
#' @param correction  "auto", "none", "GG", "HF", or "LB".
#' @param return_df   What to return in the table: "auto", "uncorrected", "GG", "HF", "LB", or "all".
#' @param afex_args   Named list of extra args passed to afex::aov_ez().
#' @keywords internal
#' @noRd
engine_anova_repeated <- function(
  data,
  meta,
  correction = c("auto", "GG"), # second value = preferred correction in auto mode
  return_df = c("auto"),
  afex_args = list()
) {
  correction <- match.arg(correction[1], c("auto", "none", "GG", "HF", "LB"))
  prefer_corr <- if (length(correction) > 1) correction[2] else "GG"
  return_df <- match.arg(
    return_df,
    c("auto", "uncorrected", "GG", "HF", "LB", "all")
  )

  df <- .standardize_repeated_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )

  # Fallback if packages missing
  if (!rlang::is_installed(c("afex", "performance"))) {
    missing_pkgs <- c("afex", "performance")[
      !rlang::is_installed(c("afex", "performance"))
    ]
    cli::cli_warn(
      "Package{?s} {.pkg {missing_pkgs}} not installed; using stats::aov fallback.",
      pkg = missing_pkgs
    )
    return(engine_anova_repeated_base(data, meta))
  }

  # ---- fit with afex ----------------------------------------------------
  # Always compute with ES turned off (we compute ES elsewhere if needed)
  base_args <- list(
    id = "id",
    dv = "outcome",
    within = "group",
    data = df,
    anova_table = list(correction = "none", es = "none") # start uncorrected
  )
  fit <- do.call(afex::aov_ez, utils::modifyList(base_args, afex_args))
  tab <- as.data.frame(fit$anova_table)

  # Pull uncorrected stats once
  F_val <- unname(tab["group", "F"])
  df1_raw <- unname(tab["group", "num Df"])
  df2_raw <- unname(tab["group", "den Df"])
  p_raw <- unname(tab["group", "Pr(>F)"])

  # Sphericity check (via performance)
  sp <- performance::check_sphericity(fit)
  # Extract Mauchly p for 'group'
  p_mauchly <- tryCatch(
    {
      # works with data.frame output from performance
      as.numeric(sp$p[
        sp$Effect %in% c("group", "group (within)", "within: group")
      ][1])
    },
    error = function(e) NA_real_
  )

  # Epsilons available in afex table
  eps_GG <- if ("GG eps" %in% colnames(tab)) {
    unname(tab["group", "GG eps"])
  } else {
    NA_real_
  }
  eps_HF <- if ("HF eps" %in% colnames(tab)) {
    unname(tab["group", "HF eps"])
  } else {
    NA_real_
  }

  # Helper to build one row given a label + epsilon
  make_row <- function(label, eps, p_col) {
    if (is.na(eps)) {
      return(NULL)
    }
    df1 <- df1_raw * eps
    df2 <- df2_raw * eps
    p <- if (!is.null(p_col) && p_col %in% colnames(tab)) {
      unname(tab["group", p_col])
    } else {
      stats::pf(F_val, df1, df2, lower.tail = FALSE)
    }
    tibble::tibble(
      test = "anova_repeated",
      method = "Repeated measures ANOVA",
      engine = "anova_repeated",
      n_obs = nrow(df),
      n_subjects = length(unique(df$id)),
      statistic = F_val,
      df1 = df1,
      df2 = df2,
      p.value = p,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      metric = label,
      notes = list(c(
        meta$diagnostics$notes %||% character(),
        sprintf("Reported with %s correction.", label)
      ))
    )
  }

  # Always compute candidates
  row_uncorr <- tibble::tibble(
    test = "anova_repeated",
    method = "Repeated measures ANOVA",
    engine = "anova_repeated",
    n_obs = nrow(df),
    n_subjects = length(unique(df$id)),
    statistic = F_val,
    df1 = df1_raw,
    df2 = df2_raw,
    p.value = p_raw,
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = "uncorrected",
    notes = list(c(meta$diagnostics$notes %||% character()))
  )
  row_GG <- make_row("GG", eps_GG, "Pr(>F[GG])")
  row_HF <- make_row("HF", eps_HF, "Pr(>F[HF])")

  out_all <- dplyr::bind_rows(
    row_uncorr,
    row_GG %||% NULL,
    row_HF %||% NULL
  )

  # Decide what to return
  choose_auto <- function() {
    if (!is.na(p_mauchly) && p_mauchly < 0.05) {
      # sphericity violated -> use preferred correction if available
      if (prefer_corr == "GG" && !is.null(row_GG)) {
        return(out_all[out_all$metric == "GG", ])
      }
      if (prefer_corr == "HF" && !is.null(row_HF)) {
        return(out_all[out_all$metric == "HF", ])
      }
      # fallback: whichever exists
      return(out_all[out_all$metric %in% c("GG", "HF"), ][1, , drop = FALSE])
    } else {
      # sphericity OK -> uncorrected
      return(out_all[out_all$metric == "uncorrected", ])
    }
  }

  res <- switch(
    return_df,
    all = out_all,
    uncorrected = out_all[out_all$metric == "uncorrected", ],
    GG = out_all[out_all$metric == "GG", ],
    HF = out_all[out_all$metric == "HF", ],
    LB = {
      # LB not available in afex table; compute if requested
      # If you want LB, add a block similar to GG/HF using car::Anova(..., correction="LB")
      cli::cli_warn(
        "Lower-bound correction not implemented in this engine yet."
      )
      out_all[out_all$metric == "uncorrected", ]
    },
    auto = choose_auto()
  )

  # Add contextual note if we auto-selected
  if (return_df == "auto") {
    note <- if (!is.na(p_mauchly) && p_mauchly < 0.05) {
      sprintf(
        "Sphericity violated (Mauchly p = %.3g); %s correction reported.",
        p_mauchly,
        if (nrow(res) && res$metric[1] != "uncorrected") {
          res$metric[1]
        } else {
          prefer_corr
        }
      )
    } else if (!is.na(p_mauchly)) {
      sprintf(
        "Sphericity not violated (Mauchly p = %.3g); uncorrected results reported.",
        p_mauchly
      )
    } else {
      "Mauchly p could not be extracted; defaulting to uncorrected."
    }
    res$notes <- lapply(res$notes, function(x) c(x, note))
  }

  res
}


# define global variables to avoid R CMD check warnings
utils::globalVariables(c(
  ".data",
  "label",
  "y",
  "ymin",
  "ymax",
  "n",
  ".id",
  ".g",
  ".y"
))
