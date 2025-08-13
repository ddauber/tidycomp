#' Tidy output for comparison results
#'
#' Generic to return a tibble representation of a result. The implemented
#' method for `comp_result` coerces the object to a tibble.
#'
#' @param x An object to tidy. Currently, the method is defined for
#'   objects of class `comp_result`.
#' @param ... Passed to methods.
#'
#' @return A tibble.
#' @export
tidy <- function(x, ...) {
  UseMethod("tidy")
}

#' @rdname tidy
#' @export
tidy.comp_result <- function(x, ...) {
  tibble::as_tibble(x)
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
  cli::cli_inform("Test: {x$method} (engine = {x$engine})")
  cli::cli_inform(
    "Estimate (mean diff): {round(x$fitted$estimate, 3)} [{round(x$fitted$conf.low,3)}, {round(x$fitted$conf.high,3)}]"
  )
  if (!is.null(x$fitted$es_value)) {
    cli::cli_inform(
      "Effect size: {x$fitted$es_metric} = {round(x$fitted$es_value, 3)} [{round(x$fitted$es_conf_low,3)}, {round(x$fitted$es_conf_high,3)}]"
    )
  }
  if (length(x$fitted$notes[[1]]) > 0) {
    cli::cli_warn("Notes: {paste(x$fitted$notes[[1]], collapse = ' | ')}")
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


# define global variables to avoid R CMD check warnings
utils::globalVariables(c(".data", "label", "y", "ymin", "ymax", "n"))
