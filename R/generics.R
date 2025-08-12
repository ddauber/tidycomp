#' Tidy output for `comp_result`
#' @export
tidy <- function(x, ...) {
  UseMethod("tidy")
}

#' @export
tidy.comp_result <- function(x, ...) {
  tibble::as_tibble(x)
}

#' Report summary
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' @export
report.comp_spec <- function(x, ...) {
  if (is.null(x$fitted)) {
    cli::cli_abort("No fitted results; run `test()` first.")
  }
  report(x$fitted)
}

#' @export
report.comp_result <- function(x, ...) {
  cli::cli_inform("Test: {x$method} (engine = {x$engine})")
  cli::cli_inform(
    "Estimate (mean diff): {round(x$estimate, 3)} [{round(x$conf.low,3)}, {round(x$conf.high,3)}]"
  )
  if (!is.null(x$es_value)) {
    cli::cli_inform(
      "Effect size: {x$es_metric} = {round(x$es_value, 3)} [{round(x$es_conf_low,3)}, {round(x$es_conf_high,3)}]"
    )
  }
  if (length(x$notes[[1]]) > 0) {
    cli::cli_warn("Notes: {paste(x$notes[[1]], collapse = ' | ')}")
  }
  invisible(x)
}

#' Autoplot
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}

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
