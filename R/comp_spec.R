#' Create a comparison specification
#'
#' @param data A data frame (tibble recommended).
#' @return An object of class `comp_spec`.
#' @export
#'
#' @examples
#' spec <- comp_spec(mtcars)
comp_spec <- function(data) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data frame.")
  }
  structure(
    list(
      data_raw = tibble::as_tibble(data),
      data_prepared = NULL,
      roles = list(outcome = NULL, group = NULL, id = NULL, weights = NULL),
      design = NULL,
      outcome_type = NULL,
      strategy = "auto",
      engine = NULL,
      diagnostics = NULL,
      prep_log = tibble::tibble(),
      fitted = NULL
    ),
    class = "comp_spec"
  )
}

#' Set roles (outcome, group, id, weights)
#' @export
set_roles <- function(spec, outcome, group, id = NULL, weights = NULL) {
  stopifnot(inherits(spec, "comp_spec"))
  out <- .capture_role(spec$data_raw, {{ outcome }})
  grp <- .capture_role(spec$data_raw, {{ group }})
  idc <- if (!rlang::quo_is_missing(rlang::enquo(id))) {
    .capture_role(spec$data_raw, {{ id }})
  } else {
    NULL
  }
  wts <- if (!rlang::quo_is_missing(rlang::enquo(weights))) {
    .capture_role(spec$data_raw, {{ weights }})
  } else {
    NULL
  }

  spec$roles <- list(outcome = out, group = grp, id = idc, weights = wts)
  cli::cli_inform("Roles set: outcome = `{out}`, group = `{grp}`.")
  spec
}

#' Set design
#' @param design One of "independent", "paired", "repeated", "factorial".
#' @export
set_design <- function(
  spec,
  design = c("independent", "paired", "repeated", "factorial")
) {
  stopifnot(inherits(spec, "comp_spec"))
  design <- rlang::arg_match(design)
  spec$design <- design
  cli::cli_inform("Design set to {design}.")
  spec
}

#' Set outcome type
#' @param type One of "numeric", "binary", "ordered", "count".
#' @export
set_outcome_type <- function(
  spec,
  type = c("numeric", "binary", "ordered", "count")
) {
  stopifnot(inherits(spec, "comp_spec"))
  type <- rlang::arg_match(type)
  spec$outcome_type <- type
  cli::cli_inform("Outcome type set to {type}.")
  spec
}

#' Set strategy (engine selection policy)
#' @param strategy One of "auto","pragmatic","parametric","robust","permutation".
#' @export
set_strategy <- function(
  spec,
  strategy = c("auto", "pragmatic", "parametric", "robust", "permutation")
) {
  stopifnot(inherits(spec, "comp_spec"))
  strategy <- rlang::arg_match(strategy)
  spec$strategy <- strategy
  cli::cli_inform("Strategy set to {strategy}.")
  spec
}

#' Force a specific engine
#' @export
set_engine <- function(spec, engine) {
  stopifnot(inherits(spec, "comp_spec"))
  if (!engine %in% names(.tidycomp_engines())) {
    cli::cli_abort(c(
      "Unknown engine `{engine}`.",
      "i" = "See names(.tidycomp_engines()) for available engines."
    ))
  }
  spec$engine <- engine
  cli::cli_inform("Engine set to `{engine}` (manual override).")
  spec
}

#' @export
print.comp_spec <- function(x, ...) {
  cat("<comp_spec>\n")
  cat(
    "  roles: ",
    paste(names(compact(x$roles))[lengths(x$roles) > 0], collapse = ", "),
    "\n",
    sep = ""
  )
  cat("  design: ", x$design %||% "—", "\n", sep = "")
  cat("  outcome_type: ", x$outcome_type %||% "—", "\n", sep = "")
  cat("  strategy: ", x$strategy %||% "—", "\n", sep = "")
  cat("  engine: ", x$engine %||% "auto", "\n", sep = "")
  invisible(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
