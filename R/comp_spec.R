#' Create a comparison specification
#'
#' Initialize a tidycomp comparison specification. The returned object
#' stores your raw data alongside analysis settings (roles, design,
#' outcome type, strategy, engine), plus slots that later steps will fill
#' (prepared data, diagnostics, fitted results).
#'
#' @param data A data frame or tibble containing the dataset to analyze.
#'   A tibble is recommended but not required.
#'
#' @details
#' A `comp_spec` is a lightweight container to coordinate a comparison
#' workflow. It does not modify your data. Key fields include:
#'
#' - `data_raw`: the input data as a tibble
#' - `data_prepared`: populated by preparation steps
#' - `roles`: a named list for variable roles (e.g., outcome, group)
#' - `design`: comparison design (e.g., "independent")
#' - `outcome_type`: expected outcome type (e.g., "numeric", "binary")
#' - `strategy`: analysis strategy; defaults to "auto"
#' - `engine`: computation backend (if applicable)
#' - `diagnostics`: results from diagnostic checks
#' - `fitted`: fitted model(s) or comparison result(s)
#'
#' @return An object of class `comp_spec`.
#' @seealso [set_roles()], [set_design()], [set_outcome_type()],
#'   [set_strategy()], [set_engine()], [diagnose()], [test()]
#'
#' @examples
#' # Minimal example
#' spec <- comp_spec(mtcars)
#' spec
#'
#' # Access stored data
#' head(spec$data_raw)
#'
#' # Illustrative next steps:
#' # spec |>
#' #   set_roles(outcome = mpg, group = am) |>
#' #   set_design("independent") |>
#' #   set_outcome_type("numeric") |>
#' #   test()
#'
#' @export
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
#'
#' Capture and store variable roles on a `comp_spec`.
#'
#' @param spec A `comp_spec`.
#' @param outcome,group,id,weights Tidy-eval columns defining roles.
#'   `id` and `weights` are optional.
#'
#' @return The updated `comp_spec`.
#' @examples
#' spec <- comp_spec(mtcars)
#' spec <- set_roles(spec, outcome = mpg, group = am)
#' @export
set_roles <- function(spec, outcome, group, id = NULL, weights = NULL) {
  stopifnot(inherits(spec, "comp_spec"))
  out <- .capture_role(spec$data_raw, {{ outcome }})
  grp <- .capture_role(spec$data_raw, {{ group }})
  idc <- if (!missing(id)) {
    .capture_role(spec$data_raw, {{ id }})
  } else {
    NULL
  }
  wts <- if (!missing(weights)) {
    .capture_role(spec$data_raw, {{ weights }})
  } else {
    NULL
  }

  spec$roles <- list(outcome = out, group = grp, id = idc, weights = wts)
  cli::cli_inform("Roles set: outcome = `{out}`, group = `{grp}`.")
  spec
}

#' Set design
#'
#' Define the comparison design on a `comp_spec`.
#'
#' @param spec A `comp_spec`.
#' @param design One of "independent", "paired", "repeated", "factorial".
#'
#' @return The updated `comp_spec`.
#' @examples
#' spec <- comp_spec(mtcars)
#' spec <- set_design(spec, "independent")
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
#'
#' Declare the outcome type used by downstream engines and checks.
#'
#' @param spec A `comp_spec`.
#' @param type One of "numeric", "binary", "ordered", "count".
#'
#' @return The updated `comp_spec`.
#' @examples
#' spec <- comp_spec(mtcars)
#' spec <- set_outcome_type(spec, "numeric")
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
#'
#' Choose how the engine will be selected when running [test()].
#'
#' @param spec A `comp_spec`.
#' @param strategy One of "auto", "pragmatic", "parametric", "robust", "permutation".
#'
#' @return The updated `comp_spec`.
#' @examples
#' spec <- comp_spec(mtcars)
#' spec <- set_strategy(spec, "auto")
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
#'
#' Manually set the engine to use when running [test()]. Overrides the
#' strategy-based selection.
#'
#' @param spec A `comp_spec`.
#' @param engine Engine ID (e.g., "welch_t", "student_t", "mann_whitney").
#'
#' @return The updated `comp_spec`.
#' @examples
#' spec <- comp_spec(mtcars)
#' spec <- set_roles(spec, mpg, am)
#' spec <- set_engine(spec, "welch_t")
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

#' @rdname comp_spec
#' @method print comp_spec
#' @param x A `comp_spec` object.
#' @param ... Ignored; included for S3 compatibility.
#' @examples
#' spec <- comp_spec(mtcars)
#' print(spec)
#' @export
print.comp_spec <- function(x, ...) {
  cat("<comp_spec>\n")
  cat(
    "  roles: ",
    paste(
      names(purrr::compact(x$roles))[lengths(x$roles) > 0],
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  cat("  design: ", x$design %||% "-", "\n", sep = "")
  cat("  outcome_type: ", x$outcome_type %||% "-", "\n", sep = "")
  cat("  strategy: ", x$strategy %||% "-", "\n", sep = "")
  cat("  engine: ", x$engine %||% "auto", "\n", sep = "")
  invisible(x)
}

#' Internal fallback helper
#'
#' Return `y` when `x` is `NULL`, otherwise return `x`.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
