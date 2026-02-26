#' @noRd
validate_numeric_inputs <- function(...) {
  args <- list(...)
  arg_names <- names(args)

  for (i in seq_along(args)) {
    if (!is.numeric(args[[i]])) {
      stop(
        "'",
        arg_names[i],
        "' must be numeric, got ",
        class(args[[i]])[1],
        ".",
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


standardize_effort <- function(catch, effort_hours) {
  validate_numeric_inputs(catch = catch, effort_hours = effort_hours)

  effort_days <- effort_hours * 24

  cpue(catch = catch, effort = effort_days)
}
