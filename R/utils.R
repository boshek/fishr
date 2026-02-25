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
