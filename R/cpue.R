#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear
#' standardization. Supports ratio and log-transformed methods.
#'
#' @param x Input data: a numeric vector of catch values, or a data frame
#'   containing catch and effort columns.
#' @param ... Additional arguments passed to methods.
#'
#' @export
cpue <- function(x, ...) {
  UseMethod("cpue")
}

#' @rdname cpue
#' @export
cpue.default <- function(x, ...) {
  stop("Unsupported input type for cpue(): ", class(x), call. = FALSE)
}

#' @rdname cpue
#'
#' @param effort Numeric vector of effort (e.g., hours)
#' @param gear_type Character. Gear type used for sampling. Must be one of the
#'   types in the internal `gear_types` table. Defaults to `"nordic_gillnet"`,
#'   the standard reference gear (factor = 1.0).
#' @param method Character; one of `"ratio"` (default) or `"log"`.
#' @param verbose Logical; print processing info? Default from
#'   `getOption("fishr.verbose", FALSE)`.
#'
#' @return A numeric vector of CPUE values
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(c(100, 200), c(10, 20), method = "log")
cpue.numeric <- function(
  x,
  effort,
  gear_type = "nordic_gillnet",
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", FALSE),
  ...
) {
  if (!gear_type %in% gear_types$gear_type) {
    stop(
      "`gear_type` must be one of: ",
      paste(gear_types$gear_type, collapse = ", "),
      call. = FALSE
    )
  }

  gear_factor <- gear_types$gear_factor[gear_types$gear_type == gear_type]

  method <- match.arg(method)
  validate_numeric_inputs(x = x, effort = effort)

  if (verbose) {
    message("Processing ", length(x), " records using ", method, " method")
  }

  raw_cpue <- switch(method, ratio = x / effort, log = log(x / effort))

  new_cpue_result(
    cpue_values = raw_cpue * gear_factor,
    method = method,
    gear_factor = gear_factor,
    n_records = length(x)
  )
}

#' @rdname cpue
#' @param catch_col Name of the catch column (default `"catch"`).
#' @param effort_col Name of the effort column (default `"effort"`).
#' @export
cpue.data.frame <- function(
  x,
  catch_col = "catch",
  effort_col = "effort",
  gear_type = "nordic_gillnet",
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", FALSE),
  ...
) {
  if (!catch_col %in% names(x)) {
    stop("Column '", catch_col, "' not found in data frame.", call. = FALSE)
  }
  if (!effort_col %in% names(x)) {
    stop("Column '", effort_col, "' not found in data frame.", call. = FALSE)
  }

  cpue(
    x = x[[catch_col]],
    effort = x[[effort_col]],
    gear_type = gear_type,
    method = method,
    verbose = verbose,
    ...
  )
}

#' Create a cpue result object
#'
#' @param cpue_values Numeric vector of CPUE values.
#' @param method Character string indicating the calculation method.
#' @param gear_factor Numeric gear correction factor used.
#' @param n_records Integer number of records processed.
#'
#' @return A `cpue_result` object.
#' @noRd
new_cpue_result <- function(cpue_values, method, gear_factor, n_records) {
  structure(
    cpue_values, # The data
    method = method, # Attributes specifying metadata
    gear_factor = gear_factor,
    n_records = n_records,
    class = "cpue_result" # class is a special attribute
  )
}

#' @export
print.cpue_result <- function(x, ...) {
  cat("Survey Result\n")
  cat("Records:", attr(x, "n_records"), "\n")
  cat("Gear factor:", attr(x, "gear_factor"), "\n")
  cat("Method:", attr(x, "method"), "\n")
  cat("CPUE values:", round(x, 2), "\n")
  invisible(x)
}

#' @export
summary.cpue_result <- function(object, ...) {
  stats <- list(
    method = attr(object, "method"),
    n_records = attr(object, "n_records"),
    gear_factor = attr(object, "gear_factor"),
    mean_cpue = mean(object),
    median_cpue = stats::median(object),
    sd_cpue = stats::sd(object),
    range_cpue = range(object)
  )

  structure(stats, class = "summary.cpue_result")
}

#' @export
print.summary.cpue_result <- function(x, ...) {
  cat("Survey Result Summary\n")
  cat("---------------------\n")
  cat("Method:      ", x$method, "\n")
  cat("Records:     ", x$n_records, "\n")
  cat("Gear factor: ", x$gear_factor, "\n")
  cat("Mean CPUE:   ", round(x$mean_cpue, 2), "\n")
  cat("Median CPUE: ", round(x$median_cpue, 2), "\n")
  cat("SD CPUE:     ", round(x$sd_cpue, 2), "\n")
  cat(
    "CPUE Range:    ",
    round(x$range_cpue[1], 2),
    "-",
    round(x$range_cpue[2], 2),
    "\n"
  )
  invisible(x)
}

#' @export
plot.cpue_result <- function(x, ...) {
  plot(
    seq_along(x),
    x,
    type = "b",
    xlab = "Record",
    ylab = "CPUE",
    main = paste("CPUE -", attr(x, "method"), "method"),
    ...
  )
}
