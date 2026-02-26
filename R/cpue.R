#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch (e.g., kg)
#' @param effort Numeric vector of effort (e.g., hours)
#' @param gear_factor Numeric adjustment for gear standardization (default is 1)
#' @param method Character; one of "ratio" or "log"
#' @param verbose Logical indicating whether to print processing messages (default is FALSE, also accept the value of `fishr.verbose`)
#'
#' @return A numeric vector of CPUE values
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(100, 10, gear_factor = 0.5)
cpue <- function(
  catch,
  effort,
  gear_factor = 1,
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", default = FALSE)
) {
  validate_numeric_inputs(catch = catch, effort = effort)
  if (verbose) {
    message("Processing ", length(catch), " records")
  }

  method <- match.arg(method)

  raw_cpue <- switch(
    method,
    ratio = catch / effort,
    log = log(catch / effort)
  )

  new_cpue_result(
    values = raw_cpue * gear_factor,
    method = method,
    gear_factor = gear_factor,
    n_records = length(catch)
  )
}

#' @export
print.cpue_result <- function(x, ...) {
  cat("CPUE Result\n")
  cat("Records:     ", attr(x, "n_records"), "\n")
  cat("Method:      ", attr(x, "method"), "\n")
  cat("Gear factor: ", attr(x, "gear_factor"), "\n")
  cat("Values:      ", round(x, 2), "\n")
  invisible(x)
}

#' @export
summary.cpue_result <- function(object, ...) {
  cat("Survey Result Summary\n")
  cat("---------------------\n")
  cat("Method:      ", attr(object, "method"), "\n")
  cat("Records:     ", attr(object, "n_records"), "\n")
  cat("Gear factor: ", attr(object, "gear_factor"), "\n")
  cat("Mean CPUE:   ", round(mean(object), 2), "\n")
  cat("Median CPUE: ", round(stats::median(object), 2), "\n")
  cat("SD CPUE:     ", round(stats::sd(object), 2), "\n")
  invisible(object)
}


#' @noRd
new_cpue_result <- function(values, method, gear_factor, n_records) {
  structure(
    values, # The data
    method = method, # Attributes specifying metadata
    gear_factor = gear_factor,
    n_records = n_records,
    class = "cpue_result" # class is a special attribute
  )
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
