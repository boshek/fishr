#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch (e.g., kg)
#' @param effort Numeric vector of effort (e.g., hours)
#' @param gear_factor Numeric adjustment for gear standardization (default is 1)
#' @param verbose Logical indicating whether to print processing messages (default is FALSE)
#'
#' @return A numeric vector of CPUE values
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(100, 10, gear_factor = 0.5)
cpue <- function(catch, effort, gear_factor = 1, verbose = FALSE) {
  if (verbose) {
    message("Processing ", length(catch), " records")
  }

  raw_cpue <- catch / effort

  raw_cpue * gear_factor
}
