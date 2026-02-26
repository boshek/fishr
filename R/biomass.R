#' Calculate Biomass Index
#'
#' @param cpue Numeric vector of CPUE values. If NULL, computed from `catch`
#'   and `effort`.
#' @param area_swept Numeric vector of area swept (e.g., km²).
#' @param catch Numeric vector of catch (e.g., kg).
#' @inheritParams cpue.numeric
#' @inheritDotParams cpue.numeric -effort
#' @param verbose Logical; print processing info? Default from
#'   `getOption("fishr.verbose", FALSE)`.
#' @export
biomass_index <- function(
  cpue = NULL,
  area_swept,
  catch = NULL,
  effort = NULL,
  verbose = getOption("fishr.verbose", FALSE),
  ...
) {
  rlang::check_dots_used()

  if (is.null(cpue) && (!is.null(catch) && !is.null(effort))) {
    cpue <- cpue(catch, effort, verbose = verbose, ...)
  }

  if (is.null(cpue)) {
    stop("Must provide either 'cpue' or both 'catch' and 'effort'.")
  }

  validate_numeric_inputs(cpue = cpue, area_swept = area_swept)

  if (verbose) {
    message("calculating biomass index for ", length(area_swept), " records")
  }

  cpue * area_swept
}
