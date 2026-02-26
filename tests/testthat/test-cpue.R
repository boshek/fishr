#' Calculate Biomass Index
#'
#' Calculates biomass index from CPUE and area swept. Can optionally
#' compute CPUE from catch and effort data. See [cpue()] for details.
#'
#' @param cpue Numeric vector of CPUE values. If `catch` and `effort` are
#'   provided, this is computed automatically.
#' @param area_swept Numeric vector of area swept (e.g., **km^2^**)
#' @param catch Numeric vector of catch (e.g., kg).
#' @inheritParams cpue.numeric
#' @inheritDotParams cpue.numeric -effort
#'
#' @return A numeric vector of biomass index values
#'
#' @details
#' Two modes of use:
#'
#' - Provide `cpue` directly for a simple calculation.
#' - Provide `catch` and `effort` to compute CPUE first, then scale by area.
#'
#' Any additional arguments in `...` are forwarded to [cpue()].
#'
#' @seealso [cpue()] to compute CPUE values from raw catch and effort.
#'
#' @export
#'
#' @examples
#' # From pre-computed CPUE
#' biomass_index(cpue = 10, area_swept = 5)
#'
#' # Compute CPUE on the fly
#' biomass_index(area_swept = 5, catch = 100, effort = 10)
#'
#' # Pass method through to cpue()
#' biomass_index(
#'   area_swept = 5,
#'   catch = c(100, 200),
#'   effort = c(10, 20),
#'   method = "log"
#' )
biomass_index <- function(
  cpue = NULL,
  area_swept,
  catch = NULL,
  effort = NULL,
  verbose = getOption("fishr.verbose", default = FALSE),
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
