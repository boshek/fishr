cpue <- function(catch, effort, gear_factor = 1) {
  raw_cpue <- catch / effort

  raw_cpue * gear_factor
}
