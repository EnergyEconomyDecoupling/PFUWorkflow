#' Read a subtarget based on country
#'
#' @param target The name of the drake target as a string.
#' @param country The 3-letter ISO abbreviation (a string) of the country for whom `target` is to be readd from the drake cache.
#' @param name_of_countries_object A string giving the name of the countries object in the drake cache.
#'                                 Default is "countries".
#' @param cache_path The path to the drake cache. Default is ".drake/".
#'
#' @return the country-specific version of `target`
#'
#' @export
readd_by_country <- function(target, country, name_of_countries_object = "countries", cache_path = ".drake/") {
  country_index <- which(country == readd(name_of_countries_object, path = cache_path, character_only = TRUE), arr.ind = TRUE)
  drake::readd(target, path = cache_path, character_only = TRUE, subtargets = country_index)
}


