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
  country_index <- which(country == drake::readd(name_of_countries_object, path = cache_path, character_only = TRUE), arr.ind = TRUE)
  drake::readd(target, path = cache_path, character_only = TRUE, subtargets = country_index)
}


#' Pipe-amenable directory creation
#'
#' This function is small wrapper on `dir.create()`,
#' returning the path created.
#' If the directory creation process was unsuccessful, a warning is emitted.
#'
#' @param path a character vector containing a single path name. Tilde expansion is done.
#' @param showWarnings logical; should the warnings on failure be shown?
#' @param recursive logical. Should elements of the path other than the last be created?
#'                  If true, like the Unix command `mkdir -p`.
#' @param mode the mode to be used on Unix-alikes
#'
#' @return The `path` argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   td <- tempdir()
#'   dir.create.pipe(td, recursive = TRUE)
#'   unlink(td, force = TRUE, recursive = TRUE)
#' }
dir.create.pipe <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777") {
  success <- dir.create(path = path, showWarnings = showWarnings, recursive = recursive, mode = mode)
  if (!success & showWarnings) {
    warning(paste("Unsuccessful creation of directory: ", path))
  }
  return(path)
}
