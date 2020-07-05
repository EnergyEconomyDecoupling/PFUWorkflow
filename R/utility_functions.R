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


set_up_for_testing <- function(countries = c("GHA", "ZAF"),
                               max_year = 2000,
                               how_far = "all_targets",
                               iea_data_path = IEATools::sample_iea_data_path(),
                               sample_fu_allocation_table_path = IEATools::sample_fu_allocation_table_path(),
                               exemplar_table_path = sample_exemplar_table_path(),
                               fu_analysis_folder = tempdir("FU_analysis_folder_for_testing"),
                               cache_path = tempfile("drake_cache_for_testing")) {
  set_up_temp_fu_analyses(fu_analysis_folder)
  plan <- get_plan(countries = countries,
                   max_year = max_year,
                   how_far = how_far,
                   iea_data_path = iea_data_path,
                   exemplar_table_path = exemplar_table_path,
                   fu_analysis_folder = fu_analysis_folder)
  temp_cache <- drake::new_cache(path = cache_path)
  list(plan = plan, cache_path = cache_path, temp_cache = temp_cache)
}


clean_up_after_testing <- function(temp_cache, cache_path) {
  # Clean up the cache.
  drake::readd("fu_analysis_folder", path = cache_path) %>%
    unlink(recursive = TRUE, force = TRUE)
  temp_cache$destroy()
}


# Helper function for setting up the FU analysis and FU etas Excel workbooks
set_up_temp_fu_analyses <- function(fu_folder) {
  # Set up FU allocation tables. Need to split file that is stored in IEATools
  GHAZAF_FU_allocation_tables <- IEATools::sample_fu_allocation_table_path() %>%
    openxlsx::read.xlsx()
  GHA_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  ZAF_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
  # Set up eta FU tables. Need to split file that is stored in IEATools.
  GHAZAF_eta_FU_tables <- IEATools::sample_eta_fu_table_path() %>%
    openxlsx::read.xlsx()
  GHA_FU_etas_table <- GHAZAF_eta_FU_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  ZAF_FU_etas_table <- GHAZAF_eta_FU_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")

  # Build FU analysis workbooks for each country.
  GHA_fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(GHA_fu_wb, "FU Allocations")
  openxlsx::writeData(GHA_fu_wb, "FU Allocations", GHA_FU_allocation_table)
  openxlsx::addWorksheet(GHA_fu_wb, "FU etas")
  openxlsx::writeData(GHA_fu_wb, "FU etas", GHA_FU_etas_table)

  ZAF_fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(ZAF_fu_wb, "FU Allocations")
  openxlsx::writeData(ZAF_fu_wb, "FU Allocations", ZAF_FU_etas_table)
  openxlsx::addWorksheet(ZAF_fu_wb, "FU etas")
  openxlsx::writeData(ZAF_fu_wb, "FU etas", ZAF_FU_etas_table)

  # Write the workbooks back to the temp directory.
  dir.create(file.path(fu_folder, "GHA"), showWarnings = FALSE)
  dir.create(file.path(fu_folder, "ZAF"), showWarnings = FALSE)
  openxlsx::saveWorkbook(GHA_fu_wb, file = file.path(fu_folder, "GHA", "GHA FU Analysis.xlsx"), overwrite = TRUE)
  openxlsx::saveWorkbook(ZAF_fu_wb, file = file.path(fu_folder, "ZAF", "ZAF FU Analysis.xlsx"), overwrite = TRUE)
}

