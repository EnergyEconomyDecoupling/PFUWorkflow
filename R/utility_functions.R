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


#' Set up directory structure for testing
#'
#' This is a private helper function that nobody should need to call,
#' except during automated testing.
#'
#' A typical way to use this function is
#'
#' ```
#' testing_setup <- SEAPSUTWorkflow:::set_up_for_testing()
#' tryCatch({
#'   # Make the plan in the temp_cache
#'   drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
#'   # Do stuff here, including test expectations
#' }, finally = {
#'   SEAPSUTWorkflow:::clean_up_after_testing(temp_cache = testing_setup$temp_cache,
#'                                            cache_path = testing_setup$cache_path)
#' })
#' ```
#'
#' @param countries The countries of interest. Default is `c("GHA", "ZAF")`
#' @param max_year The last year to be analyzed. Default is `2000.`
#' @param how_far The last target to include in the drake plan. Default is "all_targets"
#' @param iea_data_path The path to IEA data. Default is `IEATools::sample_iea_data_path()`.
#' @param sample_fu_allocation_table_path The path to the sample final-to-useful allocation table. Default is `IEATools::sample_fu_allocation_table_path()`.
#' @param exemplar_table_path The path to an example exemplar table. Default is `sample_exemplar_table_path()`.
#' @param fu_analysis_folder The path to the final-to-useful analysis folder. Default is `tempdir("FU_analysis_folder_for_testing")`.
#' @param cache_path The path to the temporary drake cache used for testing. Default is `tempfile("drake_cache_for_testing")`.
#'
#' @return A list containing a drake plan (`$plan`),
#'         the path to the temporary drake cache (`$cache_path`), and
#'         the temporary drake cache itself (`temp_cache`).
#'
#' @noRd
set_up_for_testing <- function(countries = c("GHA", "ZAF"),
                               max_year = 2000,
                               how_far = "all_targets",
                               iea_data_path = IEATools::sample_iea_data_path(),
                               sample_fu_allocation_table_path = IEATools::sample_fu_allocation_table_path(),
                               exemplar_table_path = sample_exemplar_table_path(),
                               fu_analysis_folder = tempdir(),
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


#' Clean up after testing with a temporary drake cache.
#'
#' This is a private helper function that nobody should need to call,
#' except during automated testing.
#'
#' A typical way to use this function is
#'
#' ```
#' testing_setup <- SEAPSUTWorkflow:::set_up_for_testing()
#' tryCatch({
#'   # Make the plan in the temp_cache
#'   drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
#'   # Do stuff here, including test expectations
#' }, finally = {
#'   SEAPSUTWorkflow:::clean_up_after_testing(temp_cache = testing_setup$temp_cache,
#'                                            cache_path = testing_setup$cache_path)
#' })
#' ```
#'
#' @param testing_setup A testing setup list generated by `set_up_for_testing()`.
#' @param cache_path The path to the temporary cache. Default is `testing_setup$cache_path`.
#' @param temp_cache The temporary cache itself. Default is `testing_setup$temp_cache`.
#'
#' @return `NULL`. This function should be called for its side effect of cleaning up after drake testing.
#'
#' @noRd
clean_up_after_testing <- function(testing_setup, cache_path = testing_setup$cache_path, temp_cache = testing_setup$temp_cache) {
  # Clean up the cache.
  drake::readd("fu_analysis_folder", path = cache_path) %>%
    unlink(recursive = TRUE, force = TRUE)
  temp_cache$destroy()
  return(NULL)
}


#' Create files and data to mimic a final-to-useful analysis directory
#'
#' This function creates a temporary directory structure for final-to-useful analyses
#' in `fu_folder`.
#'
#' Sample data from the `IEATools` package are used when creating the directory structure.
#'
#' This function is helpful during testing, but not at any other times.
#'
#' @param fu_folder The folder in which the final-to-useful analysis structure will be created.
#'
#' @return `NULL`. This function should be called for its side effect of creating a temporary final-to-useful directory structure.
#'
#' @noRd
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

  return(NULL)
}

