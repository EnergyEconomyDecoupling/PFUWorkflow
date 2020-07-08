#' Load FU allocation tables
#'
#' This function reads all final-to-useful allocation data
#' in files in the `fu_analysis_folder` that start with the country prefixes
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#'
#' @export
#'
#' @return A data frame of FU Allocation tables read by `IEATools::load_fu_allocation_data()`.
#'         `NULL` if no data are found.
load_fu_allocation_tables <- function(fu_analysis_folder,
                                      countries,
                                      file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                      use_subfolders = TRUE,
                                      fu_allocations_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name) {
  out <- lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    if (!file.exists(fpath)) {
      return(NULL)
    }
    IEATools::load_fu_allocation_data(fpath, fu_allocations_tab_name = fu_allocations_tab_name)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}


#' Load FU efficiency tables
#'
#' This function reads all final-to-useful efficiency data
#' in files in the `fu_analysis_folder` that start with the country prefixes
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#'
#' @export
#'
#' @return A data frame of FU efficiency tables read by `IEATools::load_eta_fu_data()`.
#'         `NULL` if no data are found.
load_fu_efficiency_tables <- function(fu_analysis_folder,
                                      countries,
                                      file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                      use_subfolders = TRUE) {
  out <- lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    if (!file.exists(fpath)) {
      return(NULL)
    }
    IEATools::load_eta_fu_data(fpath)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}


assemble_fu_allocation_tables <- function(fu_analysis_folder,
                                          countries,
                                          exemplar_lists,
                                          file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                          use_subfolders = TRUE,
                                          cache_path = ".drake/",
                                          name_of_iea_data_object = "Specified") {
  # Try to load FU allocation tables for each of the countries from disk
  allocation_tables <- load_fu_allocation_tables(fu_analysis_folder = fu_analysis_folder,
                                                 countries = countries,
                                                 file_suffix = file_suffix,
                                                 use_subfolders = use_subfolders)

  # Note: Need to write a function that determines if a FU allocation table is complete or not.
  # If the FU allocation table doesn't exist OR if the FU allocation table isn't complete,
    # Load all possible FU allocation exemplar tables from the cache
      # If no FU allocation exemplar tables are present in the cache, fail.
      # If any of the FU allocation exemplar tables are missing from the cache, emit a warning.
    # Complete the country's FU allocation table.
    # Write the FU allocation table to a different file on disk and stop. This step will require manual intervention to move the completed table into the "* FU Analysis.xlsx" file.

  # If the FU allocation table is present and complete, put the FU allocation table into the cache.

}
