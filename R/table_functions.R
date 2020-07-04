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
#' @param file_suffix The suffix for the FU analysis files. Default is " FU Analysis.xlsx".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#'
#' @export
#'
#' @return A data frame of FU Allocation tables read by `IEATools::load_fu_allocation_data()`.
load_fu_allocation_tables <- function(fu_analysis_folder,
                                      countries,
                                      file_suffix = " FU Analysis.xlsx",
                                      use_subfolders = TRUE) {
  lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    IEATools::load_fu_allocation_data(fpath)
  }) %>%
    dplyr::bind_rows()
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
#' @param file_suffix The suffix for the FU analysis files. Default is " FU Analysis.xlsx".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#'
#' @export
#'
#' @return A data frame of FU efficiency tables read by `IEATools::load_eta_fu_data()`.
load_fu_efficiency_tables <- function(fu_analysis_folder,
                                      countries,
                                      file_suffix = " FU Analysis.xlsx",
                                      use_subfolders = TRUE) {
  lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    IEATools::load_eta_fu_data(fpath)
  }) %>%
    dplyr::bind_rows()
}


