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


#' Assemble completed final-to-useful allocation tables
#'
#' This function is used in a drake workflow to assemble completed final-to-useful allcoation tables.
#'
#' @param countries
#' @param cache_path
#' @param specified_target
#' @param incomplete_allocation_tables_target
#' @param exemplar_lists_target
#'
#' @return
#' @export
#'
#' @examples
assemble_fu_allocation_tables <- function(incomplete_allocation_tables,
                                          exemplar_lists,
                                          specified_iea_data,
                                          countries,
                                          country = IEATools::iea_cols$country,
                                          year = IEATools::iea_cols$year,
                                          exemplars = SEAPSUTWorkflow::exemplar_names$exemplars) {

  exemplar_tables <- lapply(countries, FUN = function(coun) {
    coun_allocation_table <- incomplete_allocation_tables %>%
      dplyr::filter(.data[[country]] == coun)
    coun_exemplar_strings <- exemplar_lists %>%
      dplyr::filter(.data[[country]] == coun)


    # For each combination of Country and Year (the rows of coun_exemplar_strings),
    # assemble a list of country allocation tables.
    # coun_exemplar_strings_and_tables <- coun_exemplar_strings %>%
    #   dplyr::mutate(
    #     exemplar_table_col = Map(get_one_exemplar_table_list,
    #                              incomplete_allocation_tables = incomplete_allocation_tables,
    #                              country = .data[[country]],
    #                              year = .data[[year]],
    #                              exemplar_strings = .data[[exemplars]])
    #   )




    #   tidyr::unnest(.data[[SEAPSUTWorkflow::exemplar_names$exemplars]])
    #
    #
    #
    #
    #   dplyr::mutate(
    #     exemplar_table_col = lapply(.data[[SEAPSUTWorkflow::exemplar_names$exemplars]], function(exemplar_string) {
    #       # exemplar_string is one exemplar for coun for one year.
    #
    #     })
    #   )
    #
    # coun_iea_data <- specified_iea_data %>%
    #   dplyr::filter(.data[[IEATools::iea_cols$country]] == coun)
    # complete_fu_allocation_table(fu_allocation_table = coun_table,
    #                              exemplar_fu_allocation_tables = coun_exemplar_tables,
    #                              tidy_specified_iea_data = coun_iea_data)
  }) %>%
    dplyr::bind_rows()

}






get_one_exemplar_table_list <- function(incomplete_allocation_tables, country, year, exemplar_strings) {
  # print()
}
