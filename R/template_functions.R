#' Create a final-to-useful allocation template
#'
#' This function creates a blank final-to-useful allocation template on a per-country basis.
#'
#' The only arguments used internally are `country`, `data_target`, `cache_path`,
#' `overwrite`, `n_allocation_rows`, and `output_path`,
#' allowing the caller to specify only output_path to override all of the
#' path information stored in the drake cache.
#'
#' @param country A string, the 3-letter ISO code of the country for which a template is desired.
#' @param cache_path The path to the drake cache. Default is ".drake/".
#' @param data_target The target in the drake cache from which the template is created.
#'                    Default is "Specified".
#' @param fu_analysis_path_target The name of the target in the drake cache containing path
#'                                to the final-to-useful analyses.
#'                                Default is "fu_analysis_folder".
#' @param fu_allocation_template_file_name The file name for the template.
#'                                         Default is "`country` FU Allocations Template.`ext`".
#' @param ext The output file name extension.
#'            Default is ".xlsx" for an Excel file.
#' @param output_path The path of the output file to be created.
#'                    Default uses arguments `country`, `fu_analysis_path_target`,
#'                    `fu_allocation_template_file_name`, and
#'                    `ext` to construct a path.
#' @param n_allocation_rows The number of allocation rows per combination of sector and final energy carrier.
#'                          Default is `4`.
#' @param overwrite Tells whether to overwrite an existing file at `output+path`.
#'                  Default is `FALSE`.
#'
#' @return the path to the blank final-to-useful allocation template
#'
#' @export
generate_fu_allocation_template <- function(country,
                                            cache_path = ".drake/",
                                            data_target = "Specified",
                                            fu_analysis_path_target = "fu_analysis_folder",
                                            fu_allocation_template_file_name = paste0(country, " FU Allocations Template"),
                                            ext = ".xlsx",
                                            output_path = file.path(drake::readd(fu_analysis_path_target,
                                                                                 character_only = TRUE,
                                                                                 path = cache_path),
                                                                    country) %>%
                                              dir.create.pipe(showWarnings = FALSE, recursive = TRUE) %>%
                                              file.path(paste0(fu_allocation_template_file_name, ext)),
                                            n_allocation_rows = 4,
                                            overwrite = FALSE) {
  # Get the specified data for this country from the drake cache
  readd_by_country(data_target, country, cache_path = cache_path) %>%
    # Create the blank allocation template
    IEATools::fu_allocation_template() %>%
    # Write the blank allocation template
    IEATools::write_fu_allocation_template(path = output_path, overwrite_file = overwrite, n_allocation_rows = n_allocation_rows)
}


#' Create a final-to-useful efficiencies template
#'
#' The only arguments used internally are `country`, `fu_allocation_table_path`, `fu_allocation_tab_name`k
#' `eta_fu_template_file_name`
#' `overwrite`, and `output_path`,
#' allowing the caller to specify only output_path to override all of the
#' path information stored in the drake cache.
#'
#' @param country A string of the 3-letter ISO country code
#' @param country_col A string containing the name of the country column in the final-to-useful allocation table.
#'                    Default is `IEATools::iea_cols$country`.
#' @param cache_path The path to the drake cache. Default is ".drake/".
#' @param fu_analysis_path_target The name of the target in the drake cache containing path to the final-to-useful analyses.
#'                                Default is "fu_analysis_folder".
#' @param fu_allocation_table_file_name The name of the file containing the final-to-useful allocations for `country`.
#'                                      Default is "`country` FU Analysis.`ext`".
#' @param ext The file name extension for both the allocation table and the efficiency template.
#'            Default is ".xlsx" for an Excel file.
#' @param fu_allocation_table_path The path the the
#' @param fu_allocation_tab_name The name of the tab containing final-to-useful allocation information in `fu_analysis_file_name`. Default is "`r IEATools::fu_analysis_file_info$fu_allocation_tab_name`".
#' @param eta_fu_template_file_name The name of the efficiency template file written by this function.
#'                                  Default is "`country` FU etas Template.`ext`".
#' @param output_path The path to the output file.
#'                    Default is constructed from the drake cache (`fu_analysis_path_target`)
#'                    and `country`, and `eta_fu_template_file_name` arguments.
#' @param overwrite Tells whether to overwrite an existing file named `eta_fu_template_file_name`.`ext` in location `fu_analysis_path_name`.
#'                  Default is `FALSE`.
#'
#' @return the path to the blank final-to-useful efficiency template
#'
#' @export
generate_eta_fu_template <- function(country,
                                     country_col = IEATools::iea_cols$country,
                                     cache_path = ".drake/",
                                     fu_analysis_path_target = "fu_analysis_folder",
                                     fu_allocation_table_file_name = paste0(country, IEATools::fu_analysis_file_info$fu_analysis_file_suffix),
                                     ext = ".xlsx",
                                     fu_allocation_table_path = file.path(drake::readd(fu_analysis_path_target,
                                                                                       character_only = TRUE,
                                                                                       path = cache_path),
                                                                          country) %>%
                                       dir.create.pipe(showWarnings = FALSE, recursive = TRUE) %>%
                                       file.path(paste0(fu_allocation_table_file_name, ext)),
                                     fu_allocation_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name,
                                     eta_fu_template_file_name = paste0(country, " FU etas Template"),
                                     output_path = file.path(drake::readd(fu_analysis_path_target,
                                                                          character_only = TRUE,
                                                                          path = cache_path),
                                                             country) %>%
                                       dir.create.pipe(showWarnings = FALSE, recursive = TRUE) %>%
                                       file.path(paste0(eta_fu_template_file_name, ext)),
                                     overwrite = FALSE) {
  # Read the allocations from the file at fu_analysis_file_name
  IEATools::load_fu_allocation_data(fu_allocation_table_path, fu_allocations_tab_name = fu_allocation_tab_name) %>%
    dplyr::filter(country %in% .data[[country_col]]) %>%
    # Create the eta_fu template
    IEATools::eta_fu_template() %>%
    # Write the blank final-to-useful efficiencies template
    IEATools::write_eta_fu_template(output_path, overwrite_file = overwrite)
}
