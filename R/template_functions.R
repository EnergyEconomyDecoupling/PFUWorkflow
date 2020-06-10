#' Create a final-to-useful allocation template
#'
#' @param country A string, the 3-letter ISO code of the country to be analyzed.
#' @param data_target The target in the drake cache from which the template is created. Default is "Specified".
#' @param fu_analysis_path_target The name of the target in the drake cache containing path to the final-to-useful analyses. Default is "fu_analysis_folder".
#' @param fu_allocation_template_file_name The file name for the template. Default is "`country` FU Allocations Template.`ext`".
#' @param ext the file name extension. Default is ".xlsx" for an Excel file.
#' @param n_allocation_rows the number of allocation rows per combination of sector and final energy carrier. Default is `4`.
#' @param overwrite tells whether to overwrite an existing file named `fu_allocation_file_name`.`ext` in location `fu_analysis_path_name`.
#'        Default is `FALSE`.
#' @param cache_path The path to the drake cache. Default is ".drake/".
#'
#' @return the path to the blank final-to-useful allocation template
#'
#' @export
generate_fu_allocation_template <- function(country,
                                            data_target = "Specified",
                                            fu_analysis_path_target = "fu_analysis_folder",
                                            fu_allocation_template_file_name = paste0(country, " FU Allocations Template"),
                                            ext = ".xlsx",
                                            n_allocation_rows = 4,
                                            overwrite = FALSE,
                                            cache_path = ".drake/") {
  # Construct the output path from the FU analysis folder and fu_allocation_template_file_name
  output_folder <- drake::readd(fu_analysis_path_target, character_only = TRUE, path = cache_path)
  dir.create(output_folder, showWarnings = FALSE)
  output_path <- file.path(output_folder, paste0(fu_allocation_template_file_name, ext))
  # Get the specified data for this country from the drake cache
  readd_by_country(data_target, country, cache_path = cache_path) %>%
    # Create the blank allocation template
    IEATools::fu_allocation_template() %>%
    # Write the blank allocation template
    IEATools::write_fu_allocation_template(path = output_path, overwrite_file = overwrite, n_allocation_rows = n_allocation_rows)
}


#' Create a final-to-useful efficiencies template
#'
#' @param country a string of the 3-letter ISO country code
#' @param paths_target the name of the paths object in the drake cache. Default is "paths".
#' @param fu_analysis_path_target The name of the target in the drake cache containing path to the final-to-useful analyses. Default is "fu_analysis_folder".
#' @param fu_analysis_file_name the name of the file containing the final-to-useful analysis for `country`.
#'        Default is "`country` FU Analysis.`ext`".
#' @param ext the file name extension. Default is ".xlsx" for an Excel file.
#' @param fu_allocation_tab_name the name of the tab containing final-to-useful allocation information in `fu_analysis_file_name`.
#'        Default is "FU Allocations".
#' @param eta_fu_template_file_name the name of the template file written by this function.
#'        Default is "`country` FU etas Template.`ext`".
#' @param overwrite tells whether to overwrite an existing file named `eta_fu_template_file_name`.`ext` in location `fu_analysis_path_name`.
#'        Default is `FALSE`.
#' @param cache_path The path to the drake cache. Default is ".drake/".
#'
#' @return the path to the blank final-to-useful efficiency template
#'
#' @export
generate_eta_fu_template <- function(country,
                                     paths_target = "paths",
                                     fu_analysis_path_target = "fu_analysis_folder",
                                     fu_analysis_file_name = paste0(country, " FU Analysis"),
                                     ext = ".xlsx",
                                     fu_allocation_tab_name = "FU Allocations",
                                     eta_fu_template_file_name = paste0(country, " FU etas Template"),
                                     overwrite = FALSE,
                                     cache_path = ".drake/") {
  # Construct the path to the input file (which contains the FU Allocation tab)
  # from the fu_analysis_path_name and fu_analysis_file_name
  input_path <- file.path(drake::readd(fu_analysis_path_target, character_only = TRUE, path = cache_path),
                          country,
                          paste0(fu_analysis_file_name, ext))
  # Construct the output path from the FU analysis folder and eta_fu_template_file_name
  output_folder <- file.path(drake::readd(fu_analysis_path_target, character_only = TRUE, path = cache_path),
                             country)
  dir.create(output_folder, showWarnings = FALSE)
  output_path <- file.path(output_folder, paste0(eta_fu_template_file_name, ext))
  # Read the allocations from the file at fu_analysis_file_name
  IEATools::load_fu_allocation_data(input_path) %>%
    # Create the eta_fu template
    IEATools::eta_fu_template() %>%
    # Write the blank final-to-useful efficiencies template
    IEATools::write_eta_fu_template(output_path, overwrite_file = overwrite)
}
