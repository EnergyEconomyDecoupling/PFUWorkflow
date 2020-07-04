#' Extract specific country data
#'
#' Data is extracted according to the `countries` object in a way that is amenable to drake subtargets.
#' `dplyr::filter()` does the subsetting.
#'
#' @param AllIEAData A data frame containing cleaned IEA extended energy balance data.
#' @param countries A list of 3-letter country codes for countries to be analyzed.
#' @param max_year The latest year you want to include in the extracted data.
#' @param country,year See `IEATools::iea_cols`.
#'
#' @return a data frame with the desired IEA data only
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   extract_country_data(countries = c("ZAF"), max_year = 1999)
extract_country_data <- function(AllIEAData, countries, max_year,
                                 country = IEATools::iea_cols$country,
                                 year = IEATools::iea_cols$year) {
  dplyr::filter(AllIEAData, .data[[country]] %in% countries, .data[[year]] <= max_year)
}


#' Tells whether IEA data are balanced
#'
#' Performs the the energy balance check in a way that is amenable to drake subtargets.
#' Internally, this function uses [IEATools::calc_tidy_iea_df_balances()].
#' Grouping is doing internal to this function using the value of `grp_vars`.
#'
#' @param IEAData a tidy IEA data frame
#' @param countries the countries for which balancing should be checked as strings
#' @param country The name of the country column in `IEAData`. Default is `r IEATools::iea_cols$country`.
#' @param grp_vars the groups that should be checked. Default is
#'                 `c(country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage, IEATools::iea_cols$product)`.
#'
#' @return a logical stating whether all products are balanced for the country of interest
#'
#' @export
#'
#' @examples
#' # These data are not balanced, because they are raw.
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   is_balanced(countries = "ZAF")
is_balanced <- function(IEAData, countries,
                        country = IEATools::iea_cols$country,
                        grp_vars = c(country,
                                     IEATools::iea_cols$method,
                                     IEATools::iea_cols$energy_type,
                                     IEATools::iea_cols$last_stage,
                                     IEATools::iea_cols$year,
                                     IEATools::iea_cols$product)) {
  dplyr::filter(IEAData, .data[[country]] %in% countries) %>%
    dplyr::group_by(!!as.name(grp_vars)) %>%
    IEATools::calc_tidy_iea_df_balances() %>%
    IEATools::tidy_iea_df_balanced()
}


#' Balance IEA data
#'
#' Balances the IEA data in a way that is amenable to drake subtargets.
#' Internally, this function uses `IEATools::fix_tidy_iea_df_balances()`.
#' Grouping is done internal to this function using the value of `grp_vars`.
#'
#' @param IEAData A tidy IEA data frame
#' @param countries The countries that should be balanced
#' @param grp_vars the groups that should be checked. Default is
#'                 `c(country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage, IEATools::iea_cols$product)`.
#' @param country See `IEATools::iea_cols`
#'
#' @return balanced IEA data
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   is_balanced(countries = c("GHA", "ZAF"))
make_balanced <- function(IEAData,
                          countries,
                          country = IEATools::iea_cols$country,
                          grp_vars = c(country,
                                       IEATools::iea_cols$method,
                                       IEATools::iea_cols$energy_type,
                                       IEATools::iea_cols$last_stage,
                                       IEATools::iea_cols$year,
                                       IEATools::iea_cols$product)) {
  dplyr::filter(IEAData, .data[[country]] %in% countries) %>%
    dplyr::group_by(!!as.name(grp_vars)) %>%
    IEATools::fix_tidy_iea_df_balances() %>%
    dplyr::ungroup()
}


#' Specify the IEA data
#'
#' Specifies the IEA data in a way that is amenable to drake subtargets.
#' See `IEATools::specify_all()` for details.
#'
#' @param BalancedIEAData IEA data that have already been balanced
#' @param countries the countries for which specification should occur
#' @param country See `IEATools::iea_cols`.
#'
#' @return a data frame of specified IEA data
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   specify(countries = c("GHA", "ZAF"))
specify <- function(BalancedIEAData,
                    countries,
                    country = IEATools::iea_cols$country) {
  dplyr::filter(BalancedIEAData, .data[[country]] %in% countries) %>%
    IEATools::specify_all()
}


#' Convert to PSUT matrices
#'
#' Converts tidy IEA data to PSUT matrices in a way that is amenable to drake subtargets.
#' Internally, `IEATools::prep_psut()` does the conversion to matrices.
#'
#' @param SpecifiedIEAData A data frame that has already been specified via `specify()`.
#' @param countries The countries you want to convert to PSUT matrices.
#' @param country See `IEATools::iea_cols`.
#'
#' @return a `matsindf`-style data frame
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   specify(countries = c("GHA", "ZAF")) %>%
#'   make_psut(countries = c("GHA", "ZAF"))
make_psut <- function(SpecifiedIEAData,
                      countries,
                      country = IEATools::iea_cols$country) {
  dplyr::filter(SpecifiedIEAData, .data[[country]] %in% countries) %>%
    IEATools::prep_psut()
}


#' Load FU allocation and efficiency files
#'
#' This function reads all files in the `fu_analysis_folder` that start with the country prefixes
#' given in `countries`.
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
