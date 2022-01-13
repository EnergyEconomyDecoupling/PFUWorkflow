
#' Load a country concordance table
#'
#' In addition to loading the country concordance table,
#' this function checks for errors:
#' missing 3-letter country codes and repeated 3-letter country codes.
#' Both of these issues will cause a problem.
#'
#' @param country_concordance_path The path to the country concordance file.
#'                                 The file is assumed to be an Excel file.
#' @param sheet The name of the sheet to be read.
#'              Default is "country_concordance_table".
#' @param pfu_code_colname The name of the column in the country concordance table
#'                         that contains 3-letter country codes
#'                         to be used in the workflow.
#'                         Default is "PFU.code".
#'
#' @return A country concordance table.
#'
#' @export
load_country_concordance_table <- function(country_concordance_path,
                                           sheet = "country_concordance_table",
                                           pfu_code_colname = "PFU.code") {
  out <- readxl::read_excel(country_concordance_path, sheet = "country_concordance_table")
  # Ensure there are no duplicated PFU.codes
  # (3-letter country codes that we'll use in the workflow).
  country_codes <- out[[pfu_code_colname]]
  # Ensure no NA's in PFU.code column
  assertthat::assert_that(! any(is.na(country_codes)),
                          msg = paste0("Found empty values in column ",
                                       pfu_code_colname,
                                       " in the country concordance table."))
  # Ensure no repeated values in the PFU.code column.
  assertthat::assert_that(length(country_codes) == length(unique(country_codes)),
                          msg = paste0("Found repeated country codes in the ",
                                       pfu_code_colname,
                                       " column of the country concordance table."))
  return(out)
}


#' Extract specific country data
#'
#' Data is extracted according to the `countries` object in a way that is amenable to drake subtargets.
#' `dplyr::filter()` does the subsetting.
#'
#' @param .df A data frame containing cleaned data with lots of countries.
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
extract_country_data <- function(.df, countries, max_year,
                                 country = IEATools::iea_cols$country,
                                 year = IEATools::iea_cols$year) {
  dplyr::filter(.df, .data[[country]] %in% countries, .data[[year]] <= max_year)
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


