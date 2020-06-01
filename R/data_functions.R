#' Extract specific country data
#'
#' Data is extracted according to the `countries` object in a way that is amenable to drake subtargets.
#' `dplyr::filter()` does the subsetting.
#'
#' @param AllIEAData A data frame containing cleaned IEA extended energy balance data.
#' @param countries A list of 3-letter country codes for countries to be analyzed.
#' @param max_year The latest year you want to include in the extracted data.
#'
#' @return a data frame with the desired IEA data only
#'
#' @export
extract_country_data <- function(AllIEAData, countries, max_year) {
  dplyr::filter(AllIEAData, Country %in% countries, Year <= max_year)
}


#' Tells whether IEA data are balanced
#'
#' Performs the the energy balance check in a way that is amenable to drake subtargets.
#' Internally, this function uses [IEATools::calc_tidy_iea_df_balances()].
#' Grouping is doing internal to this function using the value of `grp_vars`.
#'
#' @param IEAData a tidy IEA data frame
#' @param countries the countries for which balancing should be checked as strings
#' @param grp_vars the groups that should be checked.  Default is `c("Country", "Method", "Energy.type", "Last.stage", "Product")`.
#' @param country_colname The name of the country column in `IEAData`. Default is "Country".
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
is_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product"),
                                                         country_colname = "Country") {
  dplyr::filter(IEAData, .data[[country_colname]] %in% countries) %>%
    dplyr::group_by(!!as.name(grp_vars)) %>%
    IEATools::calc_tidy_iea_df_balances() %>%
    IEATools::tidy_iea_df_balanced()
}


#' Balance IEA data
#'
#' Balances the IEA data in a way that is amenable to drake subtargets.
#' Internally, this function uses `IEATools::fix_tidy_iea_df_balances()`.
#' Grouping is doing internal to this function using the value of `grp_vars`.
#'
#' @param IEAData A tidy IEA data frame
#' @param countries The countries that should be balanced
#' @param grp_vars The groups that should be checked for energy balance.
#'                 Default is `c("Country", "Method", "Energy.type", "Last.stage", "Product")`.
#' @param country_colname The name of the country column in `IEAData`. Default is "Country".
#'
#' @return balanced IEA data
#'
#' @export
make_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product"),
                          country_colname = "Country") {
  dplyr::filter(IEAData, .data[[country_colname]] %in% countries) %>%
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
#' @param countries the countries for which specificaion should occur
#'
#' @return a data frame of specified IEA data
#'
#' @export
specify <- function(BalancedIEAData, countries) {
  dplyr::filter(BalancedIEAData, Country %in% countries) %>%
    IEATools::specify_all()
}


#' Convert to PSUT matrices
#'
#' Converts tidy IEA data to PSUT matrices in a way that is amenable to drake subtargets.
#' Internally, `IEATools::prep_psut()` does the conversion to matrices.
#'
#' @param SpecifiedIEAData A data frame that has already been specified via `specify()`.
#' @param countries The countries you want to convert to PSUT matrices.
#'
#' @return a `matsindf`-style data frame
#' @export
make_psut <- function(SpecifiedIEAData, countries) {
  dplyr::filter(SpecifiedIEAData, Country %in% countries) %>%
    IEATools::prep_psut()
}
