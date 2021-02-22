#' Get all pwt10 data for a set of countries
#'
#' Using the pwt10 package this function creates a data frame containing all the
#' data Penn World Tables 10 (pwt10) for a set of countries as specified in a string
#' of 3-letter ISO country codes named `countries`.
#' Note that some data is not available for some countries (mostly non-OECD).
#'
#' @param countries A string of iso 3-letter country codes
#'
#' @return A data frame containing socioeconomic data from pwt10 for a set of countries.
#' @export
#'
#' @examples
get_all_pwt_data <- function(countries) {

  # Get all pwt10 data and filter for countries in the string countries
  pwt10_data <- pwt10::pwt10.0 %>%
    dplyr::filter(isocode %in% countries)

  # Remove rownames
  rownames(pwt10_data) <- NULL

  return(pwt10_data)

}

#' Create a dataframe containing capital (K), labor (L), and GDP data
#'
#' This function selects the following columns from a pwt10 data frame,
#' with descriptions from pwt10 documentation:
#'   isocode: 3-letter isocode
#'   year: Year
#'   rgdpe: Expenditure-side real GDP at chained PPPs (in million 2017 USD).
#'   rgdpo: Output-side real GDP at chained PPPs (in million 2017 USD).
#'   rgdpna: Real GDP at constant 2017 national prices (in million 2017 USD)
#'   emp: Number of persons engaged (in millions)
#'   avh: Average annual hours worked by persons engaged.
#'   hc: Human capital index, based on years of schooling and returns to education;
#'       see Human capital in PWT9.
#'   rnna: Capital stock at constant 2017 national prices (in million 2017 USD).
#'   rkna: Capital services at constant 2017 national prices (2017 = 1).
#'
#' The metrics  L, the total number of hours worked in a given year
#' and Ladj, the number of hours worked adjusted by the human capital index are
#' also calculated and added as columns, with avh, hc, and emp being removed after use.
#'
#' Note that some data is not available for some countries (mostly non-OECD),
#' some of the calculated metrics i.e. Adjusted Labor (L.adj) is also absent.
#'
#' @param .df A data frame containing all pwt10 data for atleast one country,
#'            usually supplied through calling the `get_all_pwt_data` function.
#'
#' @return A data frame containing three GDP metrics, Labor, Adjusted Labor,
#'         Capital, and Capital services.
#' @export
#'
#' @examples
get_L_K_GDP_data <- function(.df) {

  # Selects columns
  L_K_GDP_data <- pwt10_data %>%
    dplyr::select(isocode, year, rgdpe, rgdpo, rgdpna, emp, avh, hc, rnna, rkna)

  # Renames columns
  L_K_GDP_data <- L_K_GDP_data %>%
    magrittr::set_colnames(c("Country", "Year", "rgdpe", "rgdpo", "rgdpna",
                             "emp", "avh", "hc", "K", "Kserv"))

  # Calculates L, the total number of hours worked in a given year
  # and Ladj, the number of hours worked adjusted by the human capital index
  L_K_GDP_data <- L_K_GDP_data %>%
    dplyr::mutate(L = (emp * avh * 1000000), .keep = "unused", .after = "rgdpna") %>%
    dplyr::mutate(Ladj = (L * hc), .after = "L") %>%
    dplyr::select(-hc)

  return(L_K_GDP_data)

}
