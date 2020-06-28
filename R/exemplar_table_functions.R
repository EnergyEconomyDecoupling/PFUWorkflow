



#' Gives a file path to a sample exemplar table
#'
#' @return a path to a sample exemplar table bundled with this package
#'
#' @export
#'
#' @examples
#' sample_exemplar_table_path()
sample_exemplar_table_path <- function() {
  file.path("extdata", "Exemplar_Table.xlsx") %>%
    system.file(package = "SEAPSUTWorkflow")
}


#' Read an exemplar table from an Excel file
#'
#' An exemplar table has a column of country names
#' and additional metadata,
#' including the exemplar country and any name changes through time.
#'
#' This function gathers (actually, `tidyr::pivot_longer()`) years into a Year column.
#' It also
#'
#' @param exemplar_table_path The path to the Excel file containing an exemplar table.
#'                            Default is the value of `sample_exemplar_table_path()`.
#' @param exemplar_table_tab_name The string name of the tab in the Excel file containing the exemplar table.
#'                                Default is "exemplar_table".
#' @param year The name of the year column to be created. Default is "Year".
#' @param country The name of the country column to be created.
#'                This column is filled with the current 3-letter ISO abbreviations for each country.
#'                Default is "Country".
#' @param prev_names The name of a column of previous names used for the country.
#'                   Default is "Prev.names".
#'
#' @return an exemplar table
#'
#' @export
#'
#' @examples
#' load_exemplar_table()
load_exemplar_table <- function(exemplar_table_path = sample_exemplar_table_path(),
                                exemplar_table_tab_name = "exemplar_table",
                                year = "Year",
                                country = "Country",
                                prev_names = "Prev.names") {
  prelim <- readxl::read_excel(path = exemplar_table_path, sheet = exemplar_table_tab_name)
  # Figure out year columns.
  year_columns <- IEATools::year_cols(prelim, return_names = TRUE)
  max_year <- year_columns %>%
    as.numeric() %>%
    max() %>%
    as.character()

  prelim %>% dplyr::mutate(
      "{country}" := .data[[max_year]]
    ) %>%
    # Gather columns of alternative names
    tidyr::pivot_longer(cols = tidyselect::all_of(year_columns), names_to = year, values_to = prev_names)
}


#' Create exemplar lists from an exemplar table
#'
#' An exemplar list is a list of countries or regions
#' that should be used to complete a country's
#' final-to-useful allocation table
#' or its efficiency table.
#' This function uses an exemplar table
#' (probably read by `load_exemplar_table()`)
#' to create exemplar lists.
#'
#' The value of this function is a tibble with three columns:
#' Country, Year, and Exemplars.
#' Country contains 3-letter ISO country codes
#' or names of world regions.
#' Year contains years.
#' Exemplars contains a list of countries or regions
#' that should serve as exemplars for the country in that year.
#'
#' Order in each of the exemplar lists is important,
#' because countries and regions will be searched
#' in the order they appear in the sub-list
#' when searching for missing allocations and efficiencies.
#'
#' @return A tibble containing countries and regions to be searched for missing
#'         final-to-useful allocation data or final-to-useful efficiency data.
#'
#' @export
#'
#' @examples
exemplar_lists <- function(exemplar_table,
                           country = IEATools::iea_cols$country,
                           exemplars = "Exemplars",
                           prev_names = "Prev.names",
                           exemplar_country = "Exemplar.country",
                           row_code = "ROW.code",
                           countries_or_group = "Country.name",
                           world = "World") {
  # Construct a list of exemplars from nearby rows in exemplar_table.
  # Put the list in the Exemplars column.
  exemplar_table %>%
    dplyr::mutate(
      "{countries_or_group}" := NULL,
      "{exemplars}" := Map(f = function(coun, row){list(coun, row)}, .data[[exemplar_country]], .data[[row_code]])
    )


}

