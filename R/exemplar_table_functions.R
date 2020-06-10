



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
#'                   Default is "Prev_names".
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
                                prev_names = "Prev_names") {
  prelim <- readxl::read_excel(path = exemplar_table_path, sheet = exemplar_table_tab_name)
  # Figure out year columns.
  # Assume that any column name that can be converted to an integer is a year.
  year_cols <- colnames(prelim) %>%
    strtoi()
  year_cols <- year_cols[!is.na(year_cols)]
  max_year <- max(year_cols)
  # Convert each to a string.
  year_cols <- lapply(year_cols, FUN = function(y) {
    toString(y)
  }) %>%
    # Convert from list to vector.
    unlist()

  prelim %>% dplyr::mutate(
      "{country}" := .data[[toString(max_year)]]
    ) %>%
    # Gather columns of alternative names
    tidyr::pivot_longer(cols = tidyselect::all_of(year_cols), names_to = year, values_to = prev_names)
}
