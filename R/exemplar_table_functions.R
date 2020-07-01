



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
                                country = IEATools::iea_cols$country,
                                year = IEATools::iea_cols$year,
                                prev_names = "Prev.names") {

  raw <- readxl::read_excel(path = exemplar_table_path, sheet = exemplar_table_tab_name)
  # Figure out year columns.
  year_columns <- IEATools::year_cols(raw, return_names = TRUE)
  max_year <- year_columns %>%
    as.numeric() %>%
    max() %>%
    as.character()

  # Set the Country column from the most recent year available.
  raw %>%
    dplyr::mutate(
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
                           year = IEATools::iea_cols$year,
                           year_temp = "Year.temp",
                           exemplars = "Exemplars",
                           prev_names = "Prev.names",
                           prev_names_list = "Prev.names.list",
                           exemplar_country = "Exemplar.country",
                           row_code = "ROW.code",
                           countries_or_group = "Country.name",
                           world = "World") {

  # A data frame that consists of all year and country combinations in exemplar_table
  year_country <- exemplar_table %>%
    dplyr::select(year, country)

  pntable <- exemplar_table %>%
    dplyr::select(country, year, prev_names) %>%
    dplyr::rename("{year_temp}" := year) %>%
    tidyr::nest("{prev_names}" := c(year_temp, prev_names))

  # First step: join the previous names to each country and year
  with_prev_names_list <- dplyr::left_join(year_country, pntable, by = country) %>%
    tidyr::unnest(cols = prev_names) %>%
    dplyr::filter(.data[[year_temp]] <= .data[[year]]) %>%
    dplyr::select(!year_temp) %>%
    dplyr::group_by(.data[[year]], .data[[country]]) %>%
    unique() %>%
    dplyr::filter(.data[[country]] != .data[[prev_names]]) %>%
    dplyr::summarise(
      "{prev_names_list}" := list(.data[[prev_names]] %>% rev())
    )

  # Join these lists back to the original data frame
  outgoing <- exemplar_table %>%
    dplyr::left_join(with_prev_names_list, by = c(country, year)) %>%
    dplyr::select(!prev_names) %>%
    dplyr::rename("{prev_names}" := prev_names_list) %>%
    dplyr::mutate(
      "{prev_names}" := as.character(.data[[prev_names]]),
      # Weirdly, the preceding line converts NULLs into "NULL".
      # Check for these and convert to a 0-length character
      "{prev_names}" := dplyr::case_when(
        .data[[prev_names]] == "NULL" ~ "",
        TRUE ~ .data[[prev_names]]
      )
    ) %>%
    # Build our list of exemplar countries,
    # starting with the previous names, followed by the assigned exemplar country,
    # this country's code for the rest of the world, and
    # the world region.
    dplyr::mutate(
      "{exemplars}" := Map(f = function(p_names, exemp, restofworldcode) {
        list(c(p_names, exemp, restofworldcode, world))
      },
      p_names = .data[[prev_names_list]],
      exemp = .data[[exemplar_country]],
      restofworldcode = .data[[row_code]])
    )

  #
  # The following commented code actually works.
  # But I'm working on a faster method.
  #

  # # Figure out all previous names for each country.
  # # These combinations are stored as a nested data frame inside pntable.
  # pntable <- exemplar_table %>%
  #   dplyr::select(country, year, prev_names) %>%
  #   tidyr::nest("{prev_names}" := c(year, prev_names))
  #
  # # First step: join the previous names to each country and year
  # with_prev_names_list <- dplyr::left_join(year_country, pntable, by = country) %>%
  #   # Now trim the data frame of previous names by the year of interest in the outer data frame.
  #   # This operation (probably the filtering) is really slow. Maybe a better way to do it?
  #   # Perhaps use a rowwise approach?
  #   dplyr::mutate(
  #     "{prev_names_list}" := Map(f = function(yr, prevn) {
  #       temp <- prevn %>%
  #         dplyr::filter(.data[[year]] <= yr) %>%
  #         # column 2 is the column with tables of previous names
  #         magrittr::extract2(2) %>%
  #         as.list() %>%
  #         # We want to keep only the previous names that are different from the others
  #         unique() %>%
  #         # Reverse, because the order now is oldest to most-recent previous name.
  #         # We want to go most-recent to oldest previous name.
  #         rev()
  #       # Remove the first element of the list,
  #       # because it is always the current country name, and
  #       # return it
  #       temp[-1]
  #     }, yr = .data[[year]], prevn = .data[[prev_names]])
  #   ) %>%
  #   # We can eliminate the column that doesn't contain the list.
  #   dplyr::select(!prev_names)
  #
  # outgoing <- exemplar_table %>%
  #   dplyr::select(!prev_names) %>%
  #   # Join this new data frame with the incoming one.
  #   dplyr::left_join(with_prev_names_list, by = c(country, year)) %>%
  #   # Build our list of exemplar countries,
  #   # starting with the previous names, followed by the assigned exemplar country,
  #   # this country's code for the rest of the world, and
  #   # the world region.
  #   dplyr::mutate(
  #     "{exemplars}" := Map(f = function(p_names, exemp, restofworldcode) {
  #       c(p_names, exemp, restofworldcode, world) %>%
  #         unlist(recursive = TRUE, use.names = FALSE)
  #     },
  #     p_names = .data[[prev_names_list]],
  #     exemp = .data[[exemplar_country]],
  #     restofworldcode = .data[[row_code]])
  #   )






}

