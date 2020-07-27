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
#' @param countries The countries for which exemplars are desired. If `NULL`, the default,
#'                  all countries in the file at `exemplar_table_path` are returned.
#' @param exemplar_table_tab_name,prev_names See `SEAPSUTWorkflow::exemplar_names`.
#' @param year,country See `IEATools::iea_cols`.
#'
#' @return an exemplar table
#'
#' @export
#'
#' @examples
#' load_exemplar_table()
load_exemplar_table <- function(exemplar_table_path = sample_exemplar_table_path(),
                                countries = NULL,
                                exemplar_table_tab_name = SEAPSUTWorkflow::exemplar_names$exemplar_tab_name,
                                prev_names = SEAPSUTWorkflow::exemplar_names$prev_names,
                                country = IEATools::iea_cols$country,
                                year = IEATools::iea_cols$year) {

  raw <- readxl::read_excel(path = exemplar_table_path, sheet = exemplar_table_tab_name)
  # Figure out year columns.
  year_columns <- IEATools::year_cols(raw, return_names = TRUE)
  max_year <- year_columns %>%
    as.numeric() %>%
    max() %>%
    as.character()

  # Set the Country column from the most recent year available.
  out <- raw %>%
    dplyr::mutate(
      "{country}" := .data[[max_year]]
    ) %>%
    # Gather columns of alternative names
    tidyr::pivot_longer(cols = tidyselect::all_of(year_columns), names_to = year, values_to = prev_names)
  if (!is.null(countries)) {
    out <- out %>%
      dplyr::filter(.data[[country]] %in% countries)
  }
  return(out)
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
#' @param exemplar_table An exemplar table, probably read by `load_exemplar_table()`.
#' @param countries The countries for which exemplar lists are desired. Default is `NULL`, which returns all known countries.
#' @param exemplars,prev_names,exemplar_country,row_code,world See `SEAPSUTWorkflow::exemplar_names`.
#' @param country,year See `IEATools::iea_cols`.
#' @param year_temp The name of a temporary year column. Default is ".year_temp".
#' @param prev_names_list The name of a temporary column in `exemplar_table`. Default is ".prev_names_list".
#'
#' @return A tibble containing countries and regions to be searched for missing
#'         final-to-useful allocation data or final-to-useful efficiency data.
#'
#' @export
#'
#' @examples
#' # Use an exemplar table that is part of this package.
#' el <- exemplar_lists(load_exemplar_table()) %>%
#' # Montenegro is a particularly interesting case, as it had many name changes.
#' # Look at the first year, the last year as Yugoslavia, the first year as Serbia,
#' # and today.
#'   dplyr::filter(.data[[IEATools::iea_cols$country]] == "MNE",
#'                 .data[[IEATools::iea_cols$year]] %in% c(1971, 1989, 1990, 2017))
#' el
#' el[[1, "Exemplars"]]
#' el[[2, "Exemplars"]]
#' el[[3, "Exemplars"]]
#' el[[4, "Exemplars"]]
exemplar_lists <- function(exemplar_table,
                           countries = NULL,
                           prev_names = SEAPSUTWorkflow::exemplar_names$prev_names,
                           exemplar_country = SEAPSUTWorkflow::exemplar_names$exemplar_country,
                           exemplars = SEAPSUTWorkflow::exemplar_names$exemplars,
                           row_code = SEAPSUTWorkflow::exemplar_names$row_code,
                           world = SEAPSUTWorkflow::exemplar_names$world,
                           country = IEATools::iea_cols$country,
                           year = IEATools::iea_cols$year,
                           year_temp = ".year_temp",
                           prev_names_list = ".prev_names_list") {

  # A data frame that consists of all year and country combinations in exemplar_table
  year_country <- exemplar_table %>%
    dplyr::select(year, country)

  pntable <- exemplar_table %>%
    dplyr::select(country, year, prev_names) %>%
    dplyr::rename("{year_temp}" := year) %>%
    tidyr::nest("{prev_names}" := c(year_temp, prev_names))

  # First step: join the previous names to each country and year
  with_prev_names_list <- dplyr::left_join(year_country, pntable, by = country) %>%
    # Unnest to get all years.
    tidyr::unnest(cols = prev_names) %>%
    # Eliminate rows that don't apply, i.e.,
    # those rows where the year of the alternative name is AFTER the year of interest
    dplyr::filter(.data[[year_temp]] <= .data[[year]]) %>%
    # Eliminate an unneeded column.
    dplyr::select(!year_temp) %>%
    # Keep only the unique name regions.
    dplyr::group_by(.data[[year]], .data[[country]]) %>%
    unique() %>%
    # Eliminate the current name of the country from its list of previous names.
    dplyr::filter(.data[[country]] != .data[[prev_names]]) %>%
    # Build a list column containing all previous names for the given country.
    dplyr::summarise(
      "{prev_names_list}" := list(.data[[prev_names]] %>% rev())
    )

  out <- exemplar_table %>%
    # Join these lists back to the original data frame
    dplyr::left_join(with_prev_names_list, by = c(country, year)) %>%
    # Do some renaming
    dplyr::select(!prev_names) %>%
    dplyr::rename("{prev_names}" := prev_names_list) %>%
    # Create the full list of exemplars and store in the exemplars column.
    # Use the helper function to do this.
    dplyr::mutate(
      "{exemplars}" := Map(f = build_one_exemplar_list,
                           p_names = .data[[prev_names]],
                           exemp = .data[[exemplar_country]],
                           restofworldcode = .data[[row_code]],
                           world = world)
    ) %>%
    # {exemplars} is a column with entries like list(c(coun1, coun2, ...)).
    # Unnest to get only c(coun1, coun2, ...).
    tidyr::unnest(.data[[exemplars]])

  # filter by country, if that was requested.
  if (!is.null(countries)) {
    out <- out %>%
      dplyr::filter(.data[[country]] %in% countries)
  }

  # Eliminate unneeded columns
  out <- out %>%
    dplyr::select(country, year, exemplars) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  return(out)

  #
  # The following commented code actually works.
  # But the approach above is MUCH faster.
  # The commented code below can be deleted after, say, August 2020.
  # ---MKH
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


#' Make a list of exemplar countries
#'
#' This is a non-public, internal helper function
#'
#' @param p_names A character vector of previous names for this country.
#' @param exemp An exemplar country.
#' @param restofworldcode The rest-of-world code for this country.
#' @param world A string indicating the world.
#'
#' @return A list of length 1 containing all possible exemplars, in the order in which they apply.
#'
#' @noRd
build_one_exemplar_list <- function(p_names, exemp, restofworldcode, world) {
  list(c(unlist(p_names), exemp, restofworldcode, world))
}
