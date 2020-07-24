#' Load FU allocation tables
#'
#' This function reads all final-to-useful allocation data
#' in files in the `fu_analysis_folder` that start with the country prefixes
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#' @param fu_allocations_tab_name The name of the tab for final-to-useful allocations in the Excel file containing final-to-useful allocation data. Default is "`r IEATools::fu_analysis_file_info$fu_allocation_tab_name`".
#'
#' @export
#'
#' @return A data frame of FU Allocation tables read by `IEATools::load_fu_allocation_data()`.
#'         `NULL` if no data are found.
load_fu_allocation_tables <- function(fu_analysis_folder,
                                      countries,
                                      file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                      use_subfolders = TRUE,
                                      fu_allocations_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name) {
  out <- lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    if (!file.exists(fpath)) {
      return(NULL)
    }
    IEATools::load_fu_allocation_data(fpath, fu_allocations_tab_name = fu_allocations_tab_name)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}


#' Load FU efficiency tables
#'
#' This function reads all final-to-useful efficiency data
#' in files in the `fu_analysis_folder` that start with the country prefixes
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`.
#'
#' @export
#'
#' @return A data frame of FU efficiency tables read by `IEATools::load_eta_fu_data()`.
#'         `NULL` if no data are found.
load_eta_fu_tables <- function(fu_analysis_folder,
                               countries,
                               file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                               use_subfolders = TRUE) {
  out <- lapply(countries, FUN = function(coun) {
    fpath <- file.path(fu_analysis_folder)
    if (use_subfolders) {
      fpath <- file.path(fpath, coun)
    }
    fpath <- file.path(fpath, paste0(coun, file_suffix))
    if (!file.exists(fpath)) {
      return(NULL)
    }
    IEATools::load_eta_fu_data(fpath)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}


#' Assemble completed final-to-useful allocation tables
#'
#' This function is used in a drake workflow to assemble completed final-to-useful allocation tables
#' given a set of incomplete allocation tables.
#'
#' Note that this function can accept tody or wide by year data frames.
#' The return value is a tidy data frame.
#'
#' @param incomplete_allocation_tables A data frame containing (potentially) incomplete final-to-useful allocation tables.
#'                                     This data frame may be tidy or wide by years.
#' @param exemplar_lists A data frame containing `country` and `year` columns along with a column of ordered vectors of strings
#'                       telling which countries should be considered exemplars for the country and year of this row.
#' @param specified_iea_data A data frame containing specified IEA data.
#' @param countries A vector of countries for which completed final-to-useful allocation tables are to be assembled.
#' @param country,year See `IEATools::iea_cols`.
#' @param exemplars,exemplar_tables,iea_data,incomplete_alloc_tables,complete_alloc_tables
#'                    See `SEAPSUTWorkflows::exemplar_names`.
#'
#' @return A tidy data frame containing completed final-to-useful allocation tables.
#'
#' @export
#'
#' @examples
#' # Load final-to-useful allocation tables, but eliminate one category of consumption,
#' # Residential consumption of Primary solid biofuels,
#' # which will be filled by the exemplar for GHA, ZAF.
#' incomplete_fu_allocation_tables <- IEATools::load_fu_allocation_data() %>%
#'   dplyr::filter(! (Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential"))
#' # Show that those rows are gone.
#' incomplete_fu_allocation_tables %>%
#'   dplyr::filter(Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
#' # But the missing rows of GHA are present in allocation data for ZAF.
#' incomplete_fu_allocation_tables %>%
#'   dplyr::filter(Country == "ZAF" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
#' # Set up exemplar list
#' el <- tibble::tribble(
#'   ~Country, ~Year, ~Exemplars,
#'   "GHA", 1971, c("ZAF"),
#'   "GHA", 2000, c("ZAF"))
#' el
#' # Load IEA data
#' iea_data <- IEATools::load_tidy_iea_df() %>%
#'   IEATools::specify_all()
#' # Assemble complete allocation tables
#' completed <- assemble_fu_allocation_tables(incomplete_allocation_tables =
#'                                              incomplete_fu_allocation_tables,
#'                                            exemplar_lists = el,
#'                                            specified_iea_data = iea_data,
#'                                            countries = "GHA")
#' # Missing data for GHA has been picked up from ZAF.
#' completed %>%
#'   dplyr::filter(Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
assemble_fu_allocation_tables <- function(incomplete_allocation_tables,
                                          exemplar_lists,
                                          specified_iea_data,
                                          countries,
                                          country = IEATools::iea_cols$country,
                                          year = IEATools::iea_cols$year,
                                          exemplars = SEAPSUTWorkflow::exemplar_names$exemplars,
                                          exemplar_tables = SEAPSUTWorkflow::exemplar_names$exemplar_tables,
                                          iea_data = SEAPSUTWorkflow::exemplar_names$iea_data,
                                          incomplete_alloc_tables = SEAPSUTWorkflow::exemplar_names$incomplete_alloc_table,
                                          complete_alloc_tables = SEAPSUTWorkflow::exemplar_names$complete_alloc_table) {

  # The incomplete tables are easier to deal with when they are tidy.
  tidy_incomplete_allocation_tables <- IEATools::tidy_fu_allocation_table(incomplete_allocation_tables)

  completed_tables_by_year <- lapply(countries, FUN = function(coun) {
    coun_exemplar_strings <- exemplar_lists %>%
      dplyr::filter(.data[[country]] == coun)

    # For each combination of Country and Year (the rows of coun_exemplar_strings),
    # assemble a list of country allocation tables.
    coun_exemplar_strings_and_tables <- coun_exemplar_strings %>%
      dplyr::mutate(
        # Create a list column containing lists of exemplar tables
        # corresponding to the countries in the Exemplars column.
        "{exemplar_tables}" := Map(get_one_exemplar_table_list,
                                   # Need to wrap this in a list so the WHOLE table is sent via Map to get_one_exemplar_table_list
                                   tidy_incomplete_tables = list(tidy_incomplete_allocation_tables),
                                   exemplar_strings = .data[[exemplars]],
                                   yr = .data[[year]],
                                   country_colname = country,
                                   year_colname = year),
        # Add a column containing an IEA data frame for the country and year of each row
        "{iea_data}" := Map(get_one_df_by_coun_and_yr,
                            .df = list(specified_iea_data),
                            coun = .data[[country]],
                            yr = .data[[year]],
                            country_colname = country,
                            year_colname = year),
        # Add a column containing incomplete fu allocation tables for each row (i.e., for each combination of country and year).
        "{incomplete_alloc_tables}" := Map(get_one_df_by_coun_and_yr,
                                           .df = list(tidy_incomplete_allocation_tables),
                                           coun = .data[[country]],
                                           yr = .data[[year]],
                                           country_colname = country,
                                           year_colname = year),
        # Add a column containing completed fu allocation tables for each row (i.e., for each combination of country and year).
        # Note that the data frames in this column contain the SOURCE of information for each allocation.
        "{complete_alloc_tables}" := Map(IEATools::complete_fu_allocation_table,
                                         fu_allocation_table = .data[[incomplete_alloc_tables]],
                                         exemplar_fu_allocation_tables = .data[[exemplar_tables]],
                                         tidy_specified_iea_data = .data[[iea_data]])
      )
  }) %>%
    dplyr::bind_rows()

  # The only information we need to return is the completed allocation tables.
  # Expand (unnest) only the completed allocation table column to give one data frame of all the FU allocations
  # for all years and all countries.
  completed_tables_by_year %>%
    dplyr::select(complete_alloc_tables) %>%
    tidyr::unnest(cols = .data[[complete_alloc_tables]])
}


#' Assemble completed final-to-useful efficiency tables
#'
#' @param incomplete_eta_fu_tables
#' @param exemplar_lists
#' @param completed_fu_allocation_tables
#' @param countries
#'
#' @return
#'
#' @export
#'
#' @examples
assemble_eta_fu_tables <- function(incomplete_eta_fu_tables,
                                   exemplar_lists,
                                   completed_fu_allocation_tables,
                                   countries,
                                   which_quantity = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u),
                                   country = IEATools::iea_cols$country,
                                   year = IEATools::iea_cols$year,
                                   exemplars = SEAPSUTWorkflow::exemplar_names$exemplars,
                                   exemplar_tables = SEAPSUTWorkflow::exemplar_names$exemplar_tables,
                                   alloc_data = SEAPSUTWorkflow::exemplar_names$alloc_data,
                                   incomplete_eta_tables = SEAPSUTWorkflow::exemplar_names$incomplete_eta_table,
                                   complete_eta_tables = SEAPSUTWorkflow::exemplar_names$complete_eta_table) {

  which_quantity <- match.arg(which_quantity, several.ok = TRUE)

  # The FU allocation tables and the incomplete efficiency tables are easier to deal with when they are tidy.
  tidy_incomplete_eta_fu_tables <- IEATools::tidy_eta_fu_table(incomplete_eta_fu_tables)
  tidy_allocation_tables <- IEATools::tidy_fu_allocation_table(completed_fu_allocation_tables)

  completed_tables_by_year <- lapply(countries, FUN = function(coun) {
    coun_exemplar_strings <- exemplar_lists %>%
      dplyr::filter(.data[[country]] == coun)

    # For each combination of Country and Year (the rows of coun_exemplar_strings),
    # assemble a list of country allocation tables.
    coun_exemplar_strings_and_tables <- coun_exemplar_strings %>%
      dplyr::mutate(
        # Create a list column containing lists of exemplar tables
        # corresponding to the countries in the Exemplars column.
        "{exemplar_tables}" := Map(get_one_exemplar_table_list,
                                   # Need to wrap this in a list so the WHOLE table is sent via Map to get_one_exemplar_table_list
                                   tidy_incomplete_tables = list(tidy_incomplete_eta_fu_tables),
                                   exemplar_strings = .data[[exemplars]],
                                   yr = .data[[year]],
                                   country_colname = country,
                                   year_colname = year),
        # Add a column containing an FU allocation data frame for the country and year of each row
        "{alloc_data}" := Map(get_one_df_by_coun_and_yr,
                              .df = list(tidy_allocation_tables),
                              coun = .data[[country]],
                              yr = .data[[year]],
                              country_colname = country,
                              year_colname = year),
        # Add a column containing incomplete fu eta tables for each row (i.e., for each combination of country and year).
        "{incomplete_eta_tables}" := Map(get_one_df_by_coun_and_yr,
                                         .df = list(tidy_incomplete_eta_fu_tables),
                                         coun = .data[[country]],
                                         yr = .data[[year]],
                                         country_colname = country,
                                         year_colname = year),
        # Add a column containing completed fu allocation tables for each row (i.e., for each combination of country and year).
        # Note that the data frames in this column contain the SOURCE of information for each allocation.
        "{complete_eta_tables}" := Map(IEATools::complete_eta_fu_table,
                                       eta_fu_table = .data[[incomplete_eta_tables]],
                                       exemplar_eta_fu_tables = .data[[exemplar_tables]],
                                       tidy_fu_allocation_table = .data[[alloc_data]],
                                       which_quantity = which_quantity)


      )
  }) %>%
    dplyr::bind_rows()

  # The only information we need to return is the completed allocation tables.
  # Expand (unnest) only the completed allocation table column to give one data frame of all the FU allocations
  # for all years and all countries.
  completed_tables_by_year %>%
    dplyr::select(complete_eta_tables) %>%
    tidyr::unnest(cols = .data[[complete_eta_tables]])
}


get_one_exemplar_table_list <- function(tidy_incomplete_tables,
                                        exemplar_strings, yr, country_colname, year_colname) {
  lapply(exemplar_strings, function(exemplar_coun) {
    tidy_incomplete_tables %>%
      dplyr::filter(.data[[country_colname]] == exemplar_coun, .data[[year_colname]] == yr)
  })
}


get_one_df_by_coun_and_yr <- function(.df, coun, yr, country_colname, year_colname) {
  .df %>%
    dplyr::filter(.data[[country_colname]] %in% coun, .data[[year_colname]] %in% yr)
}


