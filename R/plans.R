#' Create a drake plan for societal exergy analysis
#'
#' Creates a drake workflow for societal exergy analysis.
#' The caller specifies location of IEA data,
#' which countries should be analyzed, and
#' which the maximum year to be analyzed.
#'
#' The return value is a `drake` plan object with the following targets:
#'
#' * `iea_data_path`: The path to IEA extended energy balance data, supplied in the `iea_data_path` argument.
#' * `countries`: The countries to be analyzed, supplied in the `countries` argument.
#' * `max_year`: The maximum year to be analyzed, supplied in the `max_year` argument.
#' * `AllIEAData`: A data frame with all IEA extended energy balance data read from `iea_data_path`.
#' * `IEAData`: A version of the `AllIEAData` data frame containing data for only those countries specified in `countries`.
#' * `balanced_before`: A boolean that tells where the data were balanced as received, usually a vector of `FALSE`, one for each country.
#' * `BalancedIEAData`: A data frame containing balanced IEA extended energy balance data.
#' * `balanced_after`: A boolean telling whether IEA extended energy balance data is balanced after balancing, usually a vector of `TRUE`, one for each country.
#' * `OKToProceed`: `NULL` means everything is balanced and proceeding is OK.
#' * `Specified`: A data frame with specified industries. See `IEATools::specify_all()`.
#' * `PSUT_final`: A data frame containing PSUT matrices up to the final stage.
#'
#' Callers can execute the plan by calling `drake::make(plan)`.
#' Results can be recovered with
#' `drake::readd(target = iea_data_path)` or similar.
#'
#' Note that some targets can be read using `readd_by_country()`, including:
#'
#' * `AllIEAData`,
#' * `IEAData`,
#' * `BalancedIEAData`,
#' * `Specified`, and
#' * `PSUT_final`.
#'
#' @param countries A vector of country abbreviations to be analyzed, such as "c('GHA', 'ZAF')".
#' @param max_year The last year to be studied, typically the last year for which data are available.
#' @param how_far A string indicating the last target to include in the plan that is returned.
#'                Default is "all_targets" to indicate all targets of the plan should be returned.
#' @param iea_data_path The path to IEA extended energy balance data in .csv format.
#' @param exemplar_table_path The path to an exemplar table.
#' @param fu_analysis_folder The path to a folder containing final-to-useful analyses.
#'                           Sub-folders named with 3-letter country abbreviations are assumed.
#'
#' @return a drake plan object
#'
#' @export
#'
#' @seealso
#'
#' * [How to create a plan in a function](https://stackoverflow.com/questions/62140991/how-to-create-a-plan-in-a-function)
#' * [Best practices for unit tests on custom functions for a drake workflow](https://stackoverflow.com/questions/61220159/best-practices-for-unit-tests-on-custom-functions-for-a-drake-workflow)
#' * [drakepkg](https://github.com/tiernanmartin/drakepkg)
#' * [Workflows as R packages](https://books.ropensci.org/drake/projects.html#workflows-as-r-packages)
#'
#' @examples
#' \dontrun{
#'   td <- tempdir()
#'   get_plan(countries = c("GHA", "ZAF"),
#'            max_year = 1999,
#'            iea_data_path = "mypath",
#'            exemplar_table_path = "exemplarpath",
#'            fu_analysis_folder = td)
#'   unlink(td)
#' }
get_plan <- function(countries, max_year, how_far = "all_targets",
                     iea_data_path, exemplar_table_path, fu_analysis_folder) {

  # Get around some warnings.
  map <- NULL
  AllIEAData <- NULL
  IEAData <- NULL
  BalancedIEAData <- NULL
  balanced_after <- NULL
  Specified <- NULL
  PSUT_final <- NULL

  p <- drake::drake_plan(

    # (0) Set many arguments to be objects in the drake cache for later use

    # Use !!, for tidy evaluation, to put the arguments' values in the plan..
    # See https://stackoverflow.com/questions/62140991/how-to-create-a-plan-in-a-function
    # Need to enclose !!countries in c() (or an identity function), else it doesn't work when countries has length > 1.
    countries = c(!!countries),
    max_year = !!max_year,
    iea_data_path = !!iea_data_path,
    exemplar_table_path = !!exemplar_table_path,
    fu_analysis_folder = !!fu_analysis_folder,

    # (1) Grab all the IEA data for ALL countries

    AllIEAData = iea_data_path %>% IEATools::load_tidy_iea_df(),
    IEAData = drake::target(AllIEAData %>%
                              extract_country_data(countries, max_year),
                            dynamic = map(countries)),

    # (2) Balance all the final energy data.

    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    balanced_before = drake::target(IEAData %>%
                                      is_balanced(countries),
                                    dynamic = map(countries)),
    # Balance all of the data by product and year.
    BalancedIEAData = drake::target(IEAData %>%
                                      make_balanced(countries),
                                    dynamic = map(countries)),
    # Check that everything is balanced after balancing.
    balanced_after = drake::target(BalancedIEAData %>%
                                     is_balanced(countries),
                                   dynamic = map(countries)),
    # Don't continue if there is a problem.
    # stopifnot returns NULL if everything is OK.
    OKToProceed = ifelse(is.null(stopifnot(all(balanced_after))), yes = TRUE, no = FALSE),

    # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.

    Specified = drake::target(BalancedIEAData %>%
                                specify(countries),
                              dynamic = map(countries)),

    # (4) Arrange all the data into PSUT matrices with final stage data.

    PSUT_final = drake::target(Specified %>%
                                 make_psut(countries),
                               dynamic = map(countries)),

    # (5) Load incomplete FU allocation tables

    IncompleteAllocationTables = drake::target(fu_analysis_folder %>%
                                                 load_fu_allocation_tables(countries),
                                               dynamic = map(countries)),

    # (6) Load incomplete FU efficiency tables

    IncompleteEfficiencyTables = drake::target(fu_analysis_folder %>%
                                                 load_fu_efficiency_tables(countries),
                                               dynamic = map(countries)),

    # (7) Load exemplar table and make lists for each country and year

    ExemplarLists = drake::target(exemplar_table_path %>%
                                    load_exemplar_table(countries) %>%
                                    exemplar_lists(countries),
                                  dynamic = map(countries)),

    # (8) Form lists of exemplar tables, one for each country and year to be analyzed.


    # (9) Complete allocation and efficiency tables


    # (10) Extend to useful stage


    # (11) Add other methods



    # (12) Add exergy quantifications of energy


    # (13) Off to the races!  Do other calculations

  )
  if (how_far != "all_targets") {
    # Find the last row of the plan to keep.
    last_row_to_keep <- p %>%
      tibble::rowid_to_column(var = "rownum") %>%
      dplyr::filter(.data[["target"]] == how_far) %>%
      dplyr::select("rownum") %>%
      unlist() %>%
      unname()
    p <- p %>%
      dplyr::slice(1:last_row_to_keep)
  }
  return(p)
}


