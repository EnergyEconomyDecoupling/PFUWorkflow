#' Create a drake plan for societal exergy analysis
#'
#' Creates a drake workflow for societal exergy analysis.
#' The caller specifies location of IEA data,
#' which countries should be analyzed, and
#' which the maximum year to be analyzed.
#'
#' The return value is a `drake` plan object with the following objects:
#'
#' * `iea_data_path`: the path to IEA extended energy balance data, supplied in the `iea_data_path` argument.
#' * `countries`: the countries to be analyzed, supplied in the `countries` argument.
#' * `max_year`: the maximum year to be analyzed, supplied in the `max_year` argument.
#' * `AllIEAData`: a data frame with all IEA extended energy balance data read from `iea_data_path`.
#' * `IEAData`: a version of the `AllIEAData` data frame containing data for only those countries specified in `countries`.
#' * `balanced_before`: a boolean that tells where the data were balanced as received, usually a vector of `FALSE`, one for each country.
#' * `BalancedIEAData`: a data frame containing balanced IEA extended energy balance data.
#' * `balanced_after`: a boolean telling whether IEA extended energy balance data is balanced after balancing, usually a vector of `TRUE`, one for each country.
#' * `OKToProceed`: `NULL` means everything is balanced and proceeding is OK.
#'
#' Callers can execute the plan by calling `drake::make(plan)`.
#' Results can be recovered with
#' `drake::readd(target = iea_data_path)` or similar.
#'
#' @param iea_data_path The path to IEA extended energy balance data in .csv format
#' @param countries A vector of country abbreviations to be analyzed, such as "c('GHA', 'ZAF')".
#' @param max_year The last year to be studied, typically the last year for which data are available.
#'
#' @return a drake plan object
#'
#' @export
#'
#' @examples
#' get_plan(iea_data_path = "mypath", countries = c("GHA", "ZAF"), max_year = 1999)
get_plan <- function(iea_data_path, countries, max_year) {

  # Get around some warnings.
  map <- NULL
  AllIEAData <- NULL
  IEAData <- NULL
  BalancedIEAData <- NULL
  balanced_after <- NULL

  # Eliminate wornings about
  p <- drake::drake_plan(

    # (1) Grab all the IEA data for ALL countries

    # Use !! for tidy evaluation.
    # See https://stackoverflow.com/questions/62140991/how-to-create-a-plan-in-a-function
    iea_data_path = !!iea_data_path,
    # Need to enclose !!countries in c(), else it doesn't work when countries has length > 1.
    countries = c(!!countries),
    max_year = !!max_year,
    AllIEAData = iea_data_path %>%
      IEATools::load_tidy_iea_df(),
    IEAData = drake::target(extract_country_data(AllIEAData, countries, max_year), dynamic = map(countries)),

    # (2) Balance all the energy data.
    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    balanced_before = drake::target(is_balanced(IEAData, countries), dynamic = map(countries)),
    # Balance all of the data by product and year.
    BalancedIEAData = drake::target(make_balanced(IEAData, countries), dynamic = map(countries)),
    # Check that everything is balanced after balancing.
    balanced_after = drake::target(is_balanced(BalancedIEAData, countries), dynamic = map(countries)),
    # Don't continue if there is a problem.
    OKToProceed = stopifnot(all(balanced_after)),

    # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.
    Specified = target(specify(BalancedIEAData, countries), dynamic = map(countries)),

    # (4) Arrange all the data into PSUT matrices with final stage data.
    PSUT_final = target(make_psut(Specified, countries), dynamic = map(countries))


  )
  return(p)
}
