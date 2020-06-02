#' Create a drake plan for societal exergy analysis
#'
#' Creates a drake workflow for societal exergy analysis.
#' The caller specifies location of IEA data,
#' which countries should be analyzed, and
#' which the maximum year to ba analyzed.
#'
#' @param iea_data_path The path to IEA data in .csv format
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
  p <- drake::drake_plan(
    # Use !! for tidy evaluation
    iea_data_path = !!iea_data_path,
    # This next statement doesn't work for a vector.
    countries = !!countries,
    # countries = c("GHA", "ZAF"),
    max_year = !!max_year,
    AllIEAData = iea_data_path %>% IEATools::load_tidy_iea_df(),
    IEAData = target(extract_country_data(AllIEAData, countries, max_year), dynamic = map(countries))
  )
  return(p)
}
