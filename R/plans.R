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
    ieadp = iea_data_path,
    ctrys = countries,
    maxyr = max_year,
    AllIEAData = ieadp %>% IEATools::load_tidy_iea_df(),
    IEAData = target(extract_country_data(AllIEAData, ctrys, maxyr), dynamic = map(ctrys))
  )
  # Delete this code when I figure out how to put the correct items in the plan in the first place.
  # The variables do not get evaluated. So let's replace the unevaluated items.
  p[[1, "command"]] <- list(iea_data_path)
  p[[2, "command"]] <- list(countries)
  p[[3, "command"]] <- list(max_year)
  return(p)
}
