
#' Exemplar table names
#'
#' A string list containing named names of columns and tabs for exemplar tables.
#' Items in the list provide default values for column name function arguments
#' throughout the `SEAPSUTWorkflow` package.
#'
#' @format A string list with `r length(exemplar_names)` entries.
#' \describe{
#' \item{exemplar_tab_name}{The string name of the tab in the Excel file containing the exemplar table.}
#' \item{prev_names}{The name of a column of previous names used for the country.}
#' \item{exemplars}{The name of a column of exemplar countries.}
#' \item{exemplar_country}{The name of the exemplar country column.}
#' \item{row_code}{The name of the rest-of-world region code column.}
#' \item{country_name}{The name of the column containing the long name of a country.}
#' \item{world}{The name of the world region.}
#' }
#'
#' @examples
#' exemplar_names
"exemplar_names"


