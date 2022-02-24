


#' Aggregate by product
#'
#' This function aggregates PSUT matrices by energy carrier (product)
#' according to an aggregation map.
#' Aggregated matrices replace original matrices on output.
#'
#' @param .psut_df A data frame with PSUT matrices in columns.
#' @param matrices_to_aggregate A list of the matrices to aggregate according to `aggregation_map`.
#'                              Default is `c("R", "U", "V", "Y")`.
#' @param aggregation_map A description of the aggregation to be performed.
#'                        See `matsbyname::aggregate_byname()` for a description of its format.
#'                        Default is `PFUWorkflow::product_aggregation_map`.
#' @param countries The countries to be analyzed.
#' @param country The name of the country column in `.psut_df`.
#'
#' @return A data frame of aggregated products.
#'
#' @export
aggregate_products <- function(.psut_df,
                               matrices_to_aggregate = c("R", "U", "V", "Y"),
                               aggregation_map = PFUWorkflow::product_aggregation_map,
                               countries,
                               country = IEATools::iea_cols$country) {

  out <- .psut_df %>%
    dplyr::filter(.data[[country]] %in% countries)


  for (mat_name in matrices_to_aggregate) {
    out <- out %>%
      dplyr::mutate(
        "{mat_name}" := .data[[mat_name]] %>%
          matsbyname::aggregate_byname(aggregation_map = list(aggregation_map), pattern_type = "leading")
      )
  }
  return(out)
}



