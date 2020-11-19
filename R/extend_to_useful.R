#' Add allocation matrices to a data frame
#'
#' This function adds allocation (`C`) matrices to the previously-created
#' `CompletedAllocationTables` target.
#'
#' @param psut_final a data frame containing PSUT matrices with final energy as the last stage.
#' @param completed_allocation_tables the completed allocation tables from which allocation (`C`) matrices should be created.
#' @param countries the countries for which C matrices should be formed
#' @param country,year See `IEATools::ieacols`.
#' @param .values,c_source,C_Y,C_EIOU See `IEATools::template_cols`.
#'
#' @return A data frame with `C_Y` and `C_EIOU` columns containing allocation matrices.
#'
#' @export
#'
#' @examples
add_C_mats <- function(psut_final,
                       completed_allocation_tables,
                       countries,
                       country = IEATools::iea_cols$country,
                       year = IEATools::iea_cols$year,
                       .values = IEATools::template_cols$.values,
                       c_source = IEATools::template_cols$c_source,
                       C_Y = IEATools::template_cols$C_Y,
                       C_EIOU  = IEATools::template_cols$C_eiou) {
  tables <- completed_allocation_tables %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # Eliminate the c_source column (if it exists) before sending
      # the completed_allocation_tables into form_C_mats().
      # The c_source column applies to individual C values, and we're making matrices out of them.
      # In other words, form_C_mats() doesn't know what to do with that column.
      "{c_source}" := NULL
    )
  # Need to form C matrices from completed_allocation_tables.
  # Use the IEATools::form_C_mats() function for this
  C_mats <- IEATools::form_C_mats(tables, matvals = .values)
  meta_cols <- C_mats %>%
    IEATools::meta_cols(years_to_keep = year,
                        not_meta = c(C_Y, C_EIOU),
                        return_names = TRUE)

  psut_final %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::left_join(C_mats, by = meta_cols)
}


add_eta_fu_phi_u_vecs <- function(with_C_mats,
                                  completed_efficiency_tables,
                                  countries) {

}
