#' Add allocation matrices to a data frame
#'
#' This function adds allocation matrices (`C_Y` and `C_EIOU`) to the previously-created
#' `CompletedAllocationTables` target.
#'
#' @param psut_final a data frame containing PSUT matrices with final energy as the last stage.
#' @param completed_allocation_tables the completed allocation tables from which allocation (`C`) matrices should be created.
#'                                    This data frame is most likely to be the `CompletedAllocationTables` target.
#' @param countries the countries for which C matrices should be formed
#' @param country,year See `IEATools::ieacols`.
#' @param .values,c_source,C_Y,C_EIOU See `IEATools::template_cols`.
#'
#' @return A data frame with `C_Y` and `C_EIOU` columns containing allocation matrices.
#'
#' @export
add_C_mats <- function(completed_allocation_tables,
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
  # Use the IEATools::form_C_mats() function for this task.
  # The function accepts a tidy data frame in addition to wide-by-year data frames.
  C_mats <- IEATools::form_C_mats(tables, matvals = .values)
}


#' Add efficiency (`eta`) and exergy-to-energy ratio (`phi`) vectors
#' to a data frame.
#'
#' This function adds final-to-useful efficiency (`eta`) and
#' exergy-to-energy ratio vectors to the previously-created `WithCmats` target.#'
#'
#' @param with_C_mats
#' @param completed_efficiency_tables
#' @param countries
#'
#' @return A data frame with `eta_fu` and `phi_u` vectors added as columns.
#'
#' @export
add_eta_fu_phi_u_vecs <- function(completed_efficiency_tables,
                                  countries,
                                  country = IEATools::iea_cols$country,
                                  year = IEATools::iea_cols$year,
                                  eta_fu_phi_u_source = IEATools::template_cols$eta_fu_phi_u_source,
                                  .values = IEATools::template_cols$.values,
                                  eta_fu = IEATools::template_cols$eta_fu,
                                  phi_u = IEATools::template_cols$phi_u) {
  tables <- completed_efficiency_tables %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # Eliminate the eta_phi_source column (if it exists) before sending
      # the completed_allocation_tables into form_eta_fu_phi_u_vecs().
      # The eta_fu_phi_u_source column applies to individual eta_fu and phi_u values, and
      # we're making vectors out of them.
      # In other words, form_eta_fu_phi_u_vecs() doesn't know what to do with that column.
      "{eta_fu_phi_u_source}" := NULL
    )
  # Need to form eta_fu and phi_u vectors from completed_efficiency_tables.
  # Use the IEATools::completed_efficiency_tablesform_eta_fu_phi_u_vecs() function for this task.
  # The function accepts a tidy data frame in addition to wide-by-year data frames.
  eta_fu_phi_u_vecs <- IEATools::form_eta_fu_phi_u_vecs(tables, matvals = .values)
}


