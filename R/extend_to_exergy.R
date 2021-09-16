


#' Create a data frame of phi_pf vectors
#'
#' This function creates a data frame that contains all the metadata columns
#' from `phi_u_vecs` and a column of phi_pf vectors.
#' This work is accomplished by creating a vector from `phi_constants`,
#' adding one instance of the vector to the right side of the `phi_constants` data frame
#' for each row of the data frame,
#' and deleting `phi_u_colname` from the data frame.
#'
#' @param phi_constants A data frame of constant phi values (for primary, final, and useful stages)
#'                      with columns `product`, `phi_colname`, and `is_useful_colname`.
#' @param phi_u_vecs A data frame containing metadata columns and a column of phi_u vectors.
#'                   A column of phi_pf vectors replaces the column of phi_u vectors on output.
#' @param country,product See `IEATools::iea_cols`.
#' @param eta_fu,phi_u,phi_pf_colname See `IEATools::template_cols`.
#' @param phi_colname,is_useful_colname See `IEATools::phi_constants_colnames`.
#'
#' @return A version of the `phi_constants` data frame
#'         with the column of useful phi (useful exergy-to-energy ratio) vectors
#'         replaced by a column of primary and final phi vectors.
#' @export
#'
#' @examples
#' phi_constants <- IEATools::sample_phi_constants_path() %>%
#'   IEATools::load_phi_constants_table()
#' phi_u_vecs <- tibble::tibble(Country = "GHA",
#'                              Year = 1971,
#'                              rownames = c("Light", "MD"),
#'                              colnames = "col",
#'                              matnames = "phi.u",
#'                              matvals = c(0.8, 0.9),
#'                              rowtypes = "rowtype",
#'                              coltypes = "coltype") %>%
#'   dplyr::group_by(Country, Year) %>%
#'   matsindf::collapse_to_matrices() %>%
#'   dplyr::rename(phi.u = matvals) %>%
#'   dplyr::mutate(
#'     Quantity = NULL
#'   )
#' calc_phi_pf_vecs(phi_constants, phi_u_vecs)
calc_phi_pf_vecs <- function(phi_constants,
                             phi_u_vecs,
                             countries,
                             country = IEATools::iea_cols$country,
                             product = IEATools::iea_cols$product,
                             # quantity = IEATools::template_cols$quantity,
                             eta_fu = IEATools::template_cols$eta_fu,
                             phi_u = IEATools::template_cols$phi_u,
                             phi_pf_colname = IEATools::template_cols$phi_pf,
                             phi_colname = IEATools::phi_constants_names$phi_colname,
                             is_useful_colname = IEATools::phi_constants_names$is_useful_colname) {
  # Pick up non-useful (i.e., primary and final)
  # phi values.
  phi_pf_constants <- phi_constants %>%
    dplyr::filter(! .data[[is_useful_colname]])
  # Create a vector from phi_constants
  phi_pf_vec <- matrix(phi_pf_constants[[phi_colname]], nrow = nrow(phi_pf_constants), ncol = 1,
                       dimnames = list(c(phi_pf_constants[[product]]), NULL))

  trimmed_phi_u_vecs <- phi_u_vecs %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # We don't need the eta_fu or phi_u column on output.
      "{eta_fu}" := NULL,
      "{phi_u}" := NULL
    )
  nrows_trimmed_phi_u_vecs <- nrow(trimmed_phi_u_vecs)

  trimmed_phi_u_vecs %>%
    dplyr::mutate(
      # Add a column of phi_pf vectors
      "{phi_pf_colname}" := matsbyname::make_list(phi_pf_vec,
                                                  n = nrows_trimmed_phi_u_vecs,
                                                  lenx = 1)
    )
}


#' Sums phi_pf and phi_u vectors
#'
#' This function verifies that there are no rows in common between the
#' two input vectors.
#'
#' @param phi_pf_vecs A data frame of phi_pf vectors
#' @param phi_u_vecs A data frame of phi_u vectors
#' @param countries The countries for which you want to perform this task.
#'
#' @return A data frame of summed phi_pf and phi_u vectors.
#'
#' @export
#'
#' @examples
sum_phi_vecs <- function(phi_pf_vecs,
                         phi_u_vecs,
                         countries,
                         phi_pf = IEATools::template_cols$phi_pf,
                         phi_u = IEATools::template_cols$phi_u,
                         phi_colname = IEATools::phi_constants_names$phi_colname) {
  phi_df <- dplyr::full_join(phi_pf_vecs,
                             phi_u_vecs,
                             by = matsindf::everything_except(phi_pf_vecs, phi_pf) %>% as.character())
  phi_df %>%
    dplyr::mutate(
      "{phi_colname}" := matsbyname::sum_byname(.data[[phi_pf]], .data[[phi_u]]),
      # Delete the columns we no longer need.
      "{phi_pf}" := NULL,
      "{phi_u}" := NULL
    )
}
