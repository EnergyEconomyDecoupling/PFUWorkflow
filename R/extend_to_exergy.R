#' Gives a file path to a sample phi constants table
#'
#' @return A path to a sample phi constants table bundled with this package.
#'
#' @export
#'
#' @examples
#' sample_phi_constants_path()
sample_phi_constants_path <- function() {
  file.path("extdata", "phi_constants.xlsx") %>%
    system.file(package = "SEAPSUTWorkflow")
}


#' Read a table of constant phi (exergy-to-energy ratio) values from a file.
#'
#' @param phi_constants_table_path The path to the Excel file containing a table of constant phi values.
#'                                 Default is the value of `sample_phi_constants_path()`.
#' @param phi_constants_tab_name The
#'
#' @return
#' @export
#'
#' @examples
#' load_phi_constants_table()
load_phi_constants_table <- function(phi_constants_table_path = sample_phi_constants_path(),
                                     phi_constants_tab_name = SEAPSUTWorkflow::phi_constants_names$phi_constants_tab_name,
                                     product_colname = SEAPSUTWorkflow::phi_constants_names$product_colname,
                                     phi_colname = SEAPSUTWorkflow::phi_constants_names$phi_colname) {
  readxl::read_excel(path = phi_constants_table_path, sheet = phi_constants_tab_name) %>%
    dplyr::select(product_colname, phi_colname)
}



#' Assemble completed phi (exergy-to-energy ratio) tables
#'
#' This function is used in the drake workflow to assemble completed phi (exergy-to-energy ratio) tables
#' given a set of phi tables read from machine data files and a phi constants table.
#'
#'
#' @param incomplete_phi_table A data frame of phi values read from machine efficiency data files.
#' @param phi_constants_table A data frame of constant phi values with reasonable default values for all energy carriers.
#'
#' @return A data frame of phi values for every country, year, machine, destination, etc.
#'
#' @export
#'
#' @examples
assemble_phi_table <- function(incomplete_phi_table, phi_constants_table) {

}
