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





