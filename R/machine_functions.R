#' Get all filepaths to machine excel files which contain a FIN_ETA sheet
#'
#' @return A list of the file paths to machine excel files containing
#'         FIN_ETA frontsheets, and therefore usable data.
#' @export
#'
get_eta_filepaths <- function() {

  # Estalishes name of the frontsheet of each machines excel workbook
  eta_sheet <- "FIN_ETA"

  # Gets project path
  project_path <- PFUSetup::get_abs_paths()$project_path

  # Gets path to machine data
  machine_path <- file.path(project_path, "Data", "Machines - Data")

  # Lists path to each machine folder
  machine_paths <- list.dirs(path = machine_path, recursive = FALSE)

  # Lists each machine excel file in machine_paths
  machine_filepaths <- list.files(machine_paths, recursive = FALSE, full.names = TRUE, pattern = ".xlsx") %>%
    as.list()

  # Creates empty list which will contain a list of paths to machine folders
  # which contain "FIN_ETA" sheets
  eta_fin_paths <- list()

  # Loops through all paths to check they have a "FIN_ETA" sheet, and appends
  # to the eta_fin_paths if so
  for(path in machine_filepaths) {
    if(eta_sheet %in% readxl::excel_sheets(path)) {
      eta_fin_paths <- eta_fin_paths %>% append(path)
    } else {
      next
    }
  }
}



#' Create a data frame containing machine Eta.fu and Phi.u values.
#'
#' @param eta_fin_paths A list of the file paths to machine excel files containing
#'                      FIN_ETA frontsheets, and therefore usable data.
#'                      Created by calling the `get_eta_filepaths` function.
#'
#' @return A data frame containing all Eta.fu and Phi.u values present
#'         in all Machine excel files, with the following column names:
#'         "Country", "Machine", "Eu.product", "Metric", "Year", "Value".
#' @export
#'
read_all_eta_files <- function(eta_fin_paths) {

  # Creates empty tibble to store etas data in
  etas <- tibble::as_tibble()

  # Loops through each country in eta_fin_paths to add Eta.fu and Phi.u values
  # to etas tibble
  for(path in eta_fin_paths) {

    # Reads raw data
    raw_etas <- readxl::read_excel(path = path, sheet = "FIN_ETA", skip = 1)

    # Figure out year columns.
    year_columns <- IEATools::year_cols(raw_etas, return_names = TRUE)

    # Pivots year columns into a "Year" column and a "Value" column
    raw_etas <- raw_etas %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(year_columns),
                          names_to = "Year",
                          values_to = "Value")
    # Sets column classes
    raw_etas$Country <- as.character(raw_etas$Country)
    raw_etas$Machine <- as.character(raw_etas$Machine)
    raw_etas$Eu.product <- as.character(raw_etas$Eu.product)
    raw_etas$Metric <- as.character(raw_etas$Metric)
    raw_etas$Year <- as.numeric(raw_etas$Year)
    raw_etas$Value <- as.numeric(raw_etas$Value)

    # Binds values from individual excel FIN_ETA sheet into etas tibble, and
    # filters for only "Eta.fu" and "Phi.u" values
    etas <- etas %>%
      dplyr::bind_rows(raw_etas) %>%
      dplyr::filter(Metric %in% c("Eta.fu", "Phi.u"))

  }

}


