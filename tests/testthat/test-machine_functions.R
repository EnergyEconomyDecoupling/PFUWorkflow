###########################################################
context("Machine Data Tables")
###########################################################

# Tests that the path to tge example machine file is correct, and the sample
# excel workbook is present
test_that("sample_machine_workbook_path() works correctly", {
  sample_path <- sample_machine_workbook_path()
  expect_true(endsWith(sample_path, file.path("extdata",
                                              "Machine Examples")))
})


test_that("get_eta_filepaths() works correctly", {

  # Creates path to folder containing the Machine Examples Folder
  sample_path <- sample_machine_workbook_path()

  # Finds machine .xlsx files which contain a FIN_ETA sheet
  all_files <- get_eta_filepaths(filepath = sample_path) %>%
    unlist()

  # Tests that the list of filepaths returned contains only "Machine_Example.xlsx"
  # as it is the only excel file with a "FIN_ETA" sheet.
  expect_true(endsWith(all_files, file.path("extdata",
                                            "Machine Examples",
                                            "Machine_Example",
                                            "Machine_Example.xlsx")))

})


test_that("read_all_eta_files() works correctly", {

  # Establishes the path to the Machine_Example.xlsx
  sample_path <- sample_machine_workbook_path()

  # Reads data from the Machine_Example.slsx file
  etas <- read_all_eta_files(eta_fin_paths = sample_path)

  # Tests whether the dataframe created by calling read-all_eta_files exists
  expect_true(!is.null(etas))

  # Gets column names
  cnames <- etas %>%
    colnames()

  # Tests whether the column names are as expected
  expect_equal(cnames, c("Country", "Energy.type", "Last.stage", "Method",
                         "Machine", "Eu.product", "Quantity", "Year", ".values"))


  # Tests that there are 108 observations
  expect_equal(length(etas$Country), 108)

})


test_that("read_all_eta_files() works with sample machine efficiency data", {

  # Establishes the path to the folder containing individual machine data files
  # associated with the IEATools sample data for GHA and ZAF
  eta_fin_sample_path <- system.file("extdata", "Machines - Data",
                                     package = "SEAPSUTWorkflow")

  expect_true(file.exists(eta_fin_sample_path))

  # Reads data from the machine files
  etas <- read_all_eta_files(eta_fin_paths = eta_fin_sample_path)

  # Tests whether the dataframe created by calling read_all_eta_files exists
  expect_true(!is.null(etas))

  # Gets column names
  cnames <- etas %>%
    colnames()

  # Tests whether the column names are as expected
  expect_equal(cnames, c("Country", "Energy.type", "Last.stage", "Method",
                         "Machine", "Eu.product", "Quantity", "Year", ".values"))

})



