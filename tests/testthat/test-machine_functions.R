###########################################################
context("Machine Data Tables")
###########################################################

# Tests that the path to tge example machine file is correct, and the sample
# excel workbook is present
test_that("sample_machine_workbook_path() works correctly", {
  sample_path <- sample_machine_workbook_path()
  expect_true(endsWith(sample_path, file.path("extdata",
                                              "Machine_Example",
                                              "Machine_Example.xlsx")))
})


test_that("get_eta_filepaths() works correctly", {

  # Creates path to folder containing the Machine Example Folder, and two excel sheets:
  # 1 - Machine_Example.xlsx, a correctly formatted file
  # 2 - Machine_Failure.xlsx, an incorrectly formatted file
  sample_folder <- file.path("extdata") %>%
    system.file(package = "SEAPSUTWorkflow")

  all_files <- get_eta_filepaths(filepath = sample_folder) %>%
    unlist()

  # Tests that the list of filepaths returned contains only "Machine_Example.xlsx"
  # as it is the only excel file with a "FIN_ETA" sheet.
  expect_true(endsWith(all_files, file.path("extdata",
                                            "Machine_Example",
                                            "Machine_Example.xlsx")))

})

test_that("read_all_eta_files() works correctly", {

  # Establishes the path to the Machine_Example.xlsx
  eta_fin_sample_path <- sample_machine_workbook_path()

  # Reads data from the Machine_Example.slsx file
  etas <- read_all_eta_files(eta_fin_paths = eta_fin_sample_path)

  # Tests whether the dataframe created by calling read-all_eta_files exists
  expect_true(!is.null(etas))

  # Gets column names
  cnames <- etas %>%
    colnames()

  # Tests whether the column names are as expected
  expect_true("Country" %in% cnames)
  expect_true("Machine" %in% cnames)
  expect_true("Eu.product" %in% cnames)
  expect_true("Metric" %in% cnames)
  expect_true("Year" %in% cnames)
  expect_true("Value" %in% cnames)


})
