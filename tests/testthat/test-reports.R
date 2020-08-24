###########################################################
context("Report Functions")
###########################################################

test_that("reports_paths() works as expected", {

  # Get the reports from the default reports directory that was installed
  files <- list.files(system.file("reports", package = "SEAPSUTWorkflow"), recursive = TRUE, full.names = TRUE)
  # Find which ones end in .Rmd or .Rnw
  which_files_are_Rnw <- which(endsWith(files, ".Rnw"), arr.ind = TRUE)
  which_files_are_Rmd <- which(endsWith(files, ".Rmd"), arr.ind = TRUE)
  report_indices <- append(which_files_are_Rmd, which_files_are_Rnw) %>%
    unlist()
  reports <- files[[report_indices]]

  paths <- reports_paths()

  expect_equal(length(setdiff(paths, reports)), 0)

})
