###########################################################
context("Report Functions")
###########################################################

test_that("report_source_paths() works as expected", {

  # Get the reports from the default reports directory that was installed
  files <- list.files(system.file("reports", package = "SEAPSUTWorkflow"), recursive = TRUE, full.names = TRUE)
  # Find which ones end in .Rmd or .Rnw
  which_files_are_Rnw <- which(endsWith(files, ".Rnw"), arr.ind = TRUE)
  which_files_are_Rmd <- which(endsWith(files, ".Rmd"), arr.ind = TRUE)
  report_indices <- append(which_files_are_Rmd, which_files_are_Rnw) %>%
    unlist()
  reports <- files[report_indices]

  paths <- report_source_paths()

  expect_equal(length(setdiff(paths, reports)), 0)


})


test_that("report_dest_paths() works as expected", {
  rsps <- report_source_paths()
  report_dest_paths(report_source_files = rsps, report_dest_folder = "dest_folder")
})





# tryCatch({
#
#   report_folder <- tempdir(check = TRUE)
#   # Now try to generate reports.
#   expect_true(generate_reports(report_source_files = paths, report_dest_folder = report_folder))
#
# },
# finally = {
#   unlink(report_folder)
# })
