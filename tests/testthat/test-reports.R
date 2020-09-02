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

  expect_equal(length(reports), 3)
  expect_equal(length(setdiff(paths, reports)), 0)


})


test_that("report_dest_paths() works as expected", {
  rsps <- report_source_paths()
  out_paths <- report_dest_paths(report_source_files = rsps, report_dest_folder = "dest_folder")
  # Verify that all paths start with dest_folder
  expect_true(all(startsWith(out_paths, "dest_folder")))
  # Verify that all paths end in .pdf
  expect_true(all(endsWith(out_paths, ".pdf")))
  # Verify that all corresponding base names are same
  rsps_base <- basename(rsps) %>%
    tools::file_path_sans_ext()
  out_paths_base <- basename(out_paths) %>%
    tools::file_path_sans_ext()
  expect_equal(out_paths_base, rsps_base)
})


test_that("generate_reports() works as intended", {

  # This is a bogus test for the time being.
  # Replace with something better when the code is written.
  expect_true(generate_reports())
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
