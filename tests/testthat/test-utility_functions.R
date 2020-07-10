###########################################################
context("Utility Functions")
###########################################################

test_that("dir_create_pipe() works as expected", {
  td <- tempdir()
  path <- dir_create_pipe(td, recursive = TRUE)
  expect_equal(path, td)
  expect_true(dir.exists(td))
  unlink(td, force = TRUE, recursive = TRUE)
  expect_false(dir.exists(td))

  # Now try to create a directory that already exists.
  # The second attempt should produce a warning that is converted to an error.
  path <- dir_create_pipe(td, recursive = TRUE)
  expect_true(dir.exists(path))
  expect_warning(dir_create_pipe(td, recursive = TRUE))
  unlink(td, force = TRUE, recursive = TRUE)
})
