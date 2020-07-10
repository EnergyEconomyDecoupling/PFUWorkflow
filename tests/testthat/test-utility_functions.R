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


test_that("readd_by_country() works as expected", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "IEAData")

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    GHAdata <- readd_by_country("IEAData", country = "GHA", cache_path = testing_setup$cache_path)
    expect_equal(GHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "GHA")
    ZAFdata <- readd_by_country("IEAData", country = "ZAF", cache_path = testing_setup$cache_path)
    expect_equal(ZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "ZAF")
    GHAZAFdata <- readd_by_country("IEAData", country = c("GHA", "ZAF"), cache_path = testing_setup$cache_path)
    expect_equal(GHAZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
    ZAFGHAdata <- readd_by_country("IEAData", country = c("ZAF", "GHA"), cache_path = testing_setup$cache_path)
    expect_equal(ZAFGHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
