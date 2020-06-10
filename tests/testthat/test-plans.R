###########################################################
context("Plan Functions")
###########################################################

test_that("get_plan works", {
  my_plan <- get_plan(iea_data_path = "mypath",
                      fu_analysis_folder = "FUpath",
                      countries = c("GHA", "ZAF"),
                      max_year = 1999)
  expect_equal(my_plan[[1, "target"]], "iea_data_path")
  expect_equal(my_plan[[2, "target"]], "fu_analysis_folder")
  expect_equal(my_plan[[3, "target"]], "countries")
  expect_equal(my_plan[[4, "target"]], "max_year")
  expect_equal(my_plan[[5, "target"]], "AllIEAData")
  expect_equal(my_plan[[6, "target"]], "IEAData")

  # Make sure the pieces get created correctly with tidyeval.
  expect_equal(my_plan[[1, "command"]], list("mypath"))
  expect_equal(my_plan[[4, "command"]], list(1999))
})


test_that("keeping only some rows of a plan works", {
  full_plan <- get_plan(iea_data_path = "mypath",
                        fu_analysis_folder = "FUpath",
                        countries = c("GHA", "ZAF"),
                        max_year = 1999)
  my_plan <- get_plan(iea_data_path = "mypath",
                      fu_analysis_folder = "FUpath",
                      countries = c("GHA", "ZAF"),
                      max_year = 1999,
                      how_far = "Specified")

  expect_lt(nrow(my_plan), nrow(full_plan))

  my_plan[nrow(my_plan), ] %>%
    dplyr::select(target) %>%
    unlist() %>%
    unname() %>%
    expect_equal("Specified")
})


test_that("make works", {
  cache_path <- tempfile("drake_cache_for_testing")
  fu_folder <- tempdir("FU_folder_for_testing")
  plan <- get_plan(iea_data_path = IEATools::sample_iea_data_path(),
                   fu_analysis_folder = fu_folder,
                   countries = c("GHA", "ZAF"),
                   max_year = 2000)
  tryCatch({
    # Create a fake drake plan
    temp_cache <- drake::new_cache(path = cache_path)

    # Make the plan in the temp_cache
    drake::make(plan, cache = temp_cache, verbose = 0)

    # Now readd data from the cache.
    expect_equal(drake::readd(target = iea_data_path, path = cache_path), IEATools::sample_iea_data_path())
    expect_equal(drake::readd(target = countries, path = cache_path), c("GHA", "ZAF"))
    expect_equal(drake::readd(target = max_year, path = cache_path), 2000)

    # Be sure that readd_by_country also works
    expected_all_iea_data <- IEATools::sample_iea_data_path() %>%
      IEATools::load_tidy_iea_df()
    # Read directly out of the cache.
    expect_equal(drake::readd(target = AllIEAData, path = cache_path), expected_all_iea_data)
    # Read GHA data from the cache
    expected_GHA_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "GHA")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "GHA", cache_path = cache_path), expected_GHA_data)
    # Read ZAF data from the cache
    expected_ZAF_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "ZAF")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "ZAF", cache_path = cache_path), expected_ZAF_data)

    # Check energy balance information
    expect_equal(drake::readd(balanced_before, path = cache_path), c(FALSE, FALSE))
    expect_equal(drake::readd(balanced_after, path = cache_path), c(TRUE, TRUE))
    expect_null(drake::readd(OKToProceed, path = cache_path))

    # Check specification process, reading by country.
    expect_true(!is.null(readd_by_country(target = "Specified", country = "GHA", cache_path = cache_path)))

    # Check making PSUT matrices, reading by country.
    expect_true(!is.null(readd_by_country(target = "PSUT_final", country = "ZAF", cache_path = cache_path)))
  },
  finally = {
    # Clean up the cache.
    temp_cache$destroy()
    # Delete the folder
    unlink(fu_folder, recursive = TRUE, force = TRUE)
  })
})
