###########################################################
context("Utility Functions")
###########################################################

test_that("readd_by_country works", {
  # Set the path for the drake cache in a temporary directory
  cache_path <- tempfile("drake_cache_for_testing")
  tryCatch({
    # Create a fake drake plan
    temp_cache <- drake::new_cache(path = cache_path)
    plan <- get_plan(iea_data_path = IEATools::sample_iea_data_path(),
                     countries = c("GHA"),
                     max_year = 1999)
    # Make the plan in the temp_cache
    drake::make(plan, cache = temp_cache, verbose = 0)
    # Now readd data from the cache.
    expect_equal(drake::readd(target = iea_data_path, path = cache_path), IEATools::sample_iea_data_path())
    expect_equal(drake::readd(target = countries, path = cache_path), c("GHA", "ZAF"))
    expect_equal(drake::readd(target = max_year, path = cache_path), 1999)
    # Be sure that readd_bycountry also works
    expected_all_iea_data <- IEATools::sample_iea_data_path() %>%
      IEATools::load_tidy_iea_df()
    # Read directly out of the cache.
    expect_equal(drake::readd(target = AllIEAData, path = cache_path), expected_all_iea_data)
    # Read GHA data from the cache
    expected_GHA_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "GHA", Year <= 1999)
    expect_equal(readd_by_country(target = "IEAData", country = "GHA", cache_path = cache_path), expected_GHA_data)
  },
  finally = {
    temp_cache$destroy()
  })

})
