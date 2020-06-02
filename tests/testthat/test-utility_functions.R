###########################################################
context("Utility Functions")
###########################################################

test_that("readd_bycountry works", {
  # Set the path for the drake cache in a temporary directory
  cache_path <- tempfile("drake_cache_for_testing")
  # Create a fake drake plan
  tryCatch({
    temp_cache <- drake::new_cache(path = cache_path)
    plan <- get_plan(iea_data_path = IEATools::sample_iea_data_path(),
                     countries = c("GHA"),
                     max_year = 1999)
    # plan <- drake::drake_plan(
    #   countries = c("GHA"),                             # Eliminates ZAF data from the cache.
    #   max_year = 1999,                                  # Eliminates Y2K data from the cache.
    #   AllIEAData = IEATools::sample_iea_data_path() %>% IEATools::load_tidy_iea_df(),
    #   IEAData = target(extract_country_data(AllIEAData, countries, max_year), dynamic = map(countries))
    # )
    # Make the plan in the temp_cache
    drake::make(plan, cache = temp_cache, verbose = 0)
  },
  finally = {
    temp_cache$destroy()
  })

})
