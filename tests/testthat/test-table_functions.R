test_that("load_fu_allocation_tables() works for a non-existent country", {

  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "fu_analysis_folder")

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
    # Try to read a country for which no allocations or efficiencies exist.
    # (Note that the test setup has only "GHA" and "ZAF" countries, so "GRC" should return NULL.)
    expect_null(load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                          countries = "GRC"))
    expect_null(load_fu_efficiency_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                          countries = "GRC"))

    # Try when we ask for one country that DOES exist and one that doesn't exist.
    # We should NOT get NULL here.
    expect_true(!is.null(load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                                   countries = c("GRC", "GHA"))))
    expect_true(!is.null(load_fu_efficiency_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                                   countries = c("GRC", "GHA"))))

    # Try when we ask for two countries that exist. Should get one big data frame.
    result_alloc <- load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                              countries = c("GHA", "ZAF"))
    expect_equal(result_alloc[[IEATools::iea_cols$country]] %>% unique(), c("GHA", "ZAF"))
    result_eff <- load_fu_efficiency_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                            countries = c("GHA", "ZAF"))
    expect_equal(result_eff[[IEATools::iea_cols$country]] %>% unique(), c("GHA", "ZAF"))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})


test_that("assemble_fu_allocation_tables() works as expected.", {
  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$CompletedAllocationTables)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Build some completed FU Allocation tables.

    },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
