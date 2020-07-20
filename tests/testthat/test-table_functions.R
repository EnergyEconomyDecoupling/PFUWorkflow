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
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$CompletedAllocationTables,
                                                        setup_exemplars = TRUE)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Verify that the incomplete fu allocation tables look as we expect.
    incomplete_alloc_tables <- readd(target = SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                                     path = testing_setup$cache_path,
                                     character_only = TRUE)
    expect_true(all(!is.na(incomplete_alloc_tables[[IEATools::iea_cols$method]])))
    expect_true(all(!is.na(incomplete_alloc_tables[[IEATools::iea_cols$energy_type]])))
    expect_true(all(!is.na(incomplete_alloc_tables[[IEATools::iea_cols$flow_aggregation_point]])))

    # Check that Wood cookstoves do NOT exist in Residential for GHA.
    # These will be supplied by an exemplar (World).
    readd_by_country(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                     country = "GHA",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$destination]] == "Residential") %>%
      nrow() %>%
      expect_equal(0)
    # Check that efficiencies for Wood cookstoves do NOT exist for GHA.
    # These will be supplied by an exemplar (World)
    readd_by_country(SEAPSUTWorkflow::target_names$IncompleteEfficiencyTables,
                     country = "GHA",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$eu_product]] == "MTH.100.C") %>%
      nrow() %>%
      expect_equal(0)

    # Check that World DOES have the allocation and efficiencies for Wood cookstoves.
    # Allocations:
    readd_by_country(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                     country = "World",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$destination]] == "Residential") %>%
      nrow() %>%
      expect_equal(1)
    # Efficiencies:
    readd_by_country(SEAPSUTWorkflow::target_names$IncompleteEfficiencyTables,
                     country = "World",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$eu_product]] == "MTH.100.C",
                    .data[[IEATools::template_cols$quantity]] == "eta.fu") %>%
      nrow() %>%
      expect_equal(1)

    # Check the completed FU Allocation tables.
    GHA_allocations_completed <- readd_by_country(SEAPSUTWorkflow::target_names$CompletedAllocationTables,
                                                  country = "GHA",
                                                  cache_path = testing_setup$cache_path)
    # GHA_allocations_completed %>%
    #   dplyr::filter

    },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
