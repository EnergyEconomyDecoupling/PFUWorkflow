test_that("load_fu_allocation_tables() works for a non-existent country", {

  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "fu_analysis_folder")

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
    # Try to read a country for which no allocations or efficiencies exist.
    # (Note that the test setup has only "GHA" and "ZAF" countries, so "GRC" should return NULL.)
    expect_null(load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                          countries = "GRC"))
    expect_null(load_eta_fu_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                          countries = "GRC"))

    # Try when we ask for one country that DOES exist and one that doesn't exist.
    # We should NOT get NULL here.
    expect_true(!is.null(load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                                   countries = c("GRC", "GHA"))))
    expect_true(!is.null(load_eta_fu_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                                   countries = c("GRC", "GHA"))))

    # Try when we ask for two countries that exist. Should get one big data frame.
    result_alloc <- load_fu_allocation_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                              countries = c("GHA", "ZAF"))
    expect_equal(result_alloc[[IEATools::iea_cols$country]] %>% unique(), c("GHA", "ZAF"))
    result_eff <- load_eta_fu_tables(fu_analysis_folder = readd("fu_analysis_folder", path = testing_setup$cache_path),
                                            countries = c("GHA", "ZAF"))
    expect_equal(result_eff[[IEATools::iea_cols$country]] %>% unique(), c("GHA", "ZAF"))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})


test_that("simple example works when allocation table is already complete", {
  complete_fu_allocation_tables <- IEATools::load_fu_allocation_data()
  # Set up exemplar list
  el <- tibble::tribble(
    ~Country, ~Year, ~Exemplars,
    "GHA", 1971, NULL,
    "GHA", 2000, NULL)
  # Load IEA data
  specified_iea_data <- IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()
  # Assemble complete allocation tables
  completed <- assemble_fu_allocation_tables(incomplete_allocation_tables =
                                               complete_fu_allocation_tables,
                                             exemplar_lists = el,
                                             specified_iea_data = specified_iea_data,
                                             countries = "GHA")
  # Make sure we got back everything we started with
  expected <- complete_fu_allocation_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
    IEATools::tidy_fu_allocation_table()
  # Because this wasn't a situation in which any exemplars will be used,
  # the source for all rows should be "GHA".
  expect_equal(completed %>% dplyr::select(IEATools::template_cols$c_source) %>% unique() %>% unlist() %>% unname(), "GHA")
  # Make sure the number of rows is same for expected and completed.
  expect_equal(nrow(completed), nrow(expected))
})


test_that("simple example for assemble_fu_allocation_tables() works", {
  incomplete_fu_allocation_tables <- IEATools::load_fu_allocation_data() %>%
    dplyr::filter(! (Country == "GHA" & Ef.product == "Primary solid biofuels" & Destination == "Residential"))
  # Set up exemplar list
  el <- tibble::tribble(
    ~Country, ~Year, ~Exemplars,
    "GHA", 1971, c("ZAF"),
    "GHA", 2000, c("ZAF"))
  # Load IEA data
  specified_iea_data <- IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()
  # Assemble complete allocation tables
  completed <- assemble_fu_allocation_tables(incomplete_allocation_tables =
                                               incomplete_fu_allocation_tables,
                                             exemplar_lists = el,
                                             specified_iea_data = specified_iea_data,
                                             countries = "GHA")
  # Make sure we picked up 2 rows of Residential PSB consumption
  completed %>%
    dplyr::filter(.data[[IEATools::template_cols$destination]] == "Residential",
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::iea_cols$year]] == 1971) %>%
    nrow() %>%
    expect_equal(2)
  # Make sure the products are MTH.100.C and LTH.20.C, as expected
  completed %>%
    dplyr::filter(.data[[IEATools::template_cols$destination]] == "Residential",
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::iea_cols$year]] == 1971) %>%
    magrittr::extract2(IEATools::template_cols$eu_product) %>%
    expect_equal(c("MTH.100.C", "LTH.20.C"))
})


test_that("assemble_fu_allocation_tables() and assemble_eta_fu_tables() work as expected.", {
  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$CompletedEfficiencyTables,
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
    # Make sure that GHA's Residential PSBs are now supplied by World.
    residential_psb <- GHA_allocations_completed %>%
      dplyr::filter(.data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                    .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential)
    expect_equal(nrow(residential_psb), 4)
    expect_equal(residential_psb %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 1971) %>% nrow(), 2)
    expect_equal(residential_psb %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 2000) %>% nrow(), 2)
    expect_equal(residential_psb %>% magrittr::extract2(IEATools::template_cols$c_source) %>% unique(), "World")

    # Check the completed FU Efficiency tables.
    GHA_efficiencies_completed <- readd_by_country(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables,
                                                   country = "GHA",
                                                   cache_path = testing_setup$cache_path)
    wood_cookstove_efficiency <- GHA_efficiencies_completed %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves")
    expect_equal(nrow(wood_cookstove_efficiency), 4)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 1971) %>% nrow(), 2)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 2000) %>% nrow(), 2)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu) %>% nrow(), 2)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u) %>% nrow(), 2)
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("simple example for assemble_eta_fu_tables() works", {
  incomplete_eta_fu_tables <- IEATools::load_eta_fu_data() %>%
    dplyr::filter(! (Country == "GHA" & Machine == "Wood cookstoves"))
  # Set up exemplar list
  el <- tibble::tribble(
    ~Country, ~Year, ~Exemplars,
    "GHA", 1971, c("ZAF"),
    "GHA", 2000, c("ZAF"))
  # Load FU allocation data
  fu_allocation_data <- IEATools::load_fu_allocation_data()
  # Assemble complete allocation tables
  completed <- assemble_eta_fu_tables(incomplete_eta_fu_tables = incomplete_eta_fu_tables,
                                      exemplar_lists = el,
                                      completed_fu_allocation_tables = fu_allocation_data,
                                      countries = "GHA")
  # Make sure we picked up rows of efficiencies for Wood cookstoves
  completed %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA",
                  .data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu) %>%
    nrow() %>%
    expect_equal(2)
})

