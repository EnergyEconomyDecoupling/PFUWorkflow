
test_that("load_fu_allocation_tables() and load_eta_fu_tables() work for a non-existent country", {

  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "CompletedAllocationTables")

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    #### Check allocation tables

    # Try to read a country for which no allocations or efficiencies exist.
    # (Note that the test setup has only "GHA" and "ZAF" countries, so "GRC" should return NULL.)
    expect_null(load_fu_allocation_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder, character_only = TRUE, path = testing_setup$cache_path),
                                          countries = "GRC", generate_missing_fu_allocation_template = FALSE))
    # Now try when we want to generate a template.
    # First, we need to make some data for GRC, by pretending that GHA is GRC.
    iea_data_GRC <- drake::readd(SEAPSUTWorkflow::target_names$Specified, character_only = TRUE, path = testing_setup$cache_path) %>%
      # dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA", .data[[IEATools::iea_cols$year]] == 1971) %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      dplyr::mutate(
        # Pretend that GHA is GRC.
        "{IEATools::iea_cols$country}" := "GRC"
      )
    # Make sure that the GRC data are balanced
    iea_data_GRC %>%
      IEATools::calc_tidy_iea_df_balances() %>%
      IEATools::tidy_iea_df_balanced() %>%
      expect_true()

    # The GRC data should be same as GHA data except for country and balancing.
    # Check by loading IEA data from IEATools package.
    GHA <- IEATools::load_tidy_iea_df() %>%
      # dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA", .data[[IEATools::iea_cols$year]] == 1971) %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      IEATools::specify_all() %>%
      IEATools::fix_tidy_iea_df_balances()

    # Use anti_join to find which rows are different. None should be different.
    dplyr::anti_join(GHA,
                     iea_data_GRC %>% dplyr::mutate("{IEATools::iea_cols$country}" := "GHA"),
                     by = colnames(GHA)) %>%
      nrow() %>%
      expect_equal(0)
    # If we get here without error, we know that we have correctly picked up the GHA data as GRC

    # Now ask for an fu allocation table for GRC.
    # GRC does not exist, so it will generate a blank template and write to disk.
    GRC_alloc_table <- load_fu_allocation_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder, character_only = TRUE, path = testing_setup$cache_path),
                                                 specified_iea_data = iea_data_GRC,
                                                 countries = "GRC",
                                                 generate_missing_fu_allocation_template = TRUE)
    # Check that the file exists on disk
    expect_true(file.exists(testing_setup$plan %>%
                              dplyr::filter(target == SEAPSUTWorkflow::target_names$fu_analysis_folder) %>%
                              dplyr::select("command") %>%
                              unlist() %>%
                              # All of the above
                              file.path("GRC", "GRC FU Analysis.xlsx")))
    # Make sure it gets GRC as a country.
    expect_equal(GRC_alloc_table[[IEATools::iea_cols$country]] %>% unique(), "GRC")
    # It should have empty (NA) Machine and Eu.product columns
    expect_true(GRC_alloc_table[[IEATools::template_cols$machine]] %>% is.na() %>% all())
    expect_true(GRC_alloc_table[[IEATools::template_cols$eu_product]] %>% is.na() %>% all())
    # If we make the template tidy, we should get no rows.
    # That's because the tidy version strips the fu allocation table to only allocation rows.
    # A blank template has no allocation rows, so the tidy version of the grc object should have no rows.
    tidy_GRC <- IEATools::tidy_fu_allocation_table(GRC_alloc_table)
    expect_equal(nrow(tidy_GRC), 0)
    # Verify that all meaningful rows in the allocation table come from the GRC IEA data
    iea_like_rows <- GRC_alloc_table %>%
      dplyr::select(IEATools::iea_cols$country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage,
                    IEATools::iea_cols$ledger_side, IEATools::iea_cols$flow_aggregation_point, IEATools::iea_cols$unit,
                    IEATools::template_cols$ef_product, IEATools::template_cols$destination) %>%
      dplyr::rename(
        "{IEATools::iea_cols$flow}" := IEATools::template_cols$destination,
        "{IEATools::iea_cols$product}" := IEATools::template_cols$ef_product
      ) %>%
      unique()
    # Get the correct rows from the iea data table
    iea_data_GRC_cons_eiou <- iea_data_GRC %>%
      dplyr::filter(.data[[IEATools::iea_cols$ledger_side]] == IEATools::ledger_sides$consumption |
                      (.data[[IEATools::iea_cols$ledger_side]] == IEATools::ledger_sides$supply &
                         .data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::aggregation_flows$energy_industry_own_use)) %>%
      dplyr::select(IEATools::iea_cols$country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage,
                    IEATools::iea_cols$ledger_side, IEATools::iea_cols$flow_aggregation_point, IEATools::iea_cols$unit,
                    IEATools::iea_cols$flow, IEATools::iea_cols$product)
    # iea_like_rows and iea_data_GRC_cons_eiou should be same. Use anti_join to find out.
    dplyr::anti_join(iea_like_rows, iea_data_GRC_cons_eiou, by = colnames(iea_like_rows)) %>%
      nrow() %>%
      expect_equal(0)
    # If we get here without error, we know that the template for GRC correctly picked up the rows from the IEA data.


    #### Check the eta_fu_tables.

    # First, try without generating an empty template.
    expect_null(load_eta_fu_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder, path = testing_setup$cache_path, character_only = TRUE),
                                   countries = "GRC", generate_missing_fu_etas_template = FALSE))

    # Now try to generate an empty template.
    # This should fail, because there are no rows in the allocation table from which machines can be determined.
    expect_error(load_eta_fu_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder, path = testing_setup$cache_path, character_only = TRUE),
                                    completed_fu_allocation_tables = drake::readd(SEAPSUTWorkflow::target_names$CompletedAllocationTables, path = testing_setup$cache_path, character_only = TRUE),
                                    tidy_specified_iea_data = drake::readd(SEAPSUTWorkflow::target_names$Specified, path = testing_setup$cache_path, character_only = TRUE),
                                    countries = "GRC", generate_missing_fu_etas_template = TRUE),
                 ".fu_allocations has no allocation rows.")

    # Try when we ask for one country that DOES exist and one that doesn't exist.
    # We should NOT get NULL here.
    expect_true(!is.null(alloc_tables_from_disk <- load_fu_allocation_tables(fu_analysis_folder = drake::readd("fu_analysis_folder",
                                                                                                               path = testing_setup$cache_path),
                                                                             countries = c("GRC", "GHA"),
                                                                             generate_missing_fu_allocation_template = FALSE)))
    # Make sure that fu_tables contains what we expect
    # The GRC portion should have no machines in it.
    # In fact, every entry should be NA.
    alloc_tables_from_disk %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GRC") %>%
      dplyr::select(IEATools::template_cols$machine) %>%
      is.na() %>%
      all() %>%
      expect_true()
    # On the other hand, the GHA portion should have non-NA machines.
    alloc_tables_from_disk %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      dplyr::select(IEATools::template_cols$machine) %>%
      is.na() %>%
      all() %>%
      expect_false()

    # Now look at the eta_fu tables.
    expect_true(!is.null(eta_tables_from_disk <- load_eta_fu_tables(fu_analysis_folder = drake::readd("fu_analysis_folder",
                                                                                                      path = testing_setup$cache_path),
                                                                    countries = c("GRC", "GHA"),
                                                                    generate_missing_fu_etas_template = FALSE)))
    # GRC should have no rows at all, because its fu allocation table is empty.
    eta_tables_from_disk %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GRC") %>%
      nrow() %>%
      expect_equal(0)
    # But GHA should have rows, because the data are available.
    eta_tables_from_disk %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      dplyr::select(IEATools::template_cols$machine) %>%
      is.na() %>%
      all() %>%
      expect_false()


    #############################
    # This test actually reads some data from an allocation table
    # and makes an eta_fu template.
    #############################

    # Pretend that GHA is GRC (we have iea_data_GRC from above)
    # Pretend we have some GRC allocations
    fu_allocations_GRC <- drake::readd(SEAPSUTWorkflow::target_names$CompletedAllocationTables,
                                       path = testing_setup$cache_path,
                                       character_only = TRUE) %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      dplyr::mutate(
        "{IEATools::iea_cols$country}" := "GRC",
        "{IEATools::template_cols$c_source}" := NULL
      )

    # This fu_allocations_GRC object should be same a GHA allocations, except for country.
    fu_allocations_GHA <- IEATools::load_fu_allocation_data() %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
      IEATools::tidy_fu_allocation_table()
    # Check for sameness via anti_join
    dplyr::anti_join(fu_allocations_GRC %>% dplyr::mutate("{IEATools::iea_cols$country}" := "GHA"),
                     fu_allocations_GHA,
                     by = colnames(fu_allocations_GRC)) %>%
      nrow() %>%
      expect_equal(0)


    # Make sure the FU etas worksheet doesn't exist.
    # fu_file is the file that contains the template fu_analysis worksheet.
    fu_file <- file.path(readd(SEAPSUTWorkflow::target_names$fu_analysis_folder,
                               path = testing_setup$cache_path,
                               character_only = TRUE),
                         "GRC",
                         paste0("GRC", IEATools::fu_analysis_file_info$fu_analysis_file_suffix))
    expect_false(IEATools::fu_analysis_file_info$eta_fu_tab_name %in% readxl::excel_sheets(fu_file))

    # Try to load the GRC eta_fu table from disk.
    # First try without specifying a completed_fu_allocation_table
    # This should fail, because the completed_fu_allocation_tables argument is missing
    expect_error(load_eta_fu_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder,
                                                                      path = testing_setup$cache_path,
                                                                      character_only = TRUE),
                                    tidy_specified_iea_data = iea_data_GRC,
                                    countries = "GRC", generate_missing_fu_etas_template = TRUE),
                 'argument "completed_fu_allocation_tables" is missing, with no default')
    # Try again with missing tidy_specified_iea_data. This should also fail, because we need the IEA data
    # to make an eta_fu template from only the completed_fu_allocation_table.
    expect_error(load_eta_fu_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder,
                                                                      path = testing_setup$cache_path,
                                                                      character_only = TRUE),
                                    completed_fu_allocation_tables = fu_allocations_GRC,
                                    countries = "GRC", generate_missing_fu_etas_template = TRUE),
                 'argument "tidy_specified_iea_data" is missing, with no default')

    # Now try with the completed_fu_allocation_tables and tidy_specified_iea_data arguments present
    # This should work!
    eta_fu_table_GRC <- load_eta_fu_tables(fu_analysis_folder = drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder,
                                                                             path = testing_setup$cache_path,
                                                                             character_only = TRUE),
                                           completed_fu_allocation_tables = fu_allocations_GRC,
                                           tidy_specified_iea_data = iea_data_GRC,
                                           countries = "GRC", generate_missing_fu_etas_template = TRUE)

    # This should be a "GRC" template
    expect_equal(eta_fu_table_GRC[[IEATools::iea_cols$country]] %>% unique(), "GRC")
    # Turn the eta_fu part into a tidy data frame. It should have no rows, because it is a template.
    tidy_grc_eta_fu_table <- IEATools::tidy_eta_fu_table(eta_fu_table_GRC) %>%
      dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu)
    expect_equal(nrow(tidy_grc_eta_fu_table), 0)
    # This file ought to look like the GHA eta_fu template, because GRC is based on GHA
    # Make sure that is true.
    fu_alloc_rows <- fu_allocations_GRC %>%
      dplyr::select(IEATools::iea_cols$country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type,
                    IEATools::iea_cols$last_stage,
                    IEATools::template_cols$machine, IEATools::template_cols$eu_product) %>%
      unique()
    eta_fu_rows <- eta_fu_table_GRC %>%
      dplyr::select(IEATools::iea_cols$country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type,
                    IEATools::iea_cols$last_stage,
                    IEATools::template_cols$machine, IEATools::template_cols$eu_product) %>%
      unique()
    # These two data frames should contain the same information, because
    # eta_fu_table_GRC was created from fu_allocations_GRC.
    dplyr::anti_join(fu_alloc_rows, eta_fu_rows, by = colnames(fu_alloc_rows)) %>%
      nrow() %>%
      expect_equal(0)

    # Try when we ask for two countries that exist. Should get one big data frame.
    result_alloc <- load_fu_allocation_tables(fu_analysis_folder = drake::readd("fu_analysis_folder", path = testing_setup$cache_path),
                                              countries = c("GHA", "ZAF"))
    expect_equal(result_alloc[[IEATools::iea_cols$country]] %>% unique(), c("GHA", "ZAF"))
    result_eff <- load_eta_fu_tables(fu_analysis_folder = drake::readd("fu_analysis_folder", path = testing_setup$cache_path),
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
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(additional_exemplar_countries = "WLD",
                                                        how_far = SEAPSUTWorkflow::target_names$CompletedPhiuTables,
                                                        setup_exemplars = TRUE)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Verify that the incomplete fu allocation tables look as we expect.
    incomplete_alloc_tables <- drake::readd(target = SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
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
    readd_by_country(SEAPSUTWorkflow::target_names$MachineData,
                     country = "GHA",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$eu_product]] == "MTH.100.C") %>%
      nrow() %>%
      expect_equal(0)

    # Check that World DOES have the allocation and efficiencies for Wood cookstoves.
    # Allocations:
    readd_by_country(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                     country = "WLD",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$destination]] == "Residential") %>%
      nrow() %>%
      expect_equal(1)
    # Efficiencies:
    readd_by_country(SEAPSUTWorkflow::target_names$MachineData,
                     country = "WLD",
                     cache_path = testing_setup$cache_path) %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves",
                    .data[[IEATools::template_cols$eu_product]] == "MTH.100.C",
                    .data[[IEATools::template_cols$quantity]] == "eta.fu") %>%
      nrow() %>%
      expect_equal(2) # Because 2 years (1971 and 2000)

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
    expect_equal(residential_psb %>% magrittr::extract2(IEATools::template_cols$c_source) %>% unique(), "WLD")

    # Check the completed FU Efficiency tables.
    GHA_efficiencies_completed <- readd_by_country(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables,
                                                   country = "GHA",
                                                   cache_path = testing_setup$cache_path)
    wood_cookstove_efficiency <- GHA_efficiencies_completed %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves")
    expect_equal(nrow(wood_cookstove_efficiency), 2)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 1971) %>% nrow(), 1)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 2000) %>% nrow(), 1)
    expect_equal(wood_cookstove_efficiency %>% dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu) %>% nrow(), 2)

    # Check the completed phi tables.
    GHA_phi_completed <- readd_by_country(SEAPSUTWorkflow::target_names$CompletedPhiTables,
                                                   country = "GHA",
                                                   cache_path = testing_setup$cache_path)
    wood_cookstove_phi <- GHA_phi_completed %>%
      dplyr::filter(.data[[IEATools::template_cols$machine]] == "Wood cookstoves")
    expect_equal(nrow(wood_cookstove_phi), 2)
    expect_equal(wood_cookstove_phi %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 1971) %>% nrow(), 1)
    expect_equal(wood_cookstove_phi %>% dplyr::filter(.data[[IEATools::iea_cols$year]] == 2000) %>% nrow(), 1)
    expect_equal(wood_cookstove_phi %>% dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u) %>% nrow(), 2)
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


test_that("MachineData works in assemble_eta_fu_tables()", {
   # Make some incomplete efficiency tables for GHA by removing Wood cookstoves.
   # Information from the exemplar, ZAF, will supply efficiency for Wood cookstoves for GHA.
   incomplete_eta_fu_tables <- IEATools::load_eta_fu_data() %>%
     dplyr::filter(! (Country == "GHA" & Machine == "Wood cookstoves"))

   # Keep track of the cookstove rows we deleted from GHA.
   # Make sure we get the same number of cookstove rows later.
   orig_GHA_cookstove_rows <- IEATools::load_eta_fu_data() %>%
     dplyr::filter(Country == "GHA" & Machine == "Wood cookstoves")

   # Convert incomplete_eta_fu_tables to MachineData format.
   MachineData <- incomplete_eta_fu_tables %>%
     dplyr::mutate(
       "{IEATools::template_cols$maximum_values}" := NULL,
       "{IEATools::iea_cols$unit}" := NULL
     ) %>%
     tidyr::pivot_longer(cols = IEATools::year_cols(.),
                         names_to = IEATools::iea_cols$year,
                         values_to = IEATools::template_cols$.values)

   # The rows for Wood cookstoves were present but are now missing.
   IEATools::load_eta_fu_data() %>%
     dplyr::filter(Country == "GHA" & Machine == "Wood cookstoves") %>%
     nrow() %>%
     expect_gt(0)
   cookstoves_gone <- MachineData %>%
     dplyr::filter(Country == "GHA", Machine == "Wood cookstoves")
   expect_equal(nrow(cookstoves_gone), 0)

   # Set up exemplar list
   el <- tibble::tribble(
     ~Country, ~Year, ~Exemplars,
     "GHA", 1971, c("ZAF"),
     "GHA", 2000, c("ZAF"))
   # Load FU allocation data.
   # An efficiency is needed for each machine in FU allocation data.
   fu_allocation_data <- IEATools::load_fu_allocation_data()
   # Assemble complete allocation tables
   completed <- assemble_eta_fu_tables(incomplete_eta_fu_tables = MachineData,
                                       exemplar_lists = el,
                                       completed_fu_allocation_tables = fu_allocation_data,
                                       countries = "GHA")
   # Show that the missing rows have been picked up from the exemplar country, ZAF.
  rows_from_exemplar <- completed %>%
    dplyr::filter(Country == "GHA", Machine == "Wood cookstoves") %>%
    dplyr::mutate(
      eta.fu.phi.u.source = NULL
    )

  expect_equal(nrow(rows_from_exemplar), nrow(orig_GHA_cookstove_rows))


  rows_from_ZA <- IEATools::load_eta_fu_data() %>%
    dplyr::filter(Country == "ZAF", Machine == "Wood cookstoves", Quantity %in% c("eta.fu", "phi.u")) %>%
    dplyr::mutate(
      "{IEATools::template_cols$maximum_values}" := NULL,
      "{IEATools::iea_cols$unit}" := NULL
    ) %>%
    tidyr::pivot_longer(cols = IEATools::year_cols(.),
                        names_to = IEATools::iea_cols$year,
                        values_to = IEATools::template_cols$.values) %>%
    dplyr::mutate(
      Year = as.numeric(Year)
    )

  diffs <- dplyr::anti_join(rows_from_exemplar, rows_from_ZA, by = matsindf::everything_except(rows_from_exemplar, "Country") %>% as.character())
  expect_equal(nrow(diffs), 0)
})


test_that("simple example for assemble_phi_u_tables() works as expected", {
  # Load the phi constants table
  phi_constants_table <- IEATools::load_phi_constants_table()

  # Load all the MachineData for our examples.
  phi_table <- IEATools::load_eta_fu_data() %>%
    # Convert incomplete_eta_fu_tables to MachineData format.
    dplyr::mutate(
      "{IEATools::template_cols$maximum_values}" := NULL,
      "{IEATools::iea_cols$unit}" := NULL
    ) %>%
    tidyr::pivot_longer(cols = IEATools::year_cols(.),
                        names_to = IEATools::iea_cols$year,
                        values_to = IEATools::template_cols$.values) %>%
    # Convert to a table of phi values only
    dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u)
  # Set a value to NA (Charcoal stoves, MTH.100.C, GHA, 1971) in the phi table.
  incomplete_phi_table <- phi_table %>%
    dplyr::mutate(
      "{IEATools::template_cols$.values}" := dplyr::case_when(
        .data[[IEATools::iea_cols$country]] == "GHA" &
          .data[[IEATools::iea_cols$year]] == 1971 &
          .data[[IEATools::template_cols$machine]] == "Charcoal stoves" ~ NA_real_,
        TRUE ~ .data[[IEATools::template_cols$.values]]
      )
    )

  # Run through the assemble_phi_u_tables function
  completed_phi_u_table <- assemble_phi_u_tables(incomplete_phi_table,
                                                 phi_constants_table,
                                                 countries = "GHA")

  # Make sure we get the correct value.
  expected_value <- phi_constants_table %>%
    dplyr::filter(.data[[IEATools::phi_constants_names$product_colname]] == "MTH.100.C") %>%
    magrittr::extract2(IEATools::phi_constants_names$phi_colname)
  actual_value <- completed_phi_u_table %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA",
                  .data[[IEATools::iea_cols$year]] == 1971,
                  .data[[IEATools::template_cols$machine]] == "Charcoal stoves") %>%
    magrittr::extract2(IEATools::template_cols$.values)
  expect_equal(actual_value, expected_value)

  # Make sure we have a phi.source column
  expect_true(IEATools::phi_constants_names$phi_source_colname %in% names(completed_phi_u_table))
})


test_that("assemble_phi_u_tables() throws an error when not complete", {
  # Make a bogus table that needs to be completed
  empty_table <- data.frame(Country = "GHA", Year = 1971, Machine = "machine",
                            Eu.product = "MTH.100.C", Quantity = "phi.u", .values = NA_real_)
  phi_constants <- data.frame(Product = "L", phi = 0.955, is.useful = TRUE)
  expect_error(assemble_phi_u_tables(empty_table, phi_constants_table = phi_constants,
                                     countries = "GHA"),
               "Not all useful energy carriers have been assigned phi values")
})


test_that("assemble_phi_u_tables() works as expected in the workflow", {

  # Create a directory structure in a tempdir for the allocation tables
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "CompletedPhiTables")

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
    incomplete_phi_tables <- drake::readd(target = SEAPSUTWorkflow::target_names$MachineData,
                                          path = testing_setup$cache_path,
                                          character_only = TRUE)
    # Check that all values are present.
    incomplete_phi_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA",
                    .data[[IEATools::iea_cols$year]] == 1971,
                    .data[[IEATools::template_cols$machine]] == "Charcoal stoves",
                    .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u) %>%
      magrittr::extract2(IEATools::template_cols$.values) %>%
      is.na() %>%
      expect_false()

    # Now check that the completed phi tables target has that value filled
    # from the Excel sheet
    completed_phi_tables <- drake::readd(target = SEAPSUTWorkflow::target_names$CompletedPhiTables,
                                         path = testing_setup$cache_path,
                                         character_only = TRUE)
    completed_phi_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA",
                    .data[[IEATools::iea_cols$year]] == 1971,
                    .data[[IEATools::template_cols$machine]] == "Charcoal stoves",
                    .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u) %>%
      magrittr::extract2(IEATools::template_cols$.values) %>%
      expect_equal(0.200991558)


  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


