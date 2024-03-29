###########################################################
context("Plan Functions")
###########################################################

test_that("get_plan() works", {
  my_plan <- get_plan(countries = c("GHA", "ZAF"),
                      max_year = 1999,
                      country_concordance_path = "countryconcordancepath",
                      phi_constants_path = "phiconstantspath",
                      iea_data_path = "datapath",
                      ceda_data_folder = "cedapath",
                      machine_data_path = "machinepath",
                      exemplar_table_path = "exemplarpath",
                      fu_analysis_folder = "FUpath",
                      reports_source_folders = "reports_source_folders",
                      reports_dest_folder = "reports_dest_folder",
                      pipeline_caches_folder = "pipeline_caches_folder",
                      pipeline_releases_folder = "pipeline_releases_folder")

  # Keep track of row numbers for indexing purposes
  rn <- 1

  # Make sure the pieces get created correctly with tidyeval.
  expect_equal(my_plan[[rn, "target"]], "countries")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "alloc_and_eff_couns")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "max_year")
  expect_equal(my_plan[[rn, "command"]], list(1999))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("datapath"))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("countryconcordancepath"))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("phiconstantspath"))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("cedapath"))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("machinepath"))

  expect_equal(my_plan[[rn <- rn + 1, "command"]], list("exemplarpath"))

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "fu_analysis_folder")
  expect_equal(my_plan[[rn, "command"]], list("FUpath"))

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "reports_source_folders")
  expect_equal(my_plan[[rn, "command"]], list("reports_source_folders"))

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "reports_dest_folder")
  expect_equal(my_plan[[rn, "command"]], list("reports_dest_folder"))

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "pipeline_caches_folder")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "pipeline_releases_folder")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "release")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "CountryConcordanceTable")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "FinalDemandSectors")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "PrimaryIndustryPrefixes")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "AllIEAData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "IEAData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "CEDAData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "AllMachineData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "MachineData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "SocioEconData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "balanced_before")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "BalancedIEAData")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "balanced_after")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "OKToProceed")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Specified")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "PSUT_final")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "ExemplarLists")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "PhiConstants")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "IncompleteAllocationTables")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "TidyIncompleteAllocationTables")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "CompletedAllocationTables")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "CompletedEfficiencyTables")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "CompletedPhiuTables")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Cmats")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "EtafuPhiuvecs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Etafuvecs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Phiuvecs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Phipfvecs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "Phivecs")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "PSUT_useful")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "PSUT")


  expect_equal(my_plan[[rn <- rn + 1, "target"]], "AllocationGraphs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "NonStationaryAllocationGraphs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "EfficiencyGraphs")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "ExergyEnergyGraphs")

  expect_equal(my_plan[[rn <- rn + 1, "target"]], "ReleasePSUT")
  expect_equal(my_plan[[rn <- rn + 1, "target"]], "StoreCache")
})


test_that("keeping only some rows of a plan works", {
  full_plan <- get_plan(countries = c("GHA", "ZAF"),
                        max_year = 1999,
                        country_concordance_path = "countryconcordancepath",
                        phi_constants_path = "phiconstantspath",
                        iea_data_path = "datapath",
                        ceda_data_folder = "cedapath",
                        machine_data_path = "machinepath",
                        exemplar_table_path = "exemplarpath",
                        fu_analysis_folder = "FUpath",
                        reports_source_folders = "reports_source_path",
                        reports_dest_folder = "reports_dest_folder",
                        pipeline_caches_folder = "pipeline_caches_folder",
                        pipeline_releases_folder = "pipeline_releases_folder"
                        )
  short_plan <- get_plan(countries = c("GHA", "ZAF"),
                         max_year = 1999,
                         country_concordance_path = "countryconcordancepath",
                         phi_constants_path = "phiconstantspath",
                         iea_data_path = "mypath",
                         ceda_data_folder = "cedapath",
                         machine_data_path = "machinepath",
                         exemplar_table_path = "exemplarpath",
                         fu_analysis_folder = "FUpath",
                         reports_source_folders = "reports_source_path",
                         reports_dest_folder = "reports_dest_folder",
                         pipeline_caches_folder = "pipeline_caches_folder",
                         pipeline_releases_folder = "pipeline_releases_folder",
                         how_far = "Specified"
                         )

  expect_lt(nrow(short_plan), nrow(full_plan))

  full_plan[nrow(short_plan), ] %>%
    dplyr::select(target) %>%
    unlist() %>%
    unname() %>%
    expect_equal("Specified")
})


test_that("make() works", {

  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = "ExergyEnergyGraphs")

  tryCatch({

    # Make the plan in the temp_cache
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Now readd data from the cache and perform tests.
    expect_equal(drake::readd(target = PFUWorkflow::target_names$countries, path = testing_setup$cache_path, character_only = TRUE),
                 c("GHA", "ZAF"))
    expect_equal(drake::readd(target = PFUWorkflow::target_names$max_year, path = testing_setup$cache_path, character_only = TRUE),
                 2000)
    expect_equal(drake::readd(target = PFUWorkflow::target_names$exemplar_table_path, path = testing_setup$cache_path, character_only = TRUE),
                 testing_setup$plan %>% dplyr::filter(target == "exemplar_table_path") %>% magrittr::extract2("command") %>% unlist())
    expect_true(!is.null(drake::readd(target = PFUWorkflow::target_names$fu_analysis_folder, path = testing_setup$cache_path, character_only = TRUE)))
    # expect_true(!is.null(drake::readd(target = PFUWorkflow::target_names$reports_dest_folder, path = testing_setup$cache_path, character_only = TRUE)))

    # Be sure that IEAData is present
    expect_true(!is.null(drake::readd(target = PFUWorkflow::target_names$IEAData, path = testing_setup$cache_path, character_only = TRUE)))

    # Be sure that readd_by_country also works
    expected_all_iea_data <- IEATools::sample_iea_data_path() %>%
      IEATools::load_tidy_iea_df()
    # Read directly out of the cache.
    expect_equal(drake::readd(target = "AllIEAData", path = testing_setup$cache_path), expected_all_iea_data)
    # Read GHA IEA data from the cache
    expected_GHA_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "GHA")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "GHA", cache_path = testing_setup$cache_path), expected_GHA_data)
    # Read ZAF IEA data from the cache
    expected_ZAF_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "ZAF")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "ZAF", cache_path = testing_setup$cache_path), expected_ZAF_data)

    # Check energy balance information
    expect_equal(drake::readd("balanced_before", path = testing_setup$cache_path), c(FALSE, FALSE))
    expect_equal(drake::readd("balanced_after", path = testing_setup$cache_path), c(TRUE, TRUE))
    expect_true(drake::readd("OKToProceed", path = testing_setup$cache_path))

    # Check specification process, reading by country.
    expect_true(!is.null(readd_by_country(target = "Specified", country = "GHA", cache_path = testing_setup$cache_path)))

    # Check making PSUT matrices, reading by country.
    expect_true(!is.null(readd_by_country(target = "PSUT_final", country = "ZAF", cache_path = testing_setup$cache_path)))

    # Ensure that ExemplarLists were read correctly.
    expect_true(!is.null(readd_by_country(target = "ExemplarLists", country = "GHA", cache_path = testing_setup$cache_path)))
    expect_true(!is.null(readd_by_country(target = "ExemplarLists", country = "ZAF", cache_path = testing_setup$cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "ExemplarLists", country = "GHA", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "ExemplarLists", country = "ZAF", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")

    # Test allocation tables.
    expect_true(!is.null(readd_by_country(target = "IncompleteAllocationTables", country = "GHA", cache_path = testing_setup$cache_path)))
    expect_true(!is.null(readd_by_country(target = "IncompleteAllocationTables", country = "ZAF", cache_path = testing_setup$cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "IncompleteAllocationTables", country = "GHA", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "IncompleteAllocationTables", country = "ZAF", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")
    # Check that the ZAF table looks OK.
    ZAF_allocation_table <- readd_by_country(target = PFUWorkflow::target_names$IncompleteAllocationTables, country = "ZAF", cache_path = testing_setup$cache_path)
    expect_true(all(!is.na(ZAF_allocation_table[[IEATools::iea_cols$method]])))
    expect_true(all(!is.na(ZAF_allocation_table[[IEATools::iea_cols$energy_type]])))

    # Test completed allocation tables.
    expect_true(!is.null(readd_by_country(target = "CompletedAllocationTables", country = "GHA", cache_path = testing_setup$cache_path)))
    expect_true(!is.null(readd_by_country(target = "CompletedAllocationTables", country = "ZAF", cache_path = testing_setup$cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "CompletedAllocationTables", country = "GHA", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "CompletedAllocationTables", country = "ZAF", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")
    # Check that the ZAF table looks OK.
    ZAF_completed_allocation_table <- readd_by_country(target = PFUWorkflow::target_names$IncompleteAllocationTables, country = "ZAF", cache_path = testing_setup$cache_path)
    expect_true(all(!is.na(ZAF_completed_allocation_table[[IEATools::iea_cols$method]])))
    expect_true(all(!is.na(ZAF_completed_allocation_table[[IEATools::iea_cols$energy_type]])))
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })
})

