###########################################################
context("Plan Functions")
###########################################################

test_that("get_plan works", {
  my_plan <- get_plan(iea_data_path = "datapath",
                      exemplar_table_path = "exemplarpath",
                      fu_analysis_folder = "FUpath",
                      countries = c("GHA", "ZAF"),
                      max_year = 1999)

  # Make sure the pieces get created correctly with tidyeval.
  expect_equal(my_plan[[1, "target"]], "countries")
  expect_equal(my_plan[[2, "target"]], "allocation_and_efficiency_countries")
  expect_equal(my_plan[[3, "target"]], "max_year")
  expect_equal(my_plan[[3, "command"]], list(1999))
  expect_equal(my_plan[[4, "command"]], list("datapath"))
  expect_equal(my_plan[[5, "command"]], list("exemplarpath"))
  expect_equal(my_plan[[6, "target"]], "fu_analysis_folder")
  expect_equal(my_plan[[6, "command"]], list("FUpath"))
  expect_equal(my_plan[[7, "target"]], "AllIEAData")
  expect_equal(my_plan[[8, "target"]], "IEAData")
  expect_equal(my_plan[[9, "target"]], "balanced_before")
  expect_equal(my_plan[[10, "target"]], "BalancedIEAData")
  expect_equal(my_plan[[11, "target"]], "balanced_after")
  expect_equal(my_plan[[12, "target"]], "OKToProceed")
  expect_equal(my_plan[[13, "target"]], "Specified")
  expect_equal(my_plan[[14, "target"]], "PSUT_final")
})


test_that("keeping only some rows of a plan works", {
  full_plan <- get_plan(iea_data_path = "datapath",
                        exemplar_table_path = "exemplarpath",
                        fu_analysis_folder = "FUpath",
                        countries = c("GHA", "ZAF"),
                        max_year = 1999)
  short_plan <- get_plan(iea_data_path = "mypath",
                      exemplar_table_path = "exemplarpath",
                      fu_analysis_folder = "FUpath",
                      countries = c("GHA", "ZAF"),
                      max_year = 1999,
                      how_far = "Specified")

  expect_lt(nrow(short_plan), nrow(full_plan))

  full_plan[nrow(short_plan), ] %>%
    dplyr::select(target) %>%
    unlist() %>%
    unname() %>%
    expect_equal("Specified")
})


test_that("make works", {

  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing()

  tryCatch({

    # Make the plan in the temp_cache
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Now readd data from the cache and perform tests.
    expect_equal(drake::readd(target = "countries", path = testing_setup$cache_path), c("GHA", "ZAF"))
    expect_equal(drake::readd(target = "max_year", path = testing_setup$cache_path), 2000)
    expect_equal(drake::readd(target = "iea_data_path", path = testing_setup$cache_path), IEATools::sample_iea_data_path())
    expect_equal(drake::readd(target = "exemplar_table_path", path = testing_setup$cache_path),
                 testing_setup$plan %>% dplyr::filter(target == "exemplar_table_path") %>% magrittr::extract2("command") %>% unlist())
    expect_true(!is.null(drake::readd(target = "fu_analysis_folder", path = testing_setup$cache_path)))

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
    ZAF_allocation_table <- readd_by_country(target = SEAPSUTWorkflow::target_names$IncompleteAllocationTables, country = "ZAF", cache_path = testing_setup$cache_path)
    expect_true(all(!is.na(ZAF_allocation_table[[IEATools::iea_cols$method]])))
    expect_true(all(!is.na(ZAF_allocation_table[[IEATools::iea_cols$energy_type]])))

    # Test efficiency tables.
    expect_true(!is.null(readd_by_country(target = "IncompleteEfficiencyTables", country = "GHA", cache_path = testing_setup$cache_path)))
    expect_true(!is.null(readd_by_country(target = "IncompleteEfficiencyTables", country = "ZAF", cache_path = testing_setup$cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "IncompleteEfficiencyTables", country = "GHA", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "IncompleteEfficiencyTables", country = "ZAF", cache_path = testing_setup$cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")

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

  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
