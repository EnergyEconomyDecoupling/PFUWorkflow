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
  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = PFUWorkflow::target_names$IEAData)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    GHAdata <- readd_by_country(PFUWorkflow::target_names$IEAData,
                                country = "GHA",
                                cache_path = testing_setup$cache_path)
    expect_equal(GHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "GHA")
    ZAFdata <- readd_by_country(PFUWorkflow::target_names$IEAData,
                                country = "ZAF",
                                cache_path = testing_setup$cache_path)
    expect_equal(ZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "ZAF")
    GHAZAFdata <- readd_by_country(PFUWorkflow::target_names$IEAData,
                                   country = c("GHA", "ZAF"),
                                   cache_path = testing_setup$cache_path)
    expect_equal(GHAZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
    ZAFGHAdata <- readd_by_country(PFUWorkflow::target_names$IEAData,
                                   country = c("ZAF", "GHA"),
                                   cache_path = testing_setup$cache_path)
    expect_equal(ZAFGHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("clean_targets() works as expected", {
  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = PFUWorkflow::target_names$CEDAData)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
    # Verify that all targets are OK. If so, no targets will be out of date.
    expect_equal(length(drake::outdated(testing_setup$plan, cache = testing_setup$temp_cache)), 0)
    # Now clean some targets, by default everything after IEAData.
    # In this case, everything after IEAData is only 3 targets.
    clean_targets(path = testing_setup$cache_path)
    expect_equal(drake::outdated(testing_setup$plan, cache = testing_setup$temp_cache),
                 c("CEDAData", "FinalDemandSectors", "PrimaryIndustryPrefixes"))
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("setup_exemplars() works as expected", {
  testing_setup <- PFUWorkflow:::set_up_for_testing(additional_exemplar_countries = c("WRLD"),
                                                        how_far = PFUWorkflow::target_names$CompletedAllocationTables,
                                                        setup_exemplars = TRUE)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that there is World IEA data
    iea_data <- IEATools::load_tidy_iea_df(testing_setup$plan %>%
                                           dplyr::filter(.data[["target"]] == PFUWorkflow::target_names$iea_data_path) %>%
                                           magrittr::extract2("command") %>%
                                           unlist())
    iea_data %>%
      magrittr::extract2(IEATools::iea_cols$country) %>%
      unique() %>%
      expect_equal(c("GHA", "WRLD", "ZAF"))
    # World and ZAF should be the same.
    expect_equal(iea_data %>%
                   dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF"),
                 iea_data %>%
                   dplyr::filter(.data[[IEATools::iea_cols$country]] == "WRLD") %>%
                   dplyr::mutate(
                     "{IEATools::iea_cols$country}" := "ZAF"
                   ))

    # Check that there is a World exemplar table
    testing_setup$plan %>%
      dplyr::filter(target == PFUWorkflow::target_names$fu_analysis_folder) %>%
      magrittr::extract2("command") %>%
      unlist() %>%
      dir.exists() %>%
      expect_true()
    # Make sure we have allocation tables.
    alloc_tables <- drake::readd(PFUWorkflow::target_names$IncompleteAllocationTables,
                                 path = testing_setup$cache_path,
                                 character_only = TRUE)
    world_alloc_tables <- alloc_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "WRLD")
    zaf_alloc_tables <- alloc_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
    # World and ZAF should be the same, because World is just rebranded ZAF.
    expect_equal(world_alloc_tables %>%
                   dplyr::mutate("{IEATools::iea_cols$country}" := "ZAF"),
                 zaf_alloc_tables)
    # Extract the World FU allocation table. Make sure it matches the ZAF FU allocation table.
    ZAF_fu_allocation_table <- readd_by_country(PFUWorkflow::target_names$IncompleteAllocationTables,
                                                country = "ZAF",
                                                cache_path = testing_setup$cache_path)
    World_fu_allocation_table <- readd_by_country(PFUWorkflow::target_names$IncompleteAllocationTables,
                                                  country = "WRLD",
                                                  cache_path = testing_setup$cache_path)
    expect_equal(World_fu_allocation_table %>%
                   dplyr::mutate("{IEATools::iea_cols$country}" := "ZAF"),
                 ZAF_fu_allocation_table)
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })

})


test_that("set_up_for_testing() works when setting up exemplars with no exemplar countries ", {
  # When we set up exemplars and we don't give additional exemplar countries,
  # we should get "World" as an exemplar.
  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = PFUWorkflow::target_names$fu_analysis_folder,
                                                        setup_exemplars = TRUE)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    testing_setup$plan %>%
      dplyr::filter(.data[["target"]] == PFUWorkflow::target_names$alloc_and_eff_couns) %>%
      magrittr::extract2("command") %>%
      grepl("WRLD", .) %>%
      expect_true()
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })

})



# Test get_fd_sectors()
test_that("get_fd_sectors() works as expected", {

  # Create test data
  fd_sectors <- get_fd_sectors()

  # Check object type is equal to list
  testthat::expect_type(fd_sectors, "list")

  # Check that the length of fd_sectors_list is equal to 51
  testthat::expect_equal(length(fd_sectors), 54)

  # Check that each entry in fd_sectors is correct
  testthat::expect_equal(unlist(unname(fd_sectors)),
                         c("BKB/peat briquette plants",                   "Blast furnaces",
                           "Charcoal production plants",                  "Coal liquefaction plants",
                           "Coal mines",                                  "Coke ovens",
                           "Gas works",                                   "Gas-to-liquids (GTL) plants",
                           "Gasification plants for biogases",            "Liquefaction (LNG) / regasification plants",
                           "Non-specified (energy)",                      "Nuclear industry",
                           "Oil and gas extraction",                      "Oil refineries",
                           "Own use in electricity, CHP and heat plants", "Patent fuel plants",
                           "Pumped storage plants",                       "Mining and quarrying",
                           "Construction",                                "Iron and steel",
                           "Chemical and petrochemical",                  "Non-ferrous metals",
                           "Non-metallic minerals",                       "Transport equipment",
                           "Machinery",                                   "Food and tobacco",
                           "Paper, pulp and print",                       "Paper, pulp and printing",
                           "Wood and wood products",                      "Textile and leather",
                           "Non-specified (industry)",                    "Industry not elsewhere specified",
                           "Oil extraction",                              "Natural gas extraction",
                           "Domestic navigation",                         "World marine bunkers",
                           "International navigation",                    "Domestic aviation",
                           "World aviation bunkers",                      "International aviation",
                           "Road",                                        "Rail",
                           "Pipeline transport",                          "Non-specified (transport)",
                           "Transport not elsewhere specified",           "Residential",
                           "Commercial and public services",              "Agriculture/forestry",
                           "Fishing",                                     "Non-specified (other)",
                           "Final consumption not elsewhere specified",   "Non-energy use industry/transformation/energy",
                           "Non-energy use in transport",                 "Non-energy use in other"))

})

# Create test data using Recca example matrices
test_sutdata <- Recca::UKEnergy2000mats %>%
  # dplyr::filter(Last.stage == "Final") %>% # Services Y rows being added as sectors
  tidyr::pivot_wider(id_cols = Country:Last.stage,
                     names_from = "matrix.name",
                     values_from = "matrix") %>%
  dplyr::mutate(Method = "PCM", .after = "Country") %>%
  dplyr::relocate(Year, .after = "Last.stage")

# Restrict final demand sectors to just "Residential" and "Transport".
# As Recca::UKEnergy2000mats does not include "Absent_Sector" it should just be ignored
fd_sectors <- c("Residential", "Transport", "Absent_Sector")


# Test create_fd_sectors_list()
test_that("create_fd_sectors_list() works as expected", {

  # Create test data
  fd_sectors_list <- test_sutdata %>% create_fd_sectors_list(fd_sectors = fd_sectors)

  # Check object type is equal to list
  testthat::expect_type(fd_sectors_list, "list")

  # Check that the length of fd_sectors_list is equal to 4
  testthat::expect_equal(length(fd_sectors_list), 4)

  # Check that each entry in fd_sectors_list is correct
  testthat::expect_equal(unlist(fd_sectors_list), c("Residential", "Transport", "Absent_Sector",
                                                    "Residential", "Transport", "Absent_Sector",
                                                    "Residential", "Transport", "Absent_Sector",
                                                    "Residential", "Transport", "Absent_Sector"))

})


# Test get_p_industry_prefixes()
test_that("get_p_industry_prefixes() works as expected", {

  # Create test data
  p_industry_prefixes <- get_p_industry_prefixes()

  # Check object type is equal to list
  testthat::expect_type(p_industry_prefixes, "list")

  # Check that the length of p_industry_prefixes is equal to 1
  testthat::expect_equal(length(p_industry_prefixes), 1)

  # Check that each entry in p_industry_prefixes is correct
  testthat::expect_equal(unlist(p_industry_prefixes), c("Resources", "Imports", "Stock changes"))

})


