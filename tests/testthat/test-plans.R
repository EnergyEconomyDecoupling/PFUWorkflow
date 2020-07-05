###########################################################
context("Plan Functions")
###########################################################

# # Helper function for setting up the FU analysis and FU etas Excel workbooks
# set_up_temp_fu_analyses <- function(fu_folder) {
#   # Set up FU allocation tables. Need to split file that is stored in IEATools
#   GHAZAF_FU_allocation_tables <- IEATools::sample_fu_allocation_table_path() %>%
#     openxlsx::read.xlsx()
#   GHA_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
#     dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
#   ZAF_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
#     dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
#   # Set up eta FU tables. Need to split file that is stored in IEATools.
#   GHAZAF_eta_FU_tables <- IEATools::sample_eta_fu_table_path() %>%
#     openxlsx::read.xlsx()
#   GHA_FU_etas_table <- GHAZAF_eta_FU_tables %>%
#     dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
#   ZAF_FU_etas_table <- GHAZAF_eta_FU_tables %>%
#     dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
#
#   # Build FU analysis workbooks for each country.
#   GHA_fu_wb <- openxlsx::createWorkbook()
#   openxlsx::addWorksheet(GHA_fu_wb, "FU Allocations")
#   openxlsx::writeData(GHA_fu_wb, "FU Allocations", GHA_FU_allocation_table)
#   openxlsx::addWorksheet(GHA_fu_wb, "FU etas")
#   openxlsx::writeData(GHA_fu_wb, "FU etas", GHA_FU_etas_table)
#
#   ZAF_fu_wb <- openxlsx::createWorkbook()
#   openxlsx::addWorksheet(ZAF_fu_wb, "FU Allocations")
#   openxlsx::writeData(ZAF_fu_wb, "FU Allocations", ZAF_FU_etas_table)
#   openxlsx::addWorksheet(ZAF_fu_wb, "FU etas")
#   openxlsx::writeData(ZAF_fu_wb, "FU etas", ZAF_FU_etas_table)
#
#   # Write the workbooks back to the temp directory.
#   dir.create(file.path(fu_folder, "GHA"), showWarnings = FALSE)
#   dir.create(file.path(fu_folder, "ZAF"), showWarnings = FALSE)
#   openxlsx::saveWorkbook(GHA_fu_wb, file = file.path(fu_folder, "GHA", "GHA FU Analysis.xlsx"), overwrite = TRUE)
#   openxlsx::saveWorkbook(ZAF_fu_wb, file = file.path(fu_folder, "ZAF", "ZAF FU Analysis.xlsx"), overwrite = TRUE)
# }



test_that("get_plan works", {
  my_plan <- get_plan(iea_data_path = "datapath",
                      exemplar_table_path = "exemplarpath",
                      fu_analysis_folder = "FUpath",
                      countries = c("GHA", "ZAF"),
                      max_year = 1999)

  # Make sure the pieces get created correctly with tidyeval.
  expect_equal(my_plan[[1, "target"]], "countries")
  expect_equal(my_plan[[2, "target"]], "max_year")
  expect_equal(my_plan[[2, "command"]], list(1999))
  expect_equal(my_plan[[3, "command"]], list("datapath"))
  expect_equal(my_plan[[4, "command"]], list("exemplarpath"))
  expect_equal(my_plan[[5, "target"]], "fu_analysis_folder")
  expect_equal(my_plan[[5, "command"]], list("FUpath"))
  expect_equal(my_plan[[6, "target"]], "AllIEAData")
  expect_equal(my_plan[[7, "target"]], "IEAData")
  expect_equal(my_plan[[8, "target"]], "balanced_before")
  expect_equal(my_plan[[9, "target"]], "BalancedIEAData")
  expect_equal(my_plan[[10, "target"]], "balanced_after")
  expect_equal(my_plan[[11, "target"]], "OKToProceed")
  expect_equal(my_plan[[12, "target"]], "Specified")
  expect_equal(my_plan[[13, "target"]], "PSUT_final")
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
  cache_path <- tempfile("drake_cache_for_testing")
  fu_folder <- tempdir("FU_folder_for_testing")
  plan <- get_plan(countries = c("GHA", "ZAF"),
                   max_year = 2000,
                   iea_data_path = IEATools::sample_iea_data_path(),
                   exemplar_table_path = sample_exemplar_table_path(),
                   fu_analysis_folder = fu_folder)

  # Set up FU allocation and efficiency tables in the temporary directory
  set_up_temp_fu_analyses(fu_folder)


  tryCatch({
    # Create a fake drake plan
    temp_cache <- drake::new_cache(path = cache_path)

    # Make the plan in the temp_cache
    drake::make(plan, cache = temp_cache, verbose = 0)

    # Now readd data from the cache.
    expect_equal(drake::readd(target = countries, path = cache_path), c("GHA", "ZAF"))
    expect_equal(drake::readd(target = max_year, path = cache_path), 2000)
    expect_equal(drake::readd(target = iea_data_path, path = cache_path), IEATools::sample_iea_data_path())
    expect_equal(drake::readd(target = exemplar_table_path, path = cache_path), sample_exemplar_table_path())
    expect_equal(drake::readd(target = fu_analysis_folder, path = cache_path), fu_folder)

    # Be sure that readd_by_country also works
    expected_all_iea_data <- IEATools::sample_iea_data_path() %>%
      IEATools::load_tidy_iea_df()
    # Read directly out of the cache.
    expect_equal(drake::readd(target = AllIEAData, path = cache_path), expected_all_iea_data)
    # Read GHA IEA data from the cache
    expected_GHA_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "GHA")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "GHA", cache_path = cache_path), expected_GHA_data)
    # Read ZAF IEA data from the cache
    expected_ZAF_data <- expected_all_iea_data %>%
      dplyr::filter(Country == "ZAF")
    # Compare to readd_by_country
    expect_equal(readd_by_country(target = "IEAData", country = "ZAF", cache_path = cache_path), expected_ZAF_data)

    # Check energy balance information
    expect_equal(drake::readd(balanced_before, path = cache_path), c(FALSE, FALSE))
    expect_equal(drake::readd(balanced_after, path = cache_path), c(TRUE, TRUE))
    expect_true(drake::readd(OKToProceed, path = cache_path))

    # Check specification process, reading by country.
    expect_true(!is.null(readd_by_country(target = "Specified", country = "GHA", cache_path = cache_path)))

    # Check making PSUT matrices, reading by country.
    expect_true(!is.null(readd_by_country(target = "PSUT_final", country = "ZAF", cache_path = cache_path)))

    # Test allocation tables.
    expect_true(!is.null(readd_by_country(target = "IncompleteAllocationTables", country = "GHA", cache_path = cache_path)))
    expect_true(!is.null(readd_by_country(target = "IncompleteAllocationTables", country = "ZAF", cache_path = cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "IncompleteAllocationTables", country = "GHA", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "IncompleteAllocationTables", country = "ZAF", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")

    # Test efficiency tables.
    expect_true(!is.null(readd_by_country(target = "IncompleteEfficiencyTables", country = "GHA", cache_path = cache_path)))
    expect_true(!is.null(readd_by_country(target = "IncompleteEfficiencyTables", country = "ZAF", cache_path = cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "IncompleteEfficiencyTables", country = "GHA", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "IncompleteEfficiencyTables", country = "ZAF", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")

    # Ensure that ExemplarLists were read correctly.
    expect_true(!is.null(readd_by_country(target = "ExemplarLists", country = "GHA", cache_path = cache_path)))
    expect_true(!is.null(readd_by_country(target = "ExemplarLists", country = "ZAF", cache_path = cache_path)))
    # Check that each country is in the right target
    readd_by_country(target = "ExemplarLists", country = "GHA", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("GHA")
    readd_by_country(target = "ExemplarLists", country = "ZAF", cache_path = cache_path) %>%
      dplyr::select(IEATools::iea_cols$country) %>%
      unique() %>% unlist() %>% unname() %>%
      expect_equal("ZAF")


  },
  finally = {
    # Clean up the cache.
    temp_cache$destroy()
    # Delete the folder
    unlink(fu_folder, recursive = TRUE, force = TRUE)
  })
})
