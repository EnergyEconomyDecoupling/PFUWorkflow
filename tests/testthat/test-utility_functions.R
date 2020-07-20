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
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$IEAData)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    GHAdata <- readd_by_country("IEAData", country = "GHA", cache_path = testing_setup$cache_path)
    expect_equal(GHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "GHA")
    ZAFdata <- readd_by_country("IEAData", country = "ZAF", cache_path = testing_setup$cache_path)
    expect_equal(ZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 "ZAF")
    GHAZAFdata <- readd_by_country("IEAData", country = c("GHA", "ZAF"), cache_path = testing_setup$cache_path)
    expect_equal(GHAZAFdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
    ZAFGHAdata <- readd_by_country("IEAData", country = c("ZAF", "GHA"), cache_path = testing_setup$cache_path)
    expect_equal(ZAFGHAdata %>%
                   magrittr::extract2(IEATools::iea_cols$country) %>%
                   unique(),
                 c("GHA", "ZAF"))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("setup_exemplars() works as expected", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$CompletedAllocationTables,
                                                        setup_exemplars = TRUE)

  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that there is a World exemplar table
    testing_setup$plan %>%
      dplyr::filter(target == SEAPSUTWorkflow::target_names$fu_analysis_folder) %>%
      magrittr::extract2("command") %>%
      unlist() %>%
      dir.exists() %>%
      expect_true()
    # Make sure we have allocation tables.
    alloc_tables <- drake::readd(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                                 path = testing_setup$cache_path,
                                 character_only = TRUE)
    world_alloc_tables <- alloc_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "World")
    zaf_alloc_tables <- alloc_tables %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
    # World and ZAF should be the same, because World is just rebranded ZAF.
    expect_equal(world_alloc_tables %>%
                   dplyr::mutate("{IEATools::iea_cols$country}" := "ZAF"),
                 zaf_alloc_tables)
    # Extract the World FU allocation table. Make sure it matches the ZAF FU allocation table.
    ZAF_fu_allocation_table <- readd_by_country(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                                                country = "ZAF",
                                                cache_path = testing_setup$cache_path)
    World_fu_allocation_table <- readd_by_country(SEAPSUTWorkflow::target_names$IncompleteAllocationTables,
                                                  country = "World",
                                                  cache_path = testing_setup$cache_path)
    expect_equal(World_fu_allocation_table %>%
                   dplyr::mutate("{IEATools::iea_cols$country}" := "ZAF"),
                 ZAF_fu_allocation_table)

  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})
