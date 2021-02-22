# Create test data using Recca example matrices
test_sutdata <- Recca::UKEnergy2000mats %>%
  # dplyr::filter(Last.stage == "Final") %>% # Services Y rows being added as sectors
  tidyr::pivot_wider(id_cols = Country:Last.stage,
                     names_from = "matrix.name",
                     values_from = "matrix") %>%
  dplyr::mutate(Method = "PCM", .after = "Country") %>%
  dplyr::relocate(Year, .after = "Last.stage")

# Set prefixes to Resources, the only prefix in the R matrices
p_industry_prefixes <- list("Resources")

# Restrict final demand sectors to just residential, if functions work no
# transport data should be included in the calculations
fd_sectors <- c("Residential")


# Run tests
test_that("create_fd_sectors_list() works as expected", {

  # Create test data
  fd_sectors_list <- test_sutdata %>% create_fd_sectors_list(fd_sectors = fd_sectors)

  # Check object type is equal to list
  testthat::expect_type(fd_sectors_list, "list")

  # Check that fd_sectors_list is equal to 4
  testthat::expect_equal(length(fd_sectors_list), 4)

  # Check that each entry in fd_sectors_list is composed of just "Residential"
  testthat::expect_equal(unlist(fd_sectors_list), c("Residential", "Residential", "Residential", "Residential"))

})

test_that("calculate_fu_ex_total() works as expected", {

  # Create test data
  fu_total <- calculate_fu_ex_total(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  # Check object type is equal to list
  testthat::expect_type(fu_total, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(fu_total), c("Country", "Method", "Energy.type",
                                               "Stage", "Gross.Net", "Product",
                                               "Sector", "Grouping", "Year", "EX"))

  # Check that there are 8 rows of observations, composed of gross and net values for
  # each of the three stages in E terms, and one stage in X terms
  testthat::expect_equal(nrow(fu_total), 8)
  testthat::expect_equal(unique(fu_total$Stage), c("Final", "Services", "Useful"))
  testthat::expect_equal(unique(fu_total$Energy.type), c("E", "X"))

})

test_that("calculate_fu_ex_sector() works as expected", {

  # Create test data
  fu_sector <- calculate_fu_ex_sector(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  # Check object type is equal to list
  testthat::expect_type(fu_sector, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(fu_sector), c("Country", "Method", "Energy.type",
                                                "Stage", "Gross.Net", "Product",
                                                "Sector", "Grouping", "Year", "EX"))

  # Check that the only final demand sector is "Residential" as stipulated in fd_sectors
  ##### Currently taking each row for Y and U_EIOU matrices for gross final demand only #####
  testthat::expect_equal(unique(fu_sector$Sector), c("Residential"))

})

test_that("calculate_fu_ex_product() works as expected", {

  # Create test data
  fu_product <- calculate_fu_ex_product(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  # Check object type is equal to list
  testthat::expect_type(fu_product, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(fu_product), c("Country", "Method", "Energy.type",
                                                 "Stage", "Gross.Net", "Product",
                                                 "Sector", "Grouping", "Year", "EX"))
  # Check that there is more than one product
  testthat::expect_true(length(unique(fu_product$Product)) > 1)

})

test_that("calculate_p_ex_total() works as expected", {

  # Create test data
  p_total <- calculate_p_ex_total(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  # Check object type is equal to list
  testthat::expect_type(p_total, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(p_total), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Flow", "Grouping", "Year", "EX"))

  # Check that there are two rows. One for the energy quantification E and one for X
  testthat::expect_equal(nrow(p_total), 2)
  testthat::expect_equal(unique(p_total$Energy.type), c("E", "X"))

})

test_that("calculate_p_ex_flow() works as expected", {

  # Create test data
  p_flow <- calculate_p_ex_flow(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  # Check object type is equal to list
  testthat::expect_type(p_flow, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(p_flow), c("Country", "Method", "Energy.type",
                                             "Stage", "Gross.Net", "Product",
                                             "Flow", "Grouping", "Year", "EX"))
  # Check that there are two Flows, and that those flows are:
  # 1) Resources - Crude, and 2) Resources - NG
  testthat::expect_equal(length(unique(p_flow$Flow)), 2)
  testthat::expect_equal(unique(p_flow$Flow), c("Resources - Crude", "Resources - NG"))

})

test_that("calculate_p_ex_product() works as expected", {

  # Create test data
  p_product <- calculate_p_ex_product(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  # Check object type is equal to list
  testthat::expect_type(p_product, "list")

  # Check column names are correct
  testthat::expect_equal(colnames(p_product), c("Country", "Method", "Energy.type",
                                                "Stage", "Gross.Net", "Product",
                                                "Flow", "Grouping", "Year", "EX"))

  # Check that there are two products, and that those Products are:
  # 1) Crude, and 2) NG
  testthat::expect_equal(length(unique(p_product$Product)), 2)
  testthat::expect_equal(unique(p_product$Product), c("Crude", "NG"))

})

test_that("calculate_all_ex_data() works as expected", {

  # Create test data
  all_data <- calculate_all_ex_data(.sutdata = test_sutdata,
                                    fd_sectors = fd_sectors,
                                    p_industry_prefixes = p_industry_prefixes)

  # Check column names are correct
  testthat::expect_equal(colnames(all_data), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Flow.Sector", "Grouping", "Year", "EX"))
  # Check that there are two energy types, E and X
  testthat::expect_equal(unique(all_data$Energy.type), c("E", "X"))

  # Check that there are four Stages: Final, Services, Useful, Primary
  testthat::expect_equal(unique(all_data$Stage), c("Final", "Services", "Useful", "Primary"))

  # Check that there are four grouping variables: Total, Sector, Product, and Flow
  testthat::expect_equal(unique(all_data$Grouping), c("Total", "Sector", "Product", "Flow"))

  # Check that there are two Gross.Net values: Net and Gross
  testthat::expect_equal(unique(all_data$Gross.Net), c("Net", "Gross"))

})
