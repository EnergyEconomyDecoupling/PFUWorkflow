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

  fd_sectors_list <- test_sutdata %>% create_fd_sectors_list(fd_sectors = fd_sectors)

  testthat::expect_type(fd_sectors_list, "list")
  testthat::expect_equal(length(fd_sectors_list), 4)

})

test_that("create_p_industry_prefixes() works as expected", {

  p_industry_prefixes <- p_industry_prefixes

  p_industry_prefixes_chr <- p_industry_prefixes %>% unlist()

  testthat::expect_type(p_industry_prefixes, "list")
  testthat::expect_equal(p_industry_prefixes_chr, c("Resources"))
})

test_that("calculate_fu_ex_total() works as expected", {

  fu_total <- calculate_fu_ex_total(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  testthat::expect_type(fu_total, "list")
  testthat::expect_equal(colnames(fu_total), c("Country", "Method", "Energy.type",
                                               "Stage", "Gross.Net", "Product",
                                               "Sector", "Grouping", "Year", "EX"))
})

test_that("calculate_fu_ex_sector() works as expected", {

  fu_sector <- calculate_fu_ex_sector(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  testthat::expect_type(fu_sector, "list")
  testthat::expect_equal(colnames(fu_sector), c("Country", "Method", "Energy.type",
                                                "Stage", "Gross.Net", "Product",
                                                "Sector", "Grouping", "Year", "EX"))
  testthat::expect_equal(unique(fu_sector$Sector), c("Residential"))
  # Currently taking each row for Y and U_EIOU matrices for gross final demand only

})

test_that("calculate_fu_ex_product() works as expected", {

  fu_product <- calculate_fu_ex_product(.sutdata = test_sutdata, fd_sectors = fd_sectors)

  testthat::expect_type(fu_product, "list")
  testthat::expect_equal(colnames(fu_product), c("Country", "Method", "Energy.type",
                                                 "Stage", "Gross.Net", "Product",
                                                 "Sector", "Grouping", "Year", "EX"))
  testthat::expect_true(length(unique(fu_product$Product)) > 1)

})

test_that("calculate_p_ex_total() works as expected", {

  p_total <- calculate_p_ex_total(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  testthat::expect_type(p_total, "list")
  testthat::expect_equal(colnames(p_total), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Flow", "Grouping", "Year", "EX"))

})

test_that("calculate_p_ex_flow() works as expected", {

  p_flow <- calculate_p_ex_flow(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  testthat::expect_type(p_flow, "list")
  testthat::expect_equal(colnames(p_flow), c("Country", "Method", "Energy.type",
                                             "Stage", "Gross.Net", "Product",
                                             "Flow", "Grouping", "Year", "EX"))
  testthat::expect_true(length(unique(p_flow$Flow)) > 1)

})

test_that("calculate_p_ex_product() works as expected", {

  p_product <- calculate_p_ex_product(.sutdata = test_sutdata, p_industry_prefixes = p_industry_prefixes)

  testthat::expect_type(p_product, "list")
  testthat::expect_equal(colnames(p_product), c("Country", "Method", "Energy.type",
                                                "Stage", "Gross.Net", "Product",
                                                "Flow", "Grouping", "Year", "EX"))
  testthat::expect_true(length(unique(p_product$Product)) > 1)

})

test_that("calculate_all_ex_data() works as expected", {

  all_data <- calculate_all_ex_data(.sutdata = test_sutdata,
                                    fd_sectors = fd_sectors,
                                    p_industry_prefixes = p_industry_prefixes)

  testthat::expect_equal(colnames(all_data), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Flow.Sector", "Grouping", "Year", "EX"))
  testthat::expect_equal(unique(all_data$Energy.type), c("E", "X"))
  testthat::expect_equal(unique(all_data$Stage), c("Final", "Services", "Useful", "Primary"))
  testthat::expect_equal(unique(all_data$Grouping), c("Total", "Sector", "Product", "Flow"))
  testthat::expect_equal(unique(all_data$Gross.Net), c("Net", "Gross"))

})
