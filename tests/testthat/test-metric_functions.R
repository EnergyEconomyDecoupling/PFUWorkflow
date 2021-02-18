###########################################################
context("Metric Functions")
###########################################################

# Load required packages
library(testthat)
library(Recca)
library(tidyr)

# Create test data using Recca example matrices
PSUT_DF <- Recca::UKEnergy2000mats %>%
  tidyr::pivot_wider(id_cols = Country:Last.stage,
                     names_from = "matrix.name",
                     values_from = "matrix") %>%
  dplyr::mutate(Method = "PCM", .after = "Country") %>%
  dplyr::relocate(Year, .after = "Last.stage")


test_that("create_fd_sectors() works as expected", {

  fd_sectors <- create_fd_sectors()
  testthat::expect_type(fd_sectors, "character")

})

test_that("create_fd_sectors_list() works as expected", {

  fd_sectors_list <- PSUT_DF %>% create_fd_sectors_list(fd_sectors = create_fd_sectors())

  testthat::expect_type(fd_sectors_list, "list")
  testthat::expect_equal(length(fd_sectors_list), 4)

})

test_that("create_p_industry_prefixes() works as expected", {

  p_industry_prefixes <- create_p_industry_prefixes()

  p_industry_prefixes_chr <- p_industry_prefixes %>% unlist()

  testthat::expect_type(p_industry_prefixes, "list")
  testthat::expect_equal(p_industry_prefixes_chr, c("Resources", "Production",
                                                    "Imports", "Exports",
                                                    "International marine bunkers",
                                                    "International aviation bunkers",
                                                    "Stock changes"))
})

test_that("calculate_fu_ex_total() works as expected", {

  fu_total <- PSUT_DF %>% calculate_fu_ex_total()

  testthat::expect_type(fu_total, "list")
  testthat::expect_equal(colnames(fu_total), c("Country", "Method", "Energy.type",
                                               "Stage", "Gross.Net", "Product",
                                               "Sector", "Year", "EX"))
})

test_that("calculate_fu_ex_sector() works as expected", {

  fu_sector <- PSUT_DF %>% calculate_fu_ex_sector()

  testthat::expect_type(fu_sector, "list")
  testthat::expect_equal(colnames(fu_sector), c("Country", "Method", "Energy.type",
                                             "Stage", "Gross.Net", "Product",
                                             "Sector", "Year", "EX"))
  testthat::expect_true(length(unique(fu_sector$Sector)) > 1)

})

test_that("calculate_fu_ex_product() works as expected", {

  fu_product <- PSUT_DF %>% calculate_fu_ex_product()

  testthat::expect_type(fu_product, "list")
  testthat::expect_equal(colnames(fu_product), c("Country", "Method", "Energy.type",
                                                 "Stage", "Gross.Net", "Product",
                                                 "Sector", "Year", "EX"))
  testthat::expect_true(length(unique(fu_product$Product)) > 1)

})

test_that("calculate_p_ex_total() works as expected", {

  p_total <- PSUT_DF %>% calculate_p_ex_total()

  testthat::expect_type(p_total, "list")
  testthat::expect_equal(colnames(p_total), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Flow", "Year", "EX"))

})

test_that("calculate_p_ex_flow() works as expected", {

  p_flow <- PSUT_DF %>% calculate_p_ex_flow()

  testthat::expect_type(p_flow, "list")
  testthat::expect_equal(colnames(p_flow), c("Country", "Method", "Energy.type",
                                             "Stage", "Gross.Net", "Product",
                                             "Flow", "Year", "EX"))
  testthat::expect_true(length(unique(p_flow$Flow)) > 1)

})

test_that("calculate_p_ex_product() works as expected", {

  p_product <- PSUT_DF %>% calculate_p_ex_product()

  testthat::expect_type(p_product, "list")
  testthat::expect_equal(colnames(p_product), c("Country", "Method", "Energy.type",
                                                "Stage", "Gross.Net", "Product",
                                                "Flow", "Year", "EX"))
  testthat::expect_true(length(unique(p_product$Product)) > 1)

})

test_that("calculate_all_ex_data() works as expected", {

  all_data <- PSUT_DF %>% calculate_all_ex_data()

  testthat::expect_equal(colnames(all_data), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Sector", "Year", "EX"))
  # testthat::expect_equal(unique(all_data$Stage), c("Final", "Primary"))
  testthat::expect_equal(unique(all_data$Gross.Net), c("Net", "Gross"))

})
