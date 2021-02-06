###########################################################
context("Metric Functions")
###########################################################

test_that("create_fd_sectors() works as expected", {

  fd_sectors <- create_fd_sectors()
  testthat::expect_type(fd_sectors, "character")

})

test_that("create_fd_sectors_list() works as expected", {

  Y_ESP_1960 <- matrix(data = as.vector(c(7, 3, 19)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "Iron and steel"), "Product"))
  Y_ESP_1961 <- matrix(data = as.vector(c(10, 8, 15)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "Iron and steel"), "Product"))
  Y_GBR_1960 <- matrix(data = as.vector(c(12, 5, 20)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "Iron and steel"), "Product"))
  Y_GBR_1961 <- matrix(data = as.vector(c(12, 10, 10)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "Iron and steel"), "Product"))

  Y <- list(Y_ESP_1960, Y_ESP_1961, Y_GBR_1960, Y_GBR_1961)

  PSUT_DF_shell <- tibble::tribble(~Country, ~ Year,
                                   "ESP", 1960,
                                   "ESP", 1961,
                                   "GBR", 1960,
                                   "GBR", 1961)

  PSUT_DF <- tibble::tibble(PSUT_DF_shell, Y)

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
