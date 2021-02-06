###########################################################
context("Metric Functions")
###########################################################

test_that("create_fd_sectors() works as expected", {

  fd_sectors <- create_fd_sectors()
  testthat::expect_type(fd_sectors, "character")

})

test_that("create_fd_sectors_list() works as expected", {

  ESP_1960 <- matrix(data = as.vector(c(7, 3, 19)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "iron and steel"), "Product"))
  ESP_1961 <- matrix(data = as.vector(c(10, 8, 15)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "iron and steel"), "Product"))
  GBR_1960 <- matrix(data = as.vector(c(12, 5, 20)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "iron and steel"), "Product"))
  GBR_1961 <- matrix(data = as.vector(c(12, 10, 10)), nrow = 3, dimnames = list(c("Construction", "Domestic aviation", "iron and steel"), "Product"))

  Y <- list(ESP_1960, ESP_1961, GBR_1960, GBR_1961)

  PSUT_DF_shell <- tibble::tribble(~Country, ~ Year,
                                   "ESP", 1960,
                                   "ESP", 1961,
                                   "GBR", 1960,
                                   "GBR", 1961)

  PSUT_DF <- tibble::tibble(PSUT_DF_shell, Y)

  fd_sectors_list <- create_fd_sectors_list(fd_sectors = fd_sectors, PSUT_DF = PSUT_DF)

  testthat::expect_type(fd_sectors_list, "list")
  testthat::expect_equal(length(fd_sectors_list), 4)

})
