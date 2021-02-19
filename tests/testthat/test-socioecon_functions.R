###########################################################
context("Socio-economic Data")
###########################################################

test_that("get_all_pwt_data() works correctly", {

  countries <- c("USA", "GHA", "HND")

  pwt10_data <- get_all_pwt_data(countries = countries)

  testthat::expect_equal(colnames(pwt10_data), colnames(pwt10::pwt10.0))
  testthat::e



})
