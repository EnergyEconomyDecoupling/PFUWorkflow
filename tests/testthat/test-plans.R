###########################################################
context("Plan Functions")
###########################################################

test_that("get_plan works", {
  myplan <- get_plan(iea_data_path = "mypath", countries = c("GHA", "ZAF"), max_year = 1999)
  expect_equal(myplan[[1, 1]], "ieadp")
  expect_equal(myplan[[2, 1]], "ctrys")
  expect_equal(myplan[[3, 1]], "maxyr")
  expect_equal(myplan[[4, 1]], "AllIEAData")
  expect_equal(myplan[[5, 1]], "IEAData")
})
