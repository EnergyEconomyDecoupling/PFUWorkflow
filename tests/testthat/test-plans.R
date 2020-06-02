###########################################################
context("Plan Functions")
###########################################################

test_that("get_plan works", {
  myplan <- get_plan(iea_data_path = "mypath", countries = c("GHA", "ZAF"), max_year = 1999)
  expect_equal(myplan[[1, "target"]], "ieadp")
  expect_equal(myplan[[2, "target"]], "ctrys")
  expect_equal(myplan[[3, "target"]], "maxyr")
  expect_equal(myplan[[4, "target"]], "AllIEAData")
  expect_equal(myplan[[5, "target"]], "IEAData")

  # Make sure the pieces get created correctly with tidyeval.
  expect_equal(myplan[[1, "command"]], list("mypath"))
  expect_equal(myplan[[2, "command"]], list(c("GHA", "ZAF")))
  expect_equal(myplan[[3, "command"]], list(1999))
})
