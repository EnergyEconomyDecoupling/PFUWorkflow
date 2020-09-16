###########################################################
context("Graph Functions")
###########################################################

# This tests the function alloc_graph()
test_that("alloc_graph() works", {
  # Make a simple data frame with the expected structure.
  g <- tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
                  1967, 0.5, "Cars", "MD",
                  1967, 0.5, "Industry static engines", "MD",
                  2020, 0.8, "Cars", "MD",
                  2020, 0.2, "Industry static engines", "MD") %>%
    alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")

  expect_true(!is.null(g))
  expect_true(inherits(g, "ggplot"))
})


test_that("alloc_plots_df() works as expected", {
  alloc_table <- tibble::tribble(~Country, ~Year, ~Ef.product, ~Destination,
                                 ~.values, ~Machine, ~Eu.product,
                                 "GHA", 1971, "Gasoline", "Transport", 0.5, "Cars", "MD",
                                 "GHA", 1971, "Gasoline", "Transport", 0.5, "Trucks", "MD",
                                 "GHA", 2020, "Gasoline", "Transport", 0.2, "Cars", "MD",
                                 "GHA", 2020, "Gasoline", "Transport", 0.8, "Trucks", "MD",
                                 "ZAF", 1971, "Gasoline", "Transport", 0.5, "Cars", "MD",
                                 "ZAF", 1971, "Gasoline", "Transport", 0.5, "Trucks", "MD",
                                 "ZAF", 2020, "Gasoline", "Transport", 0.3, "Cars", "MD",
                                 "ZAF", 2020, "Gasoline", "Transport", 0.7, "Trucks", "MD")
  plots_df <- alloc_plots_df(alloc_table, countries = c("GHA", "ZAF"))

  expect_true(!is.null(plots_df))
  expect_true(inherits(plots_df$plots[[1]], "ggplot"))
  expect_true(inherits(plots_df$plots[[2]], "ggplot"))
})

###########################################################################################################

# This tests the function eta_fu_graph()
test_that("eta_fu_graph() works", {
  # Make a simple data frame with the expected structure.
  h <- tibble::tribble(~Country, ~Year, ~.values, ~Machine, ~Eu.product,
                       "ESP", 1967, 0.5, "Cars", "MD",
                       "MEX", 1967, 0.6, "Cars", "MD",
                       "ESP", 2020, 0.7, "Cars", "MD",
                       "MEX", 2020, 0.8, "Cars", "MD") %>%
    eta_fu_graph(country = c("ESP", "MEX"), machine = "Cars", eu_product = "MD")

  expect_true(!is.null(h))
  expect_true(inherits(h, "ggplot"))
})


test_that("eta_fu_plots_df() works as expected", {
  eta_fu_table <- tibble::tribble(~Country, ~Year, ~.values, ~Machine, ~Eu.product,
                                 "ESP", 1971, 0.4, "Cars", "MD",
                                 "ESP", 1971, 0.5, "Trucks", "MD",
                                 "ESP", 2020, 0.6, "Cars", "MD",
                                 "ESP", 2020, 0.7, "Trucks", "MD",
                                 "MEX", 1971, 0.3, "Cars", "MD",
                                 "MEX", 1971, 0.4, "Trucks", "MD",
                                 "MEX", 2020, 0.4, "Cars", "MD",
                                 "MEX", 2020, 0.5, "Trucks", "MD")
  plots_eta_df <- eta_fu_plots_df(eta_fu_table, countries = c("ESP", "MEX"))

  expect_true(!is.null(plots_eta_df))
  expect_true(inherits(plots_eta_dff$plots[[1]], "ggplot"))
  expect_true(inherits(plots_eta_df$plots[[2]], "ggplot"))
})
