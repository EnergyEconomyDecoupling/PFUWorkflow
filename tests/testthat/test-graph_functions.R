###########################################################
context("Graph Functions")
###########################################################

test_that("alloc_graph() works", {
  # Make a simple data frame with the expected structure.
  g <- tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
                  1967, 0.5, "Cars", "MD",
                  1967, 0.5, "Industry static engines", "MD",
                  2020, 0.8, "Cars", "MD",
                  2020, 0.2, "Industry static engines", "MD") %>%
    alloc_graph()

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
  plots_df <- alloc_plots_df(.df = alloc_table)

  expect_true(!is.null(plots_df))
  expect_true(inherits(plots_df$plots[[1]], "ggplot"))
  expect_true(inherits(plots_df$plots[[2]], "ggplot"))
})
