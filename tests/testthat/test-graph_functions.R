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
    alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")

  expect_true(!is.null(g))
  expect_true(inherits(g, "ggplot"))
})


test_that("alloc_plots_df() works as expected", {
  alloc_table <- alloc_table <- tibble::tribble(~Country, ~Method, ~Energy.type, ~Year, ~Ef.product, ~Destination,
                                                ~.values, ~Machine, ~Quantity, ~Eu.product, ~C.source,
                                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
                                                0.2, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
                                                0.8, "Trucks", "C_2 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World")
  plots_df <- alloc_plots_df(alloc_table, countries = c("GHA", "ZAF"))

  expect_true(!is.null(plots_df))
  expect_true(inherits(plots_df$Plots[[1]], "ggplot"))
  expect_true(inherits(plots_df$Plots[[2]], "ggplot"))
})
