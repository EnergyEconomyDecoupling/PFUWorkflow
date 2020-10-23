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

#####################################################################################################################

# This tests the function nonstat_alloc_graph()
test_that("nonstat_alloc_graph() works", {
  # Make a simple data frame with the expected structure.
  i <- tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
                       1967, 0.5, "Cars", "MD",
                       1967, 0.5, "Industry static engines", "MD",
                       2020, 0.8, "Cars", "MD",
                       2020, 0.2, "Industry static engines", "MD") %>%
    nonstat_alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")

  expect_true(!is.null(i))
  expect_true(inherits(i, "ggplot"))
})


test_that("nonstat_alloc_plots_df() works as expected", {
  non_stat_alloc_table  <- tibble::tribble(~Country, ~Method, ~Energy.type, ~Year, ~Ef.product, ~Destination,
                                                ~.values, ~Machine, ~Quantity, ~Eu.product, ~C.source,
                                                # Non-stationary data
                                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "Gasoline", "Transport",
                                                0.2, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "Gasoline", "Transport",
                                                0.8, "Trucks", "C_2 [%]", "MD", "World",

                                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "Gasoline", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "Gasoline", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World",

                                                # Stationary data (should be excluded by nonstat_alloc_plots_df())
                                                "GHA", "PCM", "E", 1971, "Diesel", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 1971, "Diesel", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "Diesel", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "Diesel", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",

                                                "ZAF", "PCM", "E", 1971, "Diesel", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 1971, "Diesel", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "Diesel", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "Diesel", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World",

                                                # Stationary data with different 2015 and 2020 c_source (ESP).
                                                # Should be excluded by nonstat_alloc_plots_df() as .values are static
                                                # even though they are different to .values in 1971 and 2009 with
                                                # c_source = World
                                                "GHA", "PCM", "E", 1971, "LPG", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 1971, "LPG", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "LPG", "Transport",
                                                0.5, "Cars", "C_1 [%]", "MD", "World",
                                                "GHA", "PCM", "E", 2009, "LPG", "Transport",
                                                0.5, "Trucks", "C_2 [%]", "MD", "World",

                                                "GHA", "PCM", "E", 2015, "LPG", "Transport",
                                                0.4, "Cars", "C_1 [%]", "MD", "ESP",
                                                "GHA", "PCM", "E", 2015, "LPG", "Transport",
                                                0.6, "Trucks", "C_2 [%]", "MD", "ESP",
                                                "GHA", "PCM", "E", 2020, "LPG", "Transport",
                                                0.4, "Cars", "C_1 [%]", "MD", "ESP",
                                                "GHA", "PCM", "E", 2020, "LPG", "Transport",
                                                0.6, "Trucks", "C_2 [%]", "MD", "ESP",

                                                "ZAF", "PCM", "E", 1971, "LPG", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 1971, "LPG", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "LPG", "Transport",
                                                0.3, "Cars", "C_1 [%]", "MD", "World",
                                                "ZAF", "PCM", "E", 2009, "LPG", "Transport",
                                                0.7, "Trucks", "C_2 [%]", "MD", "World",

                                                "ZAF", "PCM", "E", 1971, "LPG", "Transport",
                                                0.2, "Cars", "C_1 [%]", "MD", "ESP",
                                                "ZAF", "PCM", "E", 1971, "LPG", "Transport",
                                                0.8, "Trucks", "C_2 [%]", "MD", "ESP",
                                                "ZAF", "PCM", "E", 2009, "LPG", "Transport",
                                                0.2, "Cars", "C_1 [%]", "MD", "ESP",
                                                "ZAF", "PCM", "E", 2009, "LPG", "Transport",
                                                0.8, "Trucks", "C_2 [%]", "MD", "ESP"
                                                )

  plots_df <- nonstat_alloc_plots_df(non_stat_alloc_table, countries = c("GHA", "ZAF"))

  expect_true(!is.null(plots_df))
  expect_true(unique(plots_df$Ef.product) == "Gasoline")
  expect_true(inherits(plots_df$Plots[[1]], "ggplot"))
  expect_true(inherits(plots_df$Plots[[2]], "ggplot"))
})

###########################################################################################################

# This tests the function eta_fu_graph()
test_that("eta_fu_graph() works", {

  # Make a simple data frame with the expected structure.
  h <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                       "ESP", 1967, "eta.fu", 0.5, "Cars", "MD",
                       "MEX", 1967, "eta.fu", 0.6, "Cars", "MD",
                       "ESP", 2009, "eta.fu", 0.7, "Cars", "MD",
                       "MEX", 2009, "eta.fu", 0.8, "Cars", "MD",
                       "ESP", 1967, "phi.u", 0.5, "Cars", "MD",
                       "MEX", 1967, "phi.u", 0.6, "Cars", "MD",
                       "ESP", 2009, "phi.u", 0.7, "Cars", "MD",
                       "MEX", 2009, "phi.u", 0.8, "Cars", "MD") %>%

    eta_fu_graph(countries = c("ESP", "MEX"))

  expect_true(!is.null(h))
  expect_true(inherits(h, "ggplot"))

  bad_df <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                            "ESP", 1967, "eta.fu", 0.5, "Cars", "MD",
                            "MEX", 1967, "eta.fu", 0.6, "Cars", "MD",
                            "ESP", 2009, "eta.fu", 0.7, "Cars", "MD",
                            "MEX", 2009, "eta.fu", 0.8, "Trucks", "MD",
                            "ESP", 1967, "phi.u", 1.0, "Cars", "MD",
                            "MEX", 1967, "phi.u", 1.0, "Cars", "MD",
                            "ESP", 2009, "phi.u", 1.0, "Cars", "MD",
                            "MEX", 2009, "phi.u", 1.0, "Trucks", "MD")

  expect_error(eta_fu_graph(bad_df, countries = c("ESP", "MEX")), regexp = "Found more than 1 machine in eta_fu_graph()")
})


test_that("eta_fu_plots_df() works as expected", {
  eta_fu_table <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                                  "ESP", 1971, "eta.fu", 0.4, "Cars", "MD",
                                  "ESP", 1971, "eta.fu", 0.5, "Trucks", "MD",
                                  "ESP", 2009, "eta.fu", 0.6, "Cars", "MD",
                                  "ESP", 2009, "eta.fu", 0.7, "Trucks", "MD",
                                  "ESP", 1971, "phi.u", 1.0, "Cars", "MD",
                                  "ESP", 1971, "phi.u", 1.0, "Trucks", "MD",
                                  "ESP", 2009, "phi.u", 1.0, "Cars", "MD",
                                  "ESP", 2009, "phi.u", 1.0, "Trucks", "MD",
                                  "MEX", 1971, "eta.fu", 0.3, "Cars", "MD",
                                  "MEX", 1971, "eta.fu", 0.4, "Trucks", "MD",
                                  "MEX", 2009, "eta.fu", 0.4, "Cars", "MD",
                                  "MEX", 2009, "eta.fu", 0.5, "Trucks", "MD",
                                  "MEX", 1971, "phi.u", 1.0, "Cars", "MD",
                                  "MEX", 1971, "phi.u", 1.0, "Trucks", "MD",
                                  "MEX", 2009, "phi.u", 1.0, "Cars", "MD",
                                  "MEX", 2009, "phi.u", 1.0, "Trucks", "MD",
                                  "ESP", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas")

  plots_eta_df <- eta_fu_plots_df(eta_fu_table, countries = c("ESP", "MEX"))

  expect_true(!is.null(plots_eta_df))
  expect_true(inherits(plots_eta_df$Plots[[1]], "ggplot"))
  expect_true(inherits(plots_eta_df$Plots[[2]], "ggplot"))
})

###########################################################################################################

# This tests the function phi_u_graph()
test_that("phi_u_graph() works", {

  # Make a simple data frame with the expected structure.
  g <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                       "ESP", 1967, "eta.fu", 0.5, "Cars", "MD",
                       "MEX", 1967, "eta.fu", 0.6, "Cars", "MD",
                       "ESP", 2009, "eta.fu", 0.7, "Cars", "MD",
                       "MEX", 2009, "eta.fu", 0.8, "Cars", "MD",
                       "ESP", 1967, "phi.u", 0.5, "Cars", "MD",
                       "MEX", 1967, "phi.u", 0.6, "Cars", "MD",
                       "ESP", 2009, "phi.u", 0.7, "Cars", "MD",
                       "MEX", 2009, "phi.u", 0.8, "Cars", "MD") %>%

    phi_u_graph(countries = c("ESP", "MEX"))

  expect_true(!is.null(g))
  expect_true(inherits(g, "ggplot"))

  bad_phi_df <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                            "ESP", 1967, "eta.fu", 0.5, "Cars", "MD",
                            "MEX", 1967, "eta.fu", 0.6, "Cars", "MD",
                            "ESP", 2009, "eta.fu", 0.7, "Cars", "MD",
                            "MEX", 2009, "eta.fu", 0.8, "Trucks", "MD",
                            "ESP", 1967, "phi.u", 1.0, "Cars", "MD",
                            "MEX", 1967, "phi.u", 1.0, "Cars", "MD",
                            "ESP", 2009, "phi.u", 1.0, "Cars", "MD",
                            "MEX", 2009, "phi.u", 1.0, "Trucks", "MD")

  expect_error(phi_u_graph(bad_phi_df, countries = c("ESP", "MEX")), regexp = "Found more than 1 machine in phi_u_graph()")
})


test_that("phi_u_plots_df() works as expected", {
  phi_u_table <- tibble::tribble(~Country, ~Year, ~Quantity, ~.values, ~Machine, ~Eu.product,
                                  "ESP", 1971, "eta.fu", 0.4, "Cars", "MD",
                                  "ESP", 1971, "eta.fu", 0.5, "Trucks", "MD",
                                  "ESP", 2009, "eta.fu", 0.6, "Cars", "MD",
                                  "ESP", 2009, "eta.fu", 0.7, "Trucks", "MD",
                                  "ESP", 1971, "phi.u", 1.0, "Cars", "MD",
                                  "ESP", 1971, "phi.u", 1.0, "Trucks", "MD",
                                  "ESP", 2009, "phi.u", 1.0, "Cars", "MD",
                                  "ESP", 2009, "phi.u", 1.0, "Trucks", "MD",
                                  "MEX", 1971, "eta.fu", 0.3, "Cars", "MD",
                                  "MEX", 1971, "eta.fu", 0.4, "Trucks", "MD",
                                  "MEX", 2009, "eta.fu", 0.4, "Cars", "MD",
                                  "MEX", 2009, "eta.fu", 0.5, "Trucks", "MD",
                                  "MEX", 1971, "phi.u", 1.0, "Cars", "MD",
                                  "MEX", 1971, "phi.u", 1.0, "Trucks", "MD",
                                  "MEX", 2009, "phi.u", 1.0, "Cars", "MD",
                                  "MEX", 2009, "phi.u", 1.0, "Trucks", "MD",
                                  "ESP", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "ESP", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "eta.fu", 0.0001, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 1971, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas",
                                  "MEX", 2009, "phi.u", 1.0, "Non-energy use", "Natural gas")

  plots_phi_df <- phi_u_plots_df(phi_u_table, countries = c("ESP", "MEX"))

  expect_true(!is.null(plots_phi_df))
  expect_true(inherits(plots_phi_df$Plots[[1]], "ggplot"))
  expect_true(inherits(plots_phi_df$Plots[[2]], "ggplot"))
})
