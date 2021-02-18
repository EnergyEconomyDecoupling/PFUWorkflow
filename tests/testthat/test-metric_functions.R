###########################################################
context("Metric Functions")
###########################################################

library(testthat)

# Establishes an example PSUT data frame for testing

# Creates PSUT data frame shell containing only metadata columns
PSUT_DF_shell <- tibble::tribble(~Country, ~Method, ~Energy.type, ~Last.stage, ~ Year,
                                 "ESP", "PCM", "E", "Final", 1960,
                                 "ESP", "PCM", "E", "Final", 1961,
                                 "GBR", "PCM", "E", "Final", 1960,
                                 "GBR", "PCM", "E", "Final", 1961)

# Creats resource matrices (R)
R_ESP_1960 <- matrix(data = as.vector(c(10, 6, 21,
                                        9, 11, 13,
                                        11, 22, 7)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Resources [from Biodiesels]", "Resources [of Oil and natural gas]", "Resources [from Hydro]"),
                                     c("Biodiesels", "Crude Oil", "Hydro")))

R_ESP_1961 <- matrix(data = as.vector(c(11, 8, 25,
                                        12, 13, 18,
                                        10, 17, 15)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Resources [from Biodiesels]", "Resources [of Oil and natural gas]", "Resources [from Hydro]"),
                                     c("Biodiesels", "Crude Oil", "Hydro")))

R_GBR_1960 <- matrix(data = as.vector(c(10, 9, 18,
                                        10, 13, 15,
                                        13, 23, 8)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Resources [from Biodiesels]", "Resources [of Oil and natural gas]", "Resources [from Hydro]"),
                                     c("Biodiesels", "Crude Oil", "Hydro")))

R_GBR_1961 <- matrix(data = as.vector(c(12, 10, 14,
                                        13, 8, 15,
                                        10, 21, 5)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Resources [from Biodiesels]", "Resources [of Oil and natural gas]", "Resources [from Hydro]"),
                                     c("Biodiesels", "Crude Oil", "Hydro")))

# Creats final demand matrices (Y)
Y_ESP_1960 <- matrix(data = as.vector(c(7, 3, 19,
                                        6, 8, 10,
                                        8, 19, 4)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Gas/diesel oil", "Electricity", "Patent fuel"),
                                     c("Construction", "Domestic aviation", "Iron and steel")))

Y_ESP_1961 <- matrix(data = as.vector(c(8, 5, 22,
                                        9, 10, 15,
                                        7, 14, 12)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Gas/diesel oil", "Electricity", "Patent fuel"),
                                     c("Construction", "Domestic aviation", "Iron and steel")))

Y_GBR_1960 <- matrix(data = as.vector(c(7, 6, 15,
                                        7, 10, 12,
                                        10, 20, 5)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Gas/diesel oil", "Electricity", "Patent fuel"),
                                     c("Construction", "Domestic aviation", "Iron and steel")))

Y_GBR_1961 <- matrix(data = as.vector(c(9, 7, 11,
                                        10, 5, 12,
                                        7, 18, 2)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Gas/diesel oil", "Electricity", "Patent fuel"),
                                     c("Construction", "Domestic aviation", "Iron and steel")))

# Create use (U) matrices
U_ESP_1960 <- matrix(data = as.vector(c(7, 3, 19, # Adjust these values
                                        1, 8, 10,
                                        0, 0, 4)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                     c("Blast furnaces", "Coal mines", "Coke ovens")))

U_ESP_1961 <- matrix(data = as.vector(c(8, 5, 22,
                                        9, 10, 15,
                                        7, 14, 12)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                     c("Blast furnaces", "Coal mines", "Coke ovens")))

U_GBR_1960 <- matrix(data = as.vector(c(7, 6, 15,
                                        7, 10, 12,
                                        10, 20, 5)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                     c("Blast furnaces", "Coal mines", "Coke ovens")))

U_GBR_1961 <- matrix(data = as.vector(c(9, 7, 11,
                                        10, 5, 12,
                                        7, 18, 2)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                     c("Blast furnaces", "Coal mines", "Coke ovens")))

# Create make (V) matrices
V_ESP_1960 <- matrix(data = as.vector(c(7, 0, 0,
                                        0, 8, 0,
                                        0, 2, 4)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Imports [of Aviation gasoline]", "Blast furnaces", "Imports [of Hard Coal (if no detail)]"),
                                     c("Aviation gasoline", "Blast furnace gas", "Hard Coal (if no detail)")))

V_ESP_1961 <- matrix(data = as.vector(c(8, 0, 0,
                                        0, 10, 0,
                                        0, 14, 12)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Imports [of Aviation gasoline]", "Blast furnaces", "Imports [of Hard Coal (if no detail)]"),
                                     c("Aviation gasoline", "Blast furnace gas", "Hard Coal (if no detail)")))

V_GBR_1960 <- matrix(data = as.vector(c(7, 0, 0,
                                        0, 10, 0,
                                        0, 20, 5)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Imports [of Aviation gasoline]", "Blast furnaces", "Imports [of Hard Coal (if no detail)]"),
                                     c("Aviation gasoline", "Blast furnace gas", "Hard Coal (if no detail)")))

V_GBR_1961 <- matrix(data = as.vector(c(9, 0, 0,
                                        0, 5, 0,
                                        0, 18, 2)),
                     nrow = 3, ncol = 3,
                     dimnames = list(c("Imports [of Aviation gasoline]", "Blast furnaces", "Imports [of Hard Coal (if no detail)]"),
                                     c("Aviation gasoline", "Blast furnace gas", "Hard Coal (if no detail)")))

# Create example r_EIOU matrices
r_EIOU_ESP_1960 <- matrix(data = as.vector(c(1, 0, 0,
                                             0, 1, 0,
                                             0, 1, 1)),
                          nrow = 3, ncol = 3,
                          dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                          c("Blast furnaces", "Coal mines", "Coke ovens")))


r_EIOU_ESP_1961 <- matrix(data = as.vector(c(1, 0, 0,
                                             0, 1, 0,
                                             0, 1, 1)),
                          nrow = 3, ncol = 3,
                          dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                          c("Blast furnaces", "Coal mines", "Coke ovens")))

r_EIOU_GBR_1960 <- matrix(data = as.vector(c(1, 0, 0,
                                             0, 1, 0,
                                             0, 1, 1)),
                          nrow = 3, ncol = 3,
                          dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                          c("Blast furnaces", "Coal mines", "Coke ovens")))

r_EIOU_GBR_1961 <- matrix(data = as.vector(c(1, 0, 0,
                                             0, 1, 0,
                                             0, 1, 1)),
                          nrow = 3, ncol = 3,
                          dimnames = list(c("Coke oven coke", "Electricity", "Coke oven gas"),
                                          c("Blast furnaces", "Coal mines", "Coke ovens")))


# Creates lists of each group of matrices and binds to the data frame shell to
# create an example PSUT data frame
R <- list(R_ESP_1960, R_ESP_1961, R_GBR_1960, R_GBR_1961)
Y <- list(Y_ESP_1960, Y_ESP_1961, Y_GBR_1960, Y_GBR_1961)
U <- list(U_ESP_1960, U_ESP_1961, U_GBR_1960, U_GBR_1961)
V <- list(V_ESP_1960, V_ESP_1961, V_GBR_1960, V_GBR_1961)
r_EIOU <- list(r_EIOU_ESP_1960, r_EIOU_ESP_1961, r_EIOU_GBR_1960, r_EIOU_GBR_1961)
PSUT_DF <- tibble::tibble(PSUT_DF_shell, R, r_EIOU, U, V, Y)




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

test_that("calculate_p_ex_total() works as expected", {

  p_total <- PSUT_DF %>% calculate_p_ex_total()

  testthat::expect_type(p_total, "list")
  testthat::expect_equal(colnames(p_total), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Sector", "Year", "EX"))

})

test_that("calculate_p_ex_flow() works as expected", {

  p_flow <- PSUT_DF %>% calculate_p_ex_flow()

  testthat::expect_type(p_flow, "list")
  testthat::expect_equal(colnames(p_flow), c("Country", "Method", "Energy.type",
                                             "Stage", "Gross.Net", "Product",
                                             "Flow", "Year", "EX"))

})

test_that("calculate_all_ex_data() works as expected", {

  all_data <- PSUT_DF %>% calculate_all_ex_data()

  # testthat::expect_type(all_data, "data.frame")
  testthat::expect_equal(colnames(p_total), c("Country", "Method", "Energy.type",
                                              "Stage", "Gross.Net", "Product",
                                              "Sector", "Year", "EX"))
  testthat::expect_equal(unique(all_data$Stage), c("Final", "Primary"))
  testthat::expect_equal(unique(all_data$Gross.Net), c("Net", "Gross"))

})
