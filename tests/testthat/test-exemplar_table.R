###########################################################
context("Exemplar Tables")
###########################################################


test_that("sample_exemplar_table_path() works correctly", {
  p <- sample_exemplar_table_path()
  expect_true(endsWith(p, file.path("extdata", "Exemplar_Table.xlsx")))
})


test_that("load_exemplar_table() works correctly", {
  et <- load_exemplar_table()
  # Check that certain features are present
  expect_equal(et$Exemplar_Country %>% unique(), "ZAF")
  et %>%
    dplyr::filter(Country == "GHA") %>%
    dplyr::select(Countries_or_Group) %>%
    unique() %>%
    unlist() %>%
    unname() %>%
    expect_equal("Ghana")
})
