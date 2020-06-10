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
  cn <- et %>%
    colnames()
  expect_true("Country" %in% cn)
  expect_true("Year" %in% cn)
  expect_true("Prev_names" %in% cn)
  expect_true("Exemplar_Country" %in% cn)
  expect_true("Region_Code" %in% cn)
  expect_true("Countries_or_Group" %in% cn)
})
