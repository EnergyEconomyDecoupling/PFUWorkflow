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
  # The exemplar country for everybody is South Africa,
  # matching the data bundled with the IEATools package.
  expect_equal(et$Exemplar_Country %>% unique(), "ZAF")
  # Make sure Ghana has the right name and 3-letter code.
  et %>%
    dplyr::filter(Country == "GHA") %>%
    dplyr::select(Countries_or_Group) %>%
    unique() %>%
    unlist() %>%
    unname() %>%
    expect_equal("Ghana")
  # Make sure we have all the column names that we expect
  cn <- et %>%
    colnames()
  expect_true("Country" %in% cn)
  expect_true("Year" %in% cn)
  expect_true("Prev_names" %in% cn)
  expect_true("Exemplar_Country" %in% cn)
  expect_true("Region_Code" %in% cn)
  expect_true("Countries_or_Group" %in% cn)
})


test_that("exemplar_lists() works as expected", {
  el <- exemplar_lists(load_exemplar_table())

  # Check Ghana. It should have the same list for every year
  el %>%
    dplyr::select(!IEATools::iea_cols$year) %>%
    dplyr::filter(IEATools::iea_cols$country == "GHA") %>%
    unique() %>%
    magrittr::extract2("Exemplars") %>%
    unlist() %>%
    expect_equal(list("ZAF", "RoAFR", "Restofworld"))

})
