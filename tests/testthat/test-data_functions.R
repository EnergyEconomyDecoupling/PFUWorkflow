###########################################################
context("Data Functions")
###########################################################

test_that("extract_country_data works", {
  extracted <- IEATools::sample_iea_data_path() %>%
    IEATools::load_tidy_iea_df() %>%
    extract_country_data(countries = c("ZAF"), max_year = 1999)
  expect_equal(extracted[["Country"]] %>% unique(), "ZAF")
  expect_true(all(extracted[["Year"]] <= 1999))
})

test_that("is_balanced works", {
  IEATools::sample_iea_data_path() %>%
    IEATools::load_tidy_iea_df() %>%
    is_balanced(countries = c("GHA", "ZAF")) %>%
    expect_false()
})
