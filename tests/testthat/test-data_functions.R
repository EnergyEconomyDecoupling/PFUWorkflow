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


test_that("make_balanced works", {
  IEATools::sample_iea_data_path() %>%
    IEATools::load_tidy_iea_df() %>%
    make_balanced(countries = c("GHA", "ZAF")) %>%
    is_balanced(countries = c("GHA", "ZAF")) %>%
    expect_true()
})


test_that("specify works", {
  dat <- IEATools::sample_iea_data_path() %>%
    IEATools::load_tidy_iea_df()
  expect_equal(dat %>%
                 dplyr::filter(Flow == "Coal mines") %>%
                 nrow(), 2)
  expect_equal(dat %>%
                 make_balanced(countries = c("GHA", "ZAF")) %>%
                 specify(countries = c("GHA", "ZAF")) %>%
                 dplyr::filter(Flow == "Coal mines") %>%
                 nrow(), 8)
})


test_that("make_psut works", {
  psut <- IEATools::sample_iea_data_path() %>%
    IEATools::load_tidy_iea_df() %>%
    make_balanced(countries = c("GHA", "ZAF")) %>%
    specify(countries = c("GHA", "ZAF")) %>%
    make_psut(countries = c("GHA", "ZAF"))

  expect_equal(nrow(psut), 4)
})
