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
  expect_equal(et$Exemplar.country %>% unique(), c("ESP", "WLD"))
  # Make sure we have all the column names that we expect
  cn <- et %>%
    colnames()
  expect_true("Country" %in% cn)
  expect_true("Year" %in% cn)
  expect_true("Prev.names" %in% cn)
  expect_true("Exemplar.country" %in% cn)
  expect_true("Region.code" %in% cn)

  # Try to load only one country.
  et <- load_exemplar_table(countries = "ZAF")
  expect_true(et[[IEATools::iea_cols$country]] %>% unique() == "ZAF")
})


test_single_exemplar <- function(el, coun, expected_exemplar) {
  el %>%
    dplyr::select(!IEATools::iea_cols$year) %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == coun) %>%
    unique() %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equal(expected_exemplar)
}


test_that("exemplar_lists() works as expected", {
  el <- exemplar_lists(load_exemplar_table())

  # Verify the columns of the returned data frame
  expect_equal(colnames(el), c(IEATools::iea_cols$country,
                               IEATools::iea_cols$year,
                               PFUWorkflow::exemplar_names$exemplars))

  # Make sure the year column is numeric
  expect_true(is.numeric(el[[IEATools::iea_cols$year]]))

  # According to Zeke, the most difficult country is Montenegro (MNE)
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "MNE",
                  .data[[IEATools::iea_cols$year]] == 2005) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "EUR", "WLD"))
  # Check in 2004, should still have 5 exemplars
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "MNE",
                  .data[[IEATools::iea_cols$year]] == 2004) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "EUR", "WLD"))

  # Also check Kosovo, another country with two changes through time.
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "XKX",
                  .data[[IEATools::iea_cols$year]] == 2000) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "EUR", "WLD"))
  # Check in 1999, which should still have 5 exemplars
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "XKX",
                  .data[[IEATools::iea_cols$year]] == 1999) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "EUR", "WLD"))


  # Check Ghana. It should have the same list for every year
  el %>%
    dplyr::select(!IEATools::iea_cols$year) %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
    unique() %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equal(c("ESP", "AFR", "WLD"))


  # Check Republic of North Macedonia, because it has changed over time.
  test_single_exemplar(el, "MKD", c("YGS", "ESP", "EUR", "WLD"))
  # Check Serbia, because it has changed over time.
  test_single_exemplar(el, "SRB", c("YGS", "ESP", "EUR", "WLD"))
  # Check Croatia, because it has changed over time.
  test_single_exemplar(el, "HRV", c("YGS", "ESP", "EUR", "WLD"))
  # Check Bosnia and Herzegovina, because it has changed over time.
  test_single_exemplar(el, "BIH", c("YGS", "ESP", "EUR", "WLD"))


  # Check Mongolia, because it has changed over time.
  test_single_exemplar(el, "MNG", c("OAS", "ESP", "ASA", "WLD"))


  # Check Botswana, because it has changed over time.
  test_single_exemplar(el, "BWA", c("OAF", "ESP", "AFR", "WLD"))
  # Check Namibia, because it has changed over time.
  test_single_exemplar(el, "NAM", c("OAF", "ESP", "AFR", "WLD"))
  # Check Eritrea, because it has changed over time.
  test_single_exemplar(el, "ERI", c("ETH", "ESP", "AFR", "WLD"))
  # Check South Sudan, because it has changed over time.
  test_single_exemplar(el, "SSD", c("SDN", "ESP", "AFR", "WLD"))


  # Check Suriname, because it has changed over time.
  test_single_exemplar(el, "SUR", c("OAM", "ESP", "AMR", "WLD"))


  # Check Russia, because it has changed over time.
  test_single_exemplar(el, "RUS", c("FSU", "ESP", "EUR", "WLD"))
  # Check Ukraine, because it has changed over time.
  test_single_exemplar(el, "UKR", c("FSU", "ESP", "EUR", "WLD"))
  # Check Kazakhstan, because it has changed over time.
  test_single_exemplar(el, "KAZ", c("FSU", "ESP", "ASA", "WLD"))
  # Check Uzbekistan, because it has changed over time.
  test_single_exemplar(el, "UZB", c("FSU", "ESP", "ASA", "WLD"))
  # Check Belarus, because it has changed over time.
  test_single_exemplar(el, "BLR", c("FSU", "ESP", "EUR", "WLD"))
  # Check Turkmenistan, because it has changed over time.
  test_single_exemplar(el, "TKM", c("FSU", "ESP", "ASA", "WLD"))
  # Check Azerbaijan, because it has changed over time.
  test_single_exemplar(el, "AZE", c("FSU", "ESP", "ASA", "WLD"))
  # Check Lithuania, because it has changed over time.
  test_single_exemplar(el, "LTU", c("FSU", "ESP", "EUR", "WLD"))
  # Check Slovenia, because it has changed over time.
  test_single_exemplar(el, "SVN", c("YGS", "ESP", "EUR", "WLD"))
  # Check Georgia, because it has changed over time.
  test_single_exemplar(el, "GEO", c("FSU", "ESP", "ASA", "WLD"))
  # Check Latvia, because it has changed over time.
  test_single_exemplar(el, "LVA", c("FSU", "ESP", "EUR", "WLD"))
  # Check Kyrgyzstan, because it has changed over time.
  test_single_exemplar(el, "KGZ", c("FSU", "ESP", "ASA", "WLD"))
  # Check Republic of Moldova, because it has changed over time.
  test_single_exemplar(el, "MDA", c("FSU", "ESP", "EUR", "WLD"))
  # Check Estonia, because it has changed over time.
  test_single_exemplar(el, "EST", c("FSU", "ESP", "EUR", "WLD"))
  # Check Tajikistan, because it has changed over time.
  test_single_exemplar(el, "TJK", c("FSU", "ESP", "ASA", "WLD"))
  # Check Armenia, because it has changed over time.
  test_single_exemplar(el, "ARM", c("FSU", "ESP", "ASA", "WLD"))

})


test_that("exemplar_lists() works for a single country", {
  el <- exemplar_lists(load_exemplar_table(), countries = c("ZAF", "USA"))
  expect_equal(el[[IEATools::iea_cols$country]] %>% unique(), c("ZAF", "USA"))
})

