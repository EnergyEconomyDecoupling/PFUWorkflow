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
  expect_equal(et$Exemplar.country %>% unique(), "ESP")
  # Make sure Ghana has the right name and 3-letter code.
  et %>%
    dplyr::filter(Country == "GHA") %>%
    dplyr::select(Country.name) %>%
    unique() %>%
    unlist() %>%
    unname() %>%
    expect_equal("Ghana")
  # Make sure we have all the column names that we expect
  cn <- et %>%
    colnames()
  expect_true("Country" %in% cn)
  expect_true("Year" %in% cn)
  expect_true("Prev.names" %in% cn)
  expect_true("Exemplar.country" %in% cn)
  expect_true("Region.code" %in% cn)
  expect_true("Country.name" %in% cn)

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

  # According to Zeke, the most difficult country is Montenegro (MNE)
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "MNE",
                  .data[[IEATools::iea_cols$year]] == 2005) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "RoEUR", "World"))
  # Check in 2004, should still have 5 exemplars
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "MNE",
                  .data[[IEATools::iea_cols$year]] == 2004) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "RoEUR", "World"))

  # Also check Kosovo, another country with two changes through time.
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "XK",
                  .data[[IEATools::iea_cols$year]] == 2000) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "RoEUR", "World"))
  # Check in 1999, which should still have 5 exemplars
  el %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "XK",
                  .data[[IEATools::iea_cols$year]] == 1999) %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equivalent(c("SRB", "YGS", "ESP", "RoEUR", "World"))


  # Check Ghana. It should have the same list for every year
  el %>%
    dplyr::select(!IEATools::iea_cols$year) %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") %>%
    unique() %>%
    magrittr::extract2("Exemplars") %>%
    unlist(use.names = FALSE) %>%
    expect_equal(c("ESP", "RoAFR", "World"))


  # Check Republic of North Macedonia, because it has changed over time.
  test_single_exemplar(el, "MKD", c("YGS", "ESP", "RoEUR", "World"))
  # Check Serbia, because it has changed over time.
  test_single_exemplar(el, "SRB", c("YGS", "ESP", "RoEUR", "World"))
  # Check Croatia, because it has changed over time.
  test_single_exemplar(el, "HRV", c("YGS", "ESP", "RoEUR", "World"))
  # Check Bosnia and Herzegovina, because it has changed over time.
  test_single_exemplar(el, "BIH", c("YGS", "ESP", "RoEUR", "World"))


  # Check Mongolia, because it has changed over time.
  test_single_exemplar(el, "MNG", c("OAS", "ESP", "RoASA", "World"))


  # Check Botswana, because it has changed over time.
  test_single_exemplar(el, "BWA", c("OAF", "ESP", "RoAFR", "World"))
  # Check Namibia, because it has changed over time.
  test_single_exemplar(el, "NAM", c("OAF", "ESP", "RoAFR", "World"))
  # Check Eritrea, because it has changed over time.
  test_single_exemplar(el, "ERI", c("ETH", "ESP", "RoAFR", "World"))
  # Check South Sudan, because it has changed over time.
  test_single_exemplar(el, "SSD", c("SDN", "ESP", "RoAFR", "World"))


  # Check Suriname, because it has changed over time.
  test_single_exemplar(el, "SUR", c("OAM", "ESP", "RoAMR", "World"))


  # Check Russia, because it has changed over time.
  test_single_exemplar(el, "RUS", c("FSU", "ESP", "RoEUR", "World"))
  # Check Ukraine, because it has changed over time.
  test_single_exemplar(el, "UKR", c("FSU", "ESP", "RoEUR", "World"))
  # Check Kazakhstan, because it has changed over time.
  test_single_exemplar(el, "KAZ", c("FSU", "ESP", "RoASA", "World"))
  # Check Uzbekistan, because it has changed over time.
  test_single_exemplar(el, "UZB", c("FSU", "ESP", "RoASA", "World"))
  # Check Belarus, because it has changed over time.
  test_single_exemplar(el, "BLR", c("FSU", "ESP", "RoEUR", "World"))
  # Check Turkmenistan, because it has changed over time.
  test_single_exemplar(el, "TKM", c("FSU", "ESP", "RoASA", "World"))
  # Check Azerbaijan, because it has changed over time.
  test_single_exemplar(el, "AZE", c("FSU", "ESP", "RoASA", "World"))
  # Check Lithuania, because it has changed over time.
  test_single_exemplar(el, "LTU", c("FSU", "ESP", "RoEUR", "World"))
  # Check Slovenia, because it has changed over time.
  test_single_exemplar(el, "SVN", c("YGS", "ESP", "RoEUR", "World"))
  # Check Georgia, because it has changed over time.
  test_single_exemplar(el, "GEO", c("FSU", "ESP", "RoASA", "World"))
  # Check Latvia, because it has changed over time.
  test_single_exemplar(el, "LVA", c("FSU", "ESP", "RoEUR", "World"))
  # Check Kyrgyzstan, because it has changed over time.
  test_single_exemplar(el, "KGZ", c("FSU", "ESP", "RoASA", "World"))
  # Check Republic of Moldova, because it has changed over time.
  test_single_exemplar(el, "MDA", c("FSU", "ESP", "RoEUR", "World"))
  # Check Estonia, because it has changed over time.
  test_single_exemplar(el, "EST", c("FSU", "ESP", "RoEUR", "World"))
  # Check Tajikistan, because it has changed over time.
  test_single_exemplar(el, "TJK", c("FSU", "ESP", "RoASA", "World"))
  # Check Armenia, because it has changed over time.
  test_single_exemplar(el, "ARM", c("FSU", "ESP", "RoASA", "World"))

})


test_that("exemplar_lists() works for a single country", {
  el <- exemplar_lists(load_exemplar_table(), countries = c("ZAF", "USA"))
  expect_equal(el[[IEATools::iea_cols$country]] %>% unique(), c("USA", "ZAF"))
})
