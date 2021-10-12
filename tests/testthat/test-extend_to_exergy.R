

test_that("calc_phi_pf_vecs() works as expected with a simple example", {
  phi_constants <- IEATools::sample_phi_constants_path() %>%
    IEATools::load_phi_constants_table()
  phi_u_vecs <- tibble::tibble(Country = "GHA",
                               Year = 1971,
                               rownames = c("Light", "MD"),
                               colnames = "col",
                               matnames = "phi.u",
                               matvals = c(0.8, 0.9),
                               rowtypes = "rowtype",
                               coltypes = "coltype") %>%
    dplyr::group_by(Country, Year) %>%
    matsindf::collapse_to_matrices() %>%
    dplyr::rename(phi.u = matvals) %>%
    dplyr::mutate(
      Quantity = NULL
    )
  res <- calc_phi_pf_vecs(phi_constants, phi_u_vecs, countries = "GHA")
  expect_true(IEATools::template_cols$phi_pf %in% names(res))
  expect_false(IEATools::template_cols$phi_u %in% names(res))

  expect_equal(matsbyname::rowtype(res$phi.pf[[1]]), IEATools::row_col_types$product)
  expect_equal(matsbyname::coltype(res$phi.pf[[1]]), IEATools::phi_constants_names$phi_colname)
})


test_that("sum_phi_vecs() works as expected", {

  phi_pf_vec <- matrix(c(1.1,
                         1.05), nrow = 2, ncol = 1, dimnames = list(c("Coal", "Oil"), "phi"))
  # Make a bogus data frame of phi_pf vectors
  phi_pf <- tibble::tibble(phi.pf = matsbyname::make_list(phi_pf_vec, n = 2, lenx = 1),
                           Country = "GHA",
                           Year = c(1971, 2000))

  phi_u_vec <- matrix(c(0.8,
                        0.9,
                        0.7), nrow = 3, ncol = 1, dimnames = list(c("Light", "MD", "Propulsion"), "phi"))
  phi_u <- tibble::tibble(phi.u = matsbyname::make_list(phi_u_vec, n = 2, lenx = 1),
                          Country = "GHA",
                          Year = c(1971, 2000))
  res <- sum_phi_vecs(phi_pf, phi_u, countries = "GHA")
  expected_phi <- matrix(c(1.1,
                           0.8,
                           0.9,
                           1.05,
                           0.7), ncol = 1, dimnames = list(c("Coal", "Light", "MD", "Oil", "Propulsion"), "phi"))
  expect_equal(res$phi[[1]], expected_phi)
  expect_equal(res$phi[[2]], expected_phi)

  # Ensure that we get an error when phi_pf and phi_u have the same rows.
  phi_u_vec_2 <- matrix(c(0.8,
                          0.9,
                          0.7), nrow = 3, ncol = 1, dimnames = list(c("Coal", "MD", "Propulsion"), "phi"))
  phi_u_2 <- tibble::tibble(phi.u = matsbyname::make_list(phi_u_vec_2, n = 2, lenx = 1),
                            Country = "GHA",
                            Year = c(1971, 2000))
  expect_error(sum_phi_vecs(phi_pf, phi_u_2, countries = "GHA"),
               "the length of the sum of phi_pf and phi_u vectors")

  # Ensure that we get an error when phi_pf and phi_u have different column names
  phi_u_3 <- phi_u %>%
    dplyr::mutate(
      phi.u = matsbyname::setcolnames_byname(phi.u, colnames = "phi.u")
    )
  expect_error(sum_phi_vecs(phi_pf, phi_u_3, countries = "GHA"),
               "the names of the phi.pf and phi.u columns should be the same.")

  # Ensure that we get an error when either phi_pf or phi_u have 2 columns.
  phi_u_4 <- phi_u %>%
    dplyr::mutate(
      phi.u = matsbyname::sum_byname(phi.u, matrix(42, dimnames = list("row", "col")))
    )
  expect_error(sum_phi_vecs(phi_pf, phi_u_4, countries = "GHA"),
               "need phi vectors with one column only.")
  phi_pf_2 <- phi_pf %>%
    dplyr::mutate(
      phi.pf = matsbyname::sum_byname(phi.pf, matrix(42, dimnames = list("row", "col")))
    )
  expect_error(sum_phi_vecs(phi_pf_2, phi_u, countries = "GHA"))

})


test_that("extending to exergy works as expected in the workflow", {
  # Set up for 1 past the exergy stuff.
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$PSUT_useful_exergy)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    PhiConstants <- readd(SEAPSUTWorkflow::target_names$PhiConstants, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(all(names(PhiConstants) == c("Product", "phi", "is.useful")))
    Phivecs <- readd(SEAPSUTWorkflow::target_names$Phivecs, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(all(names(Phivecs) == c("Country", "Year", "phi")))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})


