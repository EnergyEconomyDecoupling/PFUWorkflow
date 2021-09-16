

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
  expect_true("phi.pf" %in% names(res))
  expect_false("phi.u" %in% names(res))
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
  sum_phi_vecs(phi_pf, phi_u, countries = "GHA")
})


test_that("extending to exergy works as expected in teh workflow", {
  # Set up for 1 past the exergy stuff.
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$IncompleteAllocationTables)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    PhiConstants <- readd(SEAPSUTWorkflow::target_names$PhiConstants, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(all(names(PhiConstants) == c("Product", "phi", "is.useful")))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})


