


test_that("extending to exergy works as expected", {
  # Set up for 1 past the exergy stuff.
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$IncompleteAllocationTables)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    Phi_constants <- readd(SEAPSUTWorkflow::target_names$Phi_constants, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(all(names(Phi_constants) == c("Product", "phi")))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })

})
