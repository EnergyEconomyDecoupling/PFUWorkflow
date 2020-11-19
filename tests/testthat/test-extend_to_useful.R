test_that("C matrices are created correctly", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$WithCmats)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the C matrices were added to the WithCMats data frame target
    WithCmats_target <- readd(SEAPSUTWorkflow::target_names$WithCmats, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$C_Y %in% colnames(WithCmats_target))
    expect_true(IEATools::template_cols$C_eiou %in% colnames(WithCmats_target))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("eta_fu and phi_u vectors are created correctly", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$WithEtaPhivecs)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    WithEtaPhi_target <- readd(SEAPSUTWorkflow::target_names$WithEtaPhivecs, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$eta_fu %in% colnames(WithEtaPhi_target))
    expect_true(IEATools::template_cols$phi_u %in% colnames(WithEtaPhi_target))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


