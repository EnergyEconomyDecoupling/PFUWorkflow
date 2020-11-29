test_that("C matrices are created correctly", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$Cmats)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the C matrices were added to the WithCMats data frame target
    Cmats_target <- readd(SEAPSUTWorkflow::target_names$Cmats, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$C_Y %in% colnames(Cmats_target))
    expect_true(IEATools::template_cols$C_eiou %in% colnames(Cmats_target))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("eta_fu and phi_u vectors are created correctly", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$EtaPhivecs)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    EtaPhi_target <- readd(SEAPSUTWorkflow::target_names$EtaPhivecs, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$eta_fu %in% colnames(EtaPhi_target))
    expect_true(IEATools::template_cols$phi_u %in% colnames(EtaPhi_target))
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})


test_that("extend_to_useful() works as expected", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$PSUT_useful)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    with_useful <- readd(SEAPSUTWorkflow::target_names$PSUT_useful, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::last_stages$useful %in% with_useful[[IEATools::iea_cols$last_stage]])
  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
