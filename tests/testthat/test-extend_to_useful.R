test_that("extend_to_useful() works as expected", {
  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = PFUWorkflow::target_names$PSUT_useful)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the C matrices were added to the WithCMats data frame target
    Cmats_target <- readd(PFUWorkflow::target_names$Cmats, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$C_Y %in% colnames(Cmats_target))
    expect_true(IEATools::template_cols$C_eiou %in% colnames(Cmats_target))

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    EtafuPhiu_target <- readd(PFUWorkflow::target_names$EtafuPhiuvecs, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::template_cols$eta_fu %in% colnames(EtafuPhiu_target))
    expect_true(IEATools::template_cols$phi_u %in% colnames(EtafuPhiu_target))

    # Check that the eta_fu and phi_u were added to the WithEtaPhi_target data frame target
    with_useful <- readd(PFUWorkflow::target_names$PSUT_useful, character_only = TRUE, path = testing_setup$cache_path)
    expect_true(IEATools::last_stages$useful %in% with_useful[[IEATools::iea_cols$last_stage]])
  },
  finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })
})
