test_that("C matrices are created correctly", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "WithCmats")
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
