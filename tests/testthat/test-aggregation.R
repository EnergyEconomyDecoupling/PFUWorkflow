test_that("extend_to_useful() works as expected", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = SEAPSUTWorkflow::target_names$AggregateProducts)
  tryCatch({
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Check that the aggregated products match expected values.
    non_agg_products <- readd(SEAPSUTWorkflow::target_names$PSUT, character_only = TRUE, path = testing_setup$cache_path)
    agg_prods <- readd(SEAPSUTWorkflow::target_names$AggregateProducts, character_only = TRUE, path = testing_setup$cache_path)

  },
  finally = {
    SEAPSUTWorkflow:::clean_up_after_testing(testing_setup)
  })
})
