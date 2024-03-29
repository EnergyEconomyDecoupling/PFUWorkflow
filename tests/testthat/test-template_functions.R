###########################################################
context("Template Functions")
###########################################################

test_that("generate_*_template works as expected", {
  testing_setup <- PFUWorkflow:::set_up_for_testing(how_far = "Specified")
  tryCatch({
    # Make the plan in the temp_cache
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Now that we have a cache, try to make a FU Allocation template file
    fu_template <- generate_fu_allocation_template(country = "GHA", cache_path = testing_setup$cache_path)

    # Check that the FU allocation template was created.
    # No reason to check anything more, because tests in the IEATools package take care of the details.
    # All we care about here is that things were created.
    expect_true(file.exists(fu_template))
    file.remove(fu_template)

    # Now make sure that we can create an efficiency template
    eff_template <- generate_eta_fu_template(country = "GHA", cache_path = testing_setup$cache_path,
                                             fu_allocation_table_path =
                                               file.path(drake::readd("fu_analysis_folder", path = testing_setup$cache_path), "GHA", "GHA FU Analysis.xlsx"))
    expect_true(file.exists(eff_template))
    file.remove(eff_template)

  }, finally = {
    PFUWorkflow:::clean_up_after_testing(testing_setup)
  })
})
