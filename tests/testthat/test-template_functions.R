###########################################################
context("Template Functions")
###########################################################

test_that("generate_*_template works as expected", {
  testing_setup <- SEAPSUTWorkflow:::set_up_for_testing(how_far = "Specified")
  # cache_path <- tempfile("drake_cache_for_testing")
  # fu_template_folder <- tempdir("FU_analysis_folder_for_testing")
  # plan <- get_plan(iea_data_path = IEATools::sample_iea_data_path(),
  #                  exemplar_table_path = sample_exemplar_table_path(),
  #                  fu_analysis_folder = fu_template_folder,
  #                  countries = c("GHA", "ZAF"),
  #                  max_year = 2000,
  #                  how_far = "Specified")
  tryCatch({
    # Create a fake drake plan
    # temp_cache <- drake::new_cache(path = cache_path)
    # Make the plan in the temp_cache
    # drake::make(plan, cache = temp_cache, verbose = 0)
    drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)

    # Now that we have a cache, try to make a FU Allocation template file
    # fu_template <- generate_fu_allocation_template(country = "GHA", cache_path = cache_path)
    fu_template <- generate_fu_allocation_template(country = "GHA", cache_path = testing_setup$cache_path)

    # Check that the FU allocation template was created.
    # No reason to check anything more, because tests in the IEATools package take care of the details.
    # All we care about here is that things were created.
    expect_true(file.exists(fu_template))
    file.remove(fu_template)

    # Now make sure that we can create an efficiency template
    # eff_template <- generate_eta_fu_template(country = "GHA", cache_path = cache_path,
    #                                          fu_allocation_table_path = IEATools::sample_fu_allocation_table_path())
    eff_template <- generate_eta_fu_template(country = "GHA", cache_path = testing_setup$cache_path,
                                             fu_allocation_table_path =
                                               file.path(drake::readd("fu_analysis_folder", path = testing_setup$cache_path), "GHA", "GHA FU Analysis.xlsx"))
    expect_true(file.exists(eff_template))
    file.remove(eff_template)

  }, finally = {
    # Clean up the cache.
    # temp_cache$destroy()
    # unlink(fu_template_folder, recursive = TRUE, force = TRUE)
    SEAPSUTWorkflow:::clean_up_after_testing(temp_cache = testing_setup$temp_cache,
                                             cache_path = testing_setup$cache_path)
  })
})
