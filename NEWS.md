# SEAPSUTWorkflow 0.2.3 (2021-08-20) [![DOI](https://zenodo.org/badge/265032888.svg)](https://zenodo.org/badge/latestdoi/265032888)

* Now using "WLD" instead of "World" everywhere.
* New targets: `country_concordance_path1 and `CountryConcordanceTable`.
* New function `clean_targets()` assists with cleaning targets.
  By default, it cleans only those targets after "IEAData",
  thereby making it easy to re-run the workflow.
* New tests for new features.
  * Now up to 284 tests, all passing.
  * Test coverage remains at 100%.


# SEAPSUTWorkflow 0.2.2 (2021-07-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5091721.svg)](https://doi.org/10.5281/zenodo.5091721)

* Split `CompletedEfficiencyTables` into `CompletedPhiTables` which contains phi.u values, and
  `CompletedEfficiencyTables` which contains eta.fu values.
* Added the target `MachineData` which contains data from `AllMachineData`,
  but only for the countries specified in `countries` and `alloc_and_eff_couns`.


# SEAPSUTWorkflow 0.2.1 (2021-03-17)

* New set of functions in `tes_tfc_functions.R` and `socioecon_functions.R`
  which are used to create the following new targets: `FinalDemandSectors`,
  `PrimaryIndustryPrefixes`, `AggregatePrimaryData`, `AggregateFinalUsefulData`,
  and `SocioEconData`.


# SEAPSUTWorkflow 0.2.0 (2021-01-23)

* New tests for new features.
    * Now up to 216 tests, all passing.
    * Test coverage remains at 100%.
* New target `AllMachineData` which consists of a tidy data
  frames containing information obtained by calling functions in
  machine_functions.R.
* New set of functions in machine_functions.R (`get_eta_filepaths()` and
  `read_all_eta_files`) which read in data from a specified file path and create
  a tidy data frame containing Eta.fu, Phi.u, and any additional metrics supplied.
* New version of plan that includes extending the PSUT matrices to the useful stage.
* New functions `nonstat_alloc_graph()` and `nonstat_alloc_plots_df()`
  which make final-to-useful allocations graphs for only non-stationary allocations
  data, and for later use in reports and other visualization.
* New functions `phi_u_graph()` and `phi_u_plots_df()`
  which make exergy-to-energy ratio graphs for use in reports and other
  visualization.


# SEAPSUTWorkflow 0.1.11 (2020-09-28)

* Hotfix: fixed eta_fu_graph function


# SEAPSUTWorkflow 0.1.10 (2020-09-25)

* Added working final-to-useful efficiency (eta.fu) report.


# SEAPSUTWorkflow 0.1.9 (2020-09-17)

* New functions `eta_fu_graph()` and `eta_fu_plots_df()`
  which make efficiency graphs for later use in reports and other
  visualization.


# SEAPSUTWorkflow 0.1.8 (2020-09-09)

* Changed structure of data frame in AllocationGraphs target. 
  Now, fewer columns are in the data that is graphed. 
  More columns are in the outer (not nested) data frame.


# SEAPSUTWorkflow 0.1.7 (2020-09-07)

* Added allocation report.
* New functions `alloc_graph()` and `alloc_plots_df()`
  which make allocation graphs for later use in reports.


# SEAPSUTWorkflow 0.1.6 (2020-08-26)

* First version that works with full SEAPSUT workflow.
* Added automated spell checking to package checks.
* Added new target `reports_output_folder` to plan and tests.


# SEAPSUTWorkflow 0.1.5 (2020-08-19)

* Fixed bugs with completion code when put into production.


# SEAPSUTWorkflow 0.1.4 (2020-08-14)

* New tests for new features and old bugs.
    * Now up to 159 tests, all passing.
    * Test coverage remains at 100%.
* `load_fu_allocation_tables()` and `load_eta_fu_tables()`
  now write blank templates and read them back in 
  if you request a non-existent fu_allocation_table or eta_fu_table.


# SEAPSUTWorkflow 0.1.3 (2020-07-29)

* New tests for new features and old bugs.
    * Now up to 137 tests, all passing.
    * Test coverage remains at 100%.
* Several updates to get this package working with FU allocations and efficiencies.


# SEAPSUTWorkflow 0.1.2 (2020-07-24)

* New tests for new features and old bugs.
    * Now up to 134 tests, all passing.
    * Test coverage remains at 100%.
* `drake` workflow now extends to completed eta_fu tables.
* New function `assemble_eta_fu_tables()`, 
  a counterpart to `assemble_fu_allocation_tables()`, 
  which allows the use of exemplar countries to complete efficiency tables.


# SEAPSUTWorkflow 0.1.1 (2020-07-24)

* Workflows can now be created with additional template countries that 
  are separate from the countries to be analyzed.
  Use the argument `additional_exemplar_countries` on `get_plan()` to make a plan that
  uses exemplar countries for final-to-useful allocations and efficiencies.
* `drake` workflow created in a function. 
  See `get_plan()`.
* Initial release.
