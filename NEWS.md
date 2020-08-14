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
