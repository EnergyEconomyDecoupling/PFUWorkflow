* New tests for new features and old bugs.
    * Now up to 128 tests, all passing.
    * Test coverage remains at 100%.


# SEAPSUTWorkflow 0.1.1 (2020-07-24)

* Workflows can now be created with additional template countries that 
  are separate from the countries to be analyzed.
  Use the argument `additional_exemplar_countries` on `get_plan()` to make a plan that
  uses exemplar countries for final-to-useful allocations and efficiencies.
* `drake` workflow created in a function. 
  See `get_plan()`.
* Initial release.
