
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/SEAPSUTWorkflow)](https://cran.r-project.org/package=SEAPSUTWorkflow)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build
status](https://github.com/MatthewHeun/SEAPSUTWorkflow/workflows/check-master-develop/badge.svg)](https://github.com/MatthewHeun/SEAPSUTWorkflow/actions)
[![Travis-CI Build
Status](https://travis-ci.org/MatthewHeun/SEAPSUTWorkflow.svg?branch=master)](https://travis-ci.org/MatthewHeun/SEAPSUTWorkflow)
[![Codecov test
coverage](https://codecov.io/gh/EnergyEconomyDecoupling/SEAPSUTWorkflow/branch/master/graph/badge.svg)](https://codecov.io/gh/EnergyEconomyDecoupling/SEAPSUTWorkflow?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5091720.svg)](https://doi.org/10.5281/zenodo.5091720)
<!-- badges: end -->

# SEAPSUTWorkflow

The `R` package SEAPSUTWorkflow provides functions and
[drake](https://github.com/ropensci/drake) workflows for Societal Exergy
Analysis (SEA) using the Physical Supply Use Table (PSUT) framework.

## Installation

<!-- You can install the released version of SEAPSUTWorkflow from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("SEAPSUTWorkflow") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install `SEAPSUTWorkflow` from github with:

``` r
# install.packages("devtools")
devtools::install_github("MatthewHeun/SEAPSUTWorkflow")
```

## Example

To create a `drake` workflow, do this:

``` r
library(SEAPSUTWorkflow)
get_plan(countries = c("GHA", "ZAF"),
         additional_exemplar_countries = "World",
         max_year = 1999,
         ceda_data_folder = "ceda_path",
         machine_data_path = "machine_path",
         iea_data_path = "iea_path",
         exemplar_table_path = "exemplar_path",
         fu_analysis_folder = "fu_folder",
         reports_source_folders = "reports_source_path",
         reports_dest_folder = "reports_dest_path"
         )
#> # A tibble: 37 x 3
#>    target                 command                       dynamic   
#>    <chr>                  <expr_lst>                    <expr_lst>
#>  1 countries              c(c("GHA", "ZAF"))            NA        
#>  2 alloc_and_eff_couns    unique(c(countries, "World")) NA        
#>  3 max_year               1999                          NA        
#>  4 iea_data_path          "iea_path"                    NA        
#>  5 ceda_data_folder       "ceda_path"                   NA        
#>  6 machine_data_path      "machine_path"                NA        
#>  7 exemplar_table_path    "exemplar_path"               NA        
#>  8 fu_analysis_folder     "fu_folder"                   NA        
#>  9 reports_source_folders "reports_source_path"         NA        
#> 10 reports_dest_folder    "reports_dest_path"           NA        
#> # ... with 27 more rows
```

## History

The functions in this package were used in …
<!-- [Heun et al. [-@Heun:2018]](https://doi.org/10.1016/j.apenergy.2018.05.109). -->

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/SEAPSUTWorkflow/>.

## References
