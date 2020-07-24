
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
[![Coverage
status](https://codecov.io/gh/MatthewHeun/SEAPSUTWorkflow/branch/master/graph/badge.svg)](https://codecov.io/github/MatthewHeun/SEAPSUTWorkflow?branch=master)
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
         max_year = 1999,
         iea_data_path = "iea_path",
         exemplar_table_path = "exemplar_path",
         fu_analysis_folder = "fu_folder")
#> # A tibble: 18 x 3
#>    target            command                                     dynamic        
#>    <chr>             <expr_lst>                                  <expr_lst>     
#>  1 countries         c(c("GHA", "ZAF"))                        … NA            …
#>  2 alloc_and_eff_co… unique(c(countries, NULL))                … NA            …
#>  3 max_year          1999                                      … NA            …
#>  4 iea_data_path     "iea_path"                                … NA            …
#>  5 exemplar_table_p… "exemplar_path"                           … NA            …
#>  6 fu_analysis_fold… "fu_folder"                               … NA            …
#>  7 AllIEAData        iea_data_path %>% IEATools::load_tidy_iea_… NA            …
#>  8 IEAData           AllIEAData %>% extract_country_data(countr… map(countries)…
#>  9 balanced_before   IEAData %>% is_balanced(countries)        … map(countries)…
#> 10 BalancedIEAData   IEAData %>% make_balanced(countries)      … map(countries)…
#> 11 balanced_after    BalancedIEAData %>% is_balanced(countries)… map(countries)…
#> 12 OKToProceed       ifelse(is.null(stopifnot(all(balanced_afte… NA            …
#> 13 Specified         BalancedIEAData %>% specify(countries)    … map(countries)…
#> 14 PSUT_final        Specified %>% make_psut(countries)        … map(countries)…
#> 15 IncompleteAlloca… fu_analysis_folder %>% load_fu_allocation_… map(alloc_and_…
#> 16 IncompleteEffici… fu_analysis_folder %>% load_eta_fu_tables(… map(alloc_and_…
#> 17 ExemplarLists     exemplar_table_path %>% load_exemplar_tabl… map(countries)…
#> 18 CompletedAllocat… assemble_fu_allocation_tables(IncompleteAl… map(countries)…
```

## History

The functions in this package were used in …
<!-- [Heun et al. [-@Heun:2018]](https://doi.org/10.1016/j.apenergy.2018.05.109). -->

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/SEAPSUTWorkflow/>.

## References
