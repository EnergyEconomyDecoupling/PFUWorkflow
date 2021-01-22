
#' Drake cache target names
#'
#' A string list containing names of drake targets.
#' Items in the list provide default values for function arguments
#' throughout the `SEAPSUTWorkflow` package.
#'
#' @format A string list with `r length(target_names)` entries.
#' \describe{
#' \item{countries}{The string name of the countries target in the drake cache, giving the countries to be analyzed.}
#' \item{alloc_and_eff_couns}{The string name of the allocation and efficiency countries target in the drake cache, giving the countries from which final-to-useful allocations and efficiencies can be drawn for final-to-useful analyses. This is a superset of countries.}
#' \item{max_year}{The integer representing the maximum year to be analyzed.}
#' \item{iea_data_path}{A string representing the path to the file containing IEA extended energy balance data.}
#' \item{ceda_data_folder}{A string representing the path to the file containing CEDA data.}
#' \item{machine_data_path}{A string representing the path to the folder containing machine data excel files.}
#' \item{exemplar_table_path}{A string representing the path to the exemplar table.}
#' \item{fu_analysis_folder}{A string representing the folder containing final-to-useful analyses for various countries.}
#' \item{report_source_folders}{A string vector representing folders containing reports to be run as the last step of the workflow.}
#' \item{report_dest_folder}{A string representing the folder containing reports from the workflow.}
#' \item{AllIEAData}{The name of a data frame containing all IEA data read from `iea_data_path`.}
#' \item{IEAData}{A filtered version of `AllIEAData` containing information for only `countries`.}
#' \item{CEDAData}{The name of the data frame containing all CEDA temperature data read from `ceda_data_folder`.}
#' \item{AllMachineData}{A data frame containing Eta.fu and Phi.u values read through functions in `machine_functions.R`.}
#' \item{MachineData}{A version of the `AllMachineData` data frame containing data for only those countries specified in `countries`.}
#' \item{balanced_before}{A boolean indicating whether the `IEAData` are balanced before any further analysis. They usually are not, so this value is typically `FALSE`.}
#' \item{BalancedIEAData}{A balanced version of `IEAData`.}
#' \item{balanced_after}{Same as `balanced_before`, only for after balancing. This should be `TRUE`.}
#' \item{Specified}{A data frame containing specified IEA data.}
#' \item{PSUT_final}{A data frame containing `Specified` in a PSUT format.}
#' \item{IncompleteAllocationTables}{A data frame of final-to-useful allocation tables, one for each country. These allocation tables may be incomplete.}
#' \item{IncompleteEfficiencyTables}{A data frame of final-to-useful efficiency tables, one for each country. These efficiency tables may be incomplete.}
#' \item{ExemplarLists}{A data frame of lists of exemplar countries for each country in `countries`, and maybe more.}
#' \item{CompletedAllocationTables}{A data frame of completed final-to-useful allocation tables.}
#' \item{CompletedEfficiencyTables}{A data frame of completed final-to-useful efficiency tables.}
#' \item{AllocationGraphs}{A data frame containing final-to-useful allocation graphs.}
#' \item{NonStationaryAllocationGraphs}{A data frame containing final-to-useful allocation graphs, for non-stationary data only.}
#' \item{EfficiencyGraphs}{A data frame containing final-to-useful efficiency graphs.}
#' \item{ExergyEnergyGraphs}{A data frame containing exergy-to-energy ratio graphs.}
#' \item{report_source_paths}{A string vector containing paths to report files. These strings provide a detailed view of all the reports that `drake` will generate and should be all report files in `report_source_folders`.}
#' \item{report_dest_paths}{A string for the path to a folder into which reports will written.}
#' \item{reports_complete}{A boolean indicating success (`TRUE`) or failure (`FALSE`) of report generation.}
#' }
#'
#' @examples
#' target_names
"target_names"


#' Exemplar table names
#'
#' A string list containing named names of columns and tabs for exemplar tables.
#' Items in the list provide default values for column name function arguments
#' throughout the `SEAPSUTWorkflow` package.
#'
#' @format A string list with `r length(exemplar_names)` entries.
#' \describe{
#' \item{exemplar_tab_name}{The string name of the tab in the Excel file containing the exemplar table.}
#' \item{prev_names}{The name of a column of previous names used for the country.}
#' \item{exemplars}{The name of a column of exemplar countries.}
#' \item{exemplar_country}{The name of an exemplar country column.}
#' \item{exemplar_countries}{The name of an exemplar countries column.}
#' \item{exemplar_tables}{The name of a column containing exemplar tables.}
#' \item{iea_data}{The name of a column containing IEA extended energy balance data.}
#' \item{alloc_data}{The name of a column containing final-to-useful allocation data.}
#' \item{incomplete_alloc_table}{The name of a column containing incomplete final-to-useful allocation tables.}
#' \item{complete_alloc_table}{The name of a column containing completed final-to-useful allocation tables.}
#' \item{incomplete_eta_table}{The name of a column containing incomplete final-to-useful efficiency tables.}
#' \item{complete_eta_table}{The name of a column containing completed final-to-useful efficiency tables.}
#' \item{row_code}{The name of the rest-of-world region code column.}
#' \item{country_name}{The name of the column containing the long name of a country.}
#' \item{world}{The name of the world region.}
#' }
#'
#' @examples
#' exemplar_names
"exemplar_names"


#' Cache information
#'
#' A string list containing information about the drake cache.
#' Items in the list provide default values for column name function arguments
#' throughout the `SEAPSUTWorkflow` package.
#'
#' @format A string list with `r length(cache_info)` entries.
#' \describe{
#' \item{cache_path}{The default path to the drake cache.}
#' }
#'
#' @examples
#' cache_info
"cache_info"

