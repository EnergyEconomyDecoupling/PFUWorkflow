# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Names of targets
#

target_names <- list(countries = "countries",
                     alloc_and_eff_couns = "alloc_and_eff_couns",
                     max_year = "max_year",
                     iea_data_path = "iea_data_path",
                     exemplar_table_path = "exemplar_table_path",
                     fu_analysis_folder = "fu_analysis_folder",
                     report_source_folders = "report_source_folders",
                     report_dest_folder = "report_dest_folder",
                     AllIEAData = "AllIEAData",
                     IEAData = "IEAData",
                     balanced_before = "balanced_before",
                     BalancedIEAData = "BalancedIEAData",
                     balanced_after = "balanced_after",
                     OKToProceed = "OKToProceed",
                     Specified = "Specified",
                     PSUT_final = "PSUT_final",
                     IncompleteAllocationTables = "IncompleteAllocationTables",
                     IncompleteEfficiencyTables = "IncompleteEfficiencyTables",
                     ExemplarLists = "ExemplarLists",
                     CompletedAllocationTables = "CompletedAllocationTables",
                     CompletedEfficiencyTables = "CompletedEfficiencyTables",
                     AllocationGraphs = "AllocationGraphs",
                     EfficiencyGraphs = "EfficiencyGraphs",
                     reports_dest_folder = "reports_dest_folder",
                     reports_complete = "reports_complete")

usethis::use_data(target_names, overwrite = TRUE)


#
# Names and constants associated with exemplar tables.
#

exemplar_names <- list(exemplar_tab_name = "exemplar_table",
                       prev_names = "Prev.names",
                       exemplars = "Exemplars",
                       exemplar_country = "Exemplar.country",
                       exemplar_countries = "Exemplar.countries",
                       exemplar_tables = "Exemplar.tables",
                       iea_data = "IEA.data",
                       alloc_data = "Alloc.data",
                       incomplete_alloc_table = "Incomplete.alloc.table",
                       complete_alloc_table = "Complete.alloc.table",
                       incomplete_eta_table = "Incomplete.eta.table",
                       complete_eta_table = "Complete.eta.table",
                       row_code = "ROW.code",
                       country_name = "Country.name",
                       world = "World")
usethis::use_data(exemplar_names, overwrite = TRUE)



#
# Cache information.
#

cache_info = list(cache_path = ".drake")
usethis::use_data(cache_info, overwrite = TRUE)
