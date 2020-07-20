# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Names of targets
#

target_names <- list(countries = "countries",
                     allocation_and_efficiency_countries = "allocation_and_efficiency_countries",
                     max_year = "max_year",
                     iea_data_path = "iea_data_path",
                     exemplar_table_path = "exemplar_table_path",
                     fu_analysis_folder = "fu_analysis_folder",
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
                     CompletedAllocationTables = "CompletedAllocationTables")
usethis::use_data(target_names, overwrite = TRUE)


#
# Names and constants associated with exemplar tables.
#

exemplar_names <- list(exemplar_tab_name = "exemplar_table",
                       prev_names = "Prev.names",
                       exemplars = "Exemplars",
                       exemplar_country = "Exemplar.country",
                       row_code = "ROW.code",
                       country_name = "Country.name",
                       world = "World")
usethis::use_data(exemplar_names, overwrite = TRUE)



#
# Cache information.
#

cache_info = list(cache_path = ".drake")
usethis::use_data(cache_info, overwrite = TRUE)
