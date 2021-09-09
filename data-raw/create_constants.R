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
                     ceda_data_folder = "ceda_data_folder",
                     machine_data_path = "machine_data_path",
                     exemplar_table_path = "exemplar_table_path",
                     fu_analysis_folder = "fu_analysis_folder",
                     report_source_folders = "report_source_folders",
                     report_dest_folder = "report_dest_folder",
                     CountryConcordanceTable = "CountryConcordanceTable",
                     AllIEAData = "AllIEAData",
                     IEAData = "IEAData",
                     CEDAData = "CEDAData",
                     AllMachineData = "AllMachineData",
                     MachineData = "MachineData",
                     balanced_before = "balanced_before",
                     BalancedIEAData = "BalancedIEAData",
                     balanced_after = "balanced_after",
                     OKToProceed = "OKToProceed",
                     Specified = "Specified",
                     PSUT_final = "PSUT_final",
                     IncompleteAllocationTables = "IncompleteAllocationTables",
                     TidyIncompleteAllocationTables = "TidyIncompleteAllocationTables",
                     ExemplarLists = "ExemplarLists",
                     CompletedAllocationTables = "CompletedAllocationTables",
                     CompletedEfficiencyTables = "CompletedEfficiencyTables",
                     CompletedPhiTables = "CompletedPhiTables",
                     Cmats = "Cmats",
                     EtaPhivecs = "EtaPhivecs",
                     PSUT_useful = "PSUT_useful",
                     FinalDemandSectors = "FinalDemandSectors",
                     PrimaryIndustryPrefixes = "PrimaryIndustryPrefixes",
                     AggregatePrimaryData = "AggregatePrimaryData",
                     AggregateFinalUsefulData = "AggregateFinalUsefulData",
                     SocioEconData = "SocioEconData",
                     AllocationGraphs = "AllocationGraphs",
                     NonStationaryAllocationGraphs = "NonStationaryAllocationGraphs",
                     EfficiencyGraphs = "EfficiencyGraphs",
                     ExergyEnergyGraphs = "ExergyEnergyGraphs",
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
                       row_code = "Region.code",
                       country_name = "Country.name",
                       world = "WLD")
usethis::use_data(exemplar_names, overwrite = TRUE)


#
# Give the names of SEAPSUTWorkflow columns, this function compliments "IEATools::iea_cols".
#

sea_cols <- list(stage_colname = "Stage",
                 gross_net_colname = "Gross.Net",
                 e_product_colname = "E.product",
                 sector_colname = "Sector",
                 flow_colname = "Flow",
                 agg_by_colname = "Aggregation.by",
                 fd_sectors_colname = "Fd.sectors",
                 p_ind_comp_colname = "p_industries_complete",
                 p_ind_prefix_colname = "p_industry_prefixes",
                 ex_colname = "EX",
                 ex_p_colname = "EX.p",
                 ex_net_colname = "EX.d_net",
                 ex_gross_colname = "EX.d_gross")
usethis::use_data(sea_cols, overwrite = TRUE)


#
# Metadata information for aggregation groups
#

agg_metadata <- list(total_value = "Total",
                     all_value = "All",
                     product_value = "Product",
                     sector_value = "Sector",
                     flow_value = "Flow")
usethis::use_data(agg_metadata, overwrite = TRUE)


#
# Metadata information for gross or net
#

gross_net_metadata <- list(gross_value = "Gross",
                           net_value = "Net")
usethis::use_data(gross_net_metadata, overwrite = TRUE)


#
# Column names for socio-economic data
#
socioecon_cols <- list(isocode_colname = "isocode",
                       year_colname = "year",
                       rgdpe_colname = "rgdpe",
                       rgdpo_colname = "rgdpo",
                       rgdpna_colname = "rgdpna",
                       emp_colname = "emp",
                       avh_colname = "avh",
                       hc_colname = "hc",
                       rnna_colname = "rnna",
                       rkna_colname = "rkna",
                       K_colname = "K",
                       Kserv_colname = "Kserv",
                       L_colname = "L",
                       Ladj_colname = "Ladj")
usethis::use_data(socioecon_cols, overwrite = TRUE)


#
# Tab name for machine efficiencies
#

machine_constants <- list(efficiency_tab_name = "FIN_ETA")
usethis::use_data(machine_constants, overwrite = TRUE)


#
# Cache information.
#

cache_info = list(cache_path = ".drake")
usethis::use_data(cache_info, overwrite = TRUE)

