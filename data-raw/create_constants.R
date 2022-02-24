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
                     PhiConstants = "PhiConstants",
                     CompletedAllocationTables = "CompletedAllocationTables",
                     CompletedEfficiencyTables = "CompletedEfficiencyTables",
                     CompletedPhiuTables = "CompletedPhiuTables",
                     Cmats = "Cmats",
                     EtafuPhiuvecs = "EtafuPhiuvecs",
                     Etafuvecs = "Etafuvecs",
                     Phiuvecs = "Phiuvecs",
                     Phipfvecs = "Phipfvecs",
                     Phivecs = "Phivecs",
                     PSUT_useful = "PSUT_useful",
                     PSUT_useful_exergy = "PSUT_useful_exergy",
                     PSUT = "PSUT",
                     AggregateProducts = "AggregateProducts",
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
                       region_code = "Region.code",
                       country_name = "Country.name",
                       world = "WLD")
usethis::use_data(exemplar_names, overwrite = TRUE)


#
# Give the names of PFUWorkflow columns, this function compliments "IEATools::iea_cols".
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

cache_info <- list(cache_path = ".drake")
usethis::use_data(cache_info, overwrite = TRUE)


#
# phi.sources
#

phi_sources <- list(eta_fu_tables = "eta_fu.tables",
                    temperature_data = "temperature.data",
                    phi_constants = "phi.constants")
usethis::use_data(phi_sources, overwrite = TRUE)


#
# Aggregation information
#

product_aggregation_map <-
  list(`Oil and oil products`        = IEATools::oil_and_oil_products %>% unlist() %>% unname(),
       `Natural gas`                 = IEATools::primary_gas_products %>% unlist() %>% unname(),
       Renewables                    = IEATools::renewable_products %>% unlist() %>% unname(),
       `Biofuels and waste products` = IEATools::biofuels_and_waste_products %>% unlist() %>% unname())
usethis::use_data(product_aggregation_map, overwrite = TRUE)

#
# All countries to run in the workflow
#
all_countries <- list(
  ago = "AGO",
  alb = "ALB",
  are = "ARE",
  arg = "ARG",
  arm = "ARM",
  aus = "AUS",
  aut = "AUT",
  aze = "AZE",
  bel = "BEL",
  ben = "BEN",
  bfa = "BFA",
  bgd = "BGD",
  bgr = "BGR",
  bhr = "BHR",
  bih = "BIH",
  blr = "BLR",
  bol = "BOL",
  bra = "BRA",
  brn = "BRN",
  bwa = "BWA",
  can = "CAN",
  che = "CHE",
  chl = "CHL",
  chn = "CHN",
  cmr = "CMR",
  cod = "COD",
  cog = "COG",
  col = "COL",
  civ = "CIV",
  cri = "CRI",
  cub = "CUB",
  cuw = "CUW",
  cyp = "CYP",
  cze = "CZE",
  deu = "DEU",
  dnk = "DNK",
  dom = "DOM",
  dza = "DZA",
  ecu = "ECU",
  egy = "EGY",
  eri = "ERI",
  esp = "ESP",
  est = "EST",
  eth = "ETH",
  fin = "FIN",
  fra = "FRA",
  gab = "GAB",
  gbr = "GBR",
  geo = "GEO",
  gha = "GHA",
  gib = "GIB",
  gnq = "GNQ",
  grc = "GRC",
  grl = "GRL",
  gtm = "GTM",
  guy = "GUY",
  hkg = "HKG",
  hnd = "HND",
  hrv = "HRV",
  hti = "HTI",
  hun = "HUN",
  idn = "IDN",
  ind = "IND",
  irl = "IRL",
  irn = "IRN",
  irq = "IRQ",
  isl = "ISL",
  isr = "ISR",
  ita = "ITA",
  jam = "JAM",
  jor = "JOR",
  jpn = "JPN",
  kaz = "KAZ",
  ken = "KEN",
  kgz = "KGZ",
  khm = "KHM",
  kor = "KOR",
  kwt = "KWT",
  lao = "LAO",
  lbn = "LBN",
  lby = "LBY",
  lka = "LKA",
  ltu = "LTU",
  lux = "LUX",
  lva = "LVA",
  mar = "MAR",
  mda = "MDA",
  mdg = "MDG",
  mex = "MEX",
  msu = "MSU",
  mkd = "MKD",
  mli = "MLI",
  mlt = "MLT",
  mmr = "MMR",
  mne = "MNE",
  mng = "MNG",
  moz = "MOZ",
  mrt = "MRT",
  mus = "MUS",
  myu = "MYU",
  mys = "MYS",
  nam = "NAM",
  ner = "NER",
  nga = "NGA",
  nic = "NIC",
  nld = "NLD",
  nor = "NOR",
  npl = "NPL",
  nzl = "NZL",
  oaf = "OAF",
  oam = "OAM",
  oas = "OAS",
  omn = "OMN",
  pak = "PAK",
  pan = "PAN",
  per = "PER",
  phl = "PHL",
  pol = "POL",
  prk = "PRK",
  prt = "PRT",
  pry = "PRY",
  pse = "PSE",
  qat = "QAT",
  rou = "ROU",
  rus = "RUS",
  rwa = "RWA",
  sau = "SAU",
  sdn = "SDN",
  sen = "SEN",
  sgp = "SGP",
  slv = "SLV",
  srb = "SRB",
  ssd = "SSD",
  sun = "SUN",
  sur = "SUR",
  svk = "SVK",
  svn = "SVN",
  swe = "SWE",
  syr = "SYR",
  tcd = "TCD",
  tgo = "TGO",
  tha = "THA",
  tjk = "TJK",
  tkm = "TKM",
  tto = "TTO",
  tun = "TUN",
  tur = "TUR",
  twn = "TWN",
  tza = "TZA",
  uga = "UGA",
  ukr = "UKR",
  ury = "URY",
  usa = "USA",
  uzb = "UZB",
  ven = "VEN",
  vnm = "VNM",
  wab = "WAB",
  wld = "WLD",
  wmb = "WMB",
  xkx = "XKX",
  yem = "YEM",
  yug = "YUG",
  zaf = "ZAF",
  zmb = "ZMB",
  zwe = "ZWE"

)

usethis::use_data(all_countries, overwrite = TRUE)


#
# Countries whose data also exists in another 'country'; i.e. Memo: Uganda (UGA)
# in Other Africa (OAF).
#
double_counted_countries <- list(
  bfa = "BFA",
  mdg = "MDG",
  mli = "MLI",
  mrt = "MRT",
  msu = "MSU",
  myu = "MYU",
  rwa = "RWA",
  tcd = "TCD",
  uga = "UGA"
)

usethis::use_data(double_counted_countries, overwrite = TRUE)


#
# Countries to run in the workflow which should sum to World (WLD)
#
canonical_countries <- dplyr::setdiff(all_countries,
                                      double_counted_countries)

usethis::use_data(canonical_countries, overwrite = TRUE)


