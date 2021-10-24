#' Create a drake plan for societal exergy analysis
#'
#' Creates a drake workflow for societal exergy analysis.
#' The caller specifies location of IEA data,
#' which countries should be analyzed, and
#' which the maximum year to be analyzed.
#'
#' The return value is a `drake` plan object with the following targets:
#'
#' * `countries`: The countries to be analyzed, supplied in the `countries` argument.
#' * `alloc_and_eff_couns`: The full set of countries for which final-to-useful allocations and efficiencies will be read. This is the sum of `countries` and `additional_exemplar_countries`, with duplicates removed.
#' * `max_year`: The maximum year to be analyzed, supplied in the `max_year` argument.
#' * `iea_data_path`: The path to IEA extended energy balance data, supplied in the `iea_data_path` argument.
#' * `country_concordance_path`: The path to the country concordance file, supplied in the `country_concordance_path` argument.
#' * `phi_constants_path`: The path to a phi (exergy-to-energy ratio) file, supplied in the `phi_constants_path` argument.
#' * `ceda_data_folder`: The path to the CEDA data, supplied in the `ceda_data_folder` argument.
#' * `machine_data_path`: The path to the machine data excel files, supplied in the `machine_data_path` argument.
#' * `exemplar_table_path`: The path to an exemplar table, supplied in the `exemplar_table_path` argument.
#' * `fu_analysis_folder`: The path to the final-to-useful analysis folder, supplied in the `fu_analysis_folder` argument.
#' * `report_output_folder`: The path to a report output folder, supplied in the `report_output_folder` argument.
#' * `CountryConcordanceTable`: A data frame containing concordance information which maps full country names to custom 3 letter codes.
#' * `AllIEAData`: A data frame with all IEA extended energy balance data read from `iea_data_path`.
#' * `IEAData`: A version of the `AllIEAData` data frame containing data for only those countries specified in `countries`.
#' * `CEDAData`: A data frame containing temperature data supplied through `CEDATools::read_cru_cy_files`.
#' * `AllMachineData`: A data frame containing Eta.fu values read through functions in `machine_functions.R`.
#' * `MachineData`: A filtered version of `AllMachineData` containing information for only `alloc_and_eff_couns`.
#' * `balanced_before`: A boolean that tells where the data were balanced as received, usually a vector of `FALSE`, one for each country.
#' * `BalancedIEAData`: A data frame containing balanced IEA extended energy balance data.
#' * `balanced_after`: A boolean telling whether IEA extended energy balance data is balanced after balancing, usually a vector of `TRUE`, one for each country.
#' * `OKToProceed`: `NULL` means everything is balanced and proceeding is OK.
#' * `Specified`: A data frame with specified industries. See `IEATools::specify_all()`.
#' * `PSUT_final`: A data frame containing PSUT matrices up to the final stage.
#' * `ExemplarLists`: A data frame containing lists of exemplar countries on a per-country, per-year basis.
#' # `Phi_constants`: A table of reasonable constant values for phi, the energy-to-exergy ratio.
#' * `IncompleteAllocationTables`: A data frame containing final-to-useful allocation tables.
#' * `TidyIncompleteAllocationTables`: A data frame containing final-to-useful allocation tables.
#' * `CompletedAllocationTables` : A data frame containing completed final-to-useful allocation tables.
#' * `CompletedEfficiencyTables`: A data frame containing completed final-to-useful efficiency tables.
#' * `CompletedPhiTables` : A data frame of completed exergy-to-energy ratios.
#' * `Cmats` : A data frame containing `CompletedAllocationTables` in matrix form.
#' * `EtaPhivecs` : A data frame containing final-to-useful efficiency and exergy-to-energy ratio vectors.
#' * `PSUT_useful` : A data frame containing PSUT matrices up to the useful stage.
#' * `FinalDemandSectors`: A list of final demand sectors, supplied through the `get_fd_sectors` function.
#' * `PrimaryIndustryPrefixes`: A string vector of primary industry prefixes, supplied through the `get_p_industry_prefixes` function.
#' * `AggregatePrimaryData` : A data frame containing aggregate primary energy and exergy values by total, product, and flow.
#' * `AggregateFinalUsefulData` : A data frame containing aggregate final and useful energy and exergy values by total, product, and sector.
#' * `SocioEconData` : A data frame containing socioeconomic data, supplied by the `get_L_K_GDP_data` function.
#' * `AllocationGraphs` : A data frame containing allocation plots.
#' * `NonStationaryAllocationGraphs` : A data frame containing allocation plots for non-stationary data only.
#' * `EfficiencyGraphs` : A data frame containing final-to-useful efficiency plots.
#' * `ExergyEnergyGraphs` : A data frame containing exergy-to-energy ratio plots.
#' * `report_source_paths`: A string vector of paths to sources for reports.
#' * `report_dest_paths`: A string for the path to a folder into which reports will written.
#' * `reports_complete`: A boolean that tells whether reports were written successfully.
#'
#' Callers can execute the plan by calling `drake::make(plan)`.
#' Results can be recovered with
#' `drake::readd(target = iea_data_path)` or similar.
#'
#' Note that some targets can be read using `readd_by_country()`, including:
#'
#' * `AllIEAData`,
#' * `IEAData`,
#' * `BalancedIEAData`,
#' * `Specified`,
#' * `PSUT_final`,
#' * `ExemplarLists`,
#' * `IncompleteAllocationTables`,
#' * `CompletedAllocationTables`,
#' * `IncompleteEfficiencyTables`, and
#' * `CompletedEfficiencyTables`,
#'
#' @param countries A vector of abbreviations for countries whose energy conversion chain is to be analyzed,
#'                  such as "c('GHA', 'ZAF')".
#'                  Countries named in `countries` can also serve as exemplars for
#'                  final-to-useful allocations and efficiencies.
#' @param additional_exemplar_countries A vector of country abbreviations for which final-to-useful allocations
#'                                      and efficiencies will be read.
#'                                      An energy conversion chain will _not_ be constructed for these countries.
#'                                      However, their final-to-useful allocations and efficiencies
#'                                      may be used as exemplar information for the countries in `countries`.
#'                                      Default is `NULL`, indicating no additional exemplars.
#' @param max_year The last year to be studied, typically the last year for which data are available.
#' @param how_far A string indicating the last target to include in the plan that is returned.
#'                Default is "all_targets" to indicate all targets of the plan should be returned.
#' @param iea_data_path The path to IEA extended energy balance data in .csv format.
#' @param country_concordance_path The path to the country concordance Excel file.
#' @param phi_constants_path The path to a phi (exergy-to-energy ratio) Excel file.
#' @param ceda_data_folder The path to the CEDA data in text file, .per, format.
#' @param machine_data_path The path to the machine data in .xlsx format.
#' @param exemplar_table_path The path to an exemplar table.
#' @param fu_analysis_folder The path to a folder containing final-to-useful analyses.
#'                           Sub-folders named with 3-letter country abbreviations are assumed.
#' @param reports_source_folders A string vector containing paths to folders of report sources, usually
#'                               `.Rnw` or `.Rmd` files.
#' @param reports_dest_folder The path to a folder into which reports are written.
#'
#' @return A drake plan object.
#'
#' @export
#'
#' @seealso
#'
#' * [How to create a plan in a function](https://stackoverflow.com/questions/62140991/how-to-create-a-plan-in-a-function)
#' * [Best practices for unit tests on custom functions for a drake workflow](https://stackoverflow.com/questions/61220159/best-practices-for-unit-tests-on-custom-functions-for-a-drake-workflow)
#' * [drakepkg](https://github.com/tiernanmartin/drakepkg)
#' * [Workflows as R packages](https://books.ropensci.org/drake/projects.html#workflows-as-r-packages)
#'
#' @examples
#' get_plan(countries = c("GHA", "ZAF"),
#'          max_year = 1999,
#'          iea_data_path = "iea_path",
#'          country_concordance_path = "country_concordance_path",
#'          phi_constants_path = "phi_constants_path",
#'          ceda_data_folder = "ceda_path",
#'          machine_data_path = "machine_path",
#'          exemplar_table_path = "exemplar_path",
#'          fu_analysis_folder = "fu_folder",
#'          reports_source_folders = "reports_source_folders",
#'          reports_dest_folder = "reports_dest_folder")
get_plan <- function(countries, additional_exemplar_countries = NULL,
                     max_year, how_far = "all_targets",
                     iea_data_path,
                     country_concordance_path, phi_constants_path, ceda_data_folder,
                     machine_data_path, exemplar_table_path,
                     fu_analysis_folder,
                     reports_source_folders, reports_dest_folder) {

  # Get around warnings of type "no visible binding for global variable".
  alloc_and_eff_couns <- NULL
  map <- NULL
  CountryConcordanceTable <- NULL
  AllIEAData <- NULL
  IEAData <- NULL
  CEDAData <- NULL
  AllMachineData <- NULL
  MachineData <- NULL
  BalancedIEAData <- NULL
  balanced_after <- NULL
  Specified <- NULL
  PSUT_final <- NULL
  IncompleteAllocationTables <- NULL
  TidyIncompleteAllocationTables <- NULL
  IncompleteEfficiencyTables <- NULL
  ExemplarLists <- NULL
  PhiConstants <- NULL
  CompletedAllocationTables <- NULL
  CompletedEfficiencyTables <- NULL
  CompletedPhiuTables <- NULL
  Cmats <- NULL
  EtafuPhiuvecs <- NULL
  Phipfvecs <- NULL
  Phiuvecs <- NULL
  Phivecs <- NULL
  PSUT_useful <- NULL
  PSUT_useful_exergy <- NULL
  FinalDemandSectors <- NULL
  PrimaryIndustryPrefixes <- NULL
  AggregatePrimaryData <- NULL
  AggregateFinalUsefulData <- NULL
  SocioEconData <- NULL

  p <- drake::drake_plan(

    # (0) Set many arguments to be objects in the drake cache for later use

    # Use !!, for tidy evaluation, to put the arguments' values in the plan.
    # See https://stackoverflow.com/questions/62140991/how-to-create-a-plan-in-a-function
    # Need to enclose !!countries in c() (or an identity function), else it doesn't work when countries has length > 1.
    countries = c(!!countries),
    alloc_and_eff_couns = unique(c(countries, !!additional_exemplar_countries)),
    max_year = !!max_year,
    iea_data_path = !!iea_data_path,
    country_concordance_path = !!country_concordance_path,
    phi_constants_path = !!phi_constants_path,
    ceda_data_folder = !!ceda_data_folder,
    machine_data_path = !!machine_data_path,
    exemplar_table_path = !!exemplar_table_path,
    fu_analysis_folder = !!fu_analysis_folder,
    reports_source_folders = !!reports_source_folders,
    reports_dest_folder = !!reports_dest_folder,

    # Load country concordance table
    CountryConcordanceTable = readxl::read_excel(country_concordance_path, sheet = "country_concordance_table"),

    # (1a) Grab all IEA data for ALL countries

    AllIEAData = iea_data_path %>% IEATools::load_tidy_iea_df(override_df = CountryConcordanceTable),
    IEAData = drake::target(AllIEAData %>%
                              extract_country_data(countries = alloc_and_eff_couns, max_year = max_year),
                            dynamic = map(alloc_and_eff_couns)),

    # (1b) Grab all CEDA data for ALL countries

    CEDAData = drake::target(CEDATools::create_agg_cru_cy_df(agg_cru_cy_folder = ceda_data_folder,
                                                             agg_cru_cy_metric = c("tmp", "tmn", "tmx"),
                                                             agg_cru_cy_year = 2020)),
    # (1c) Grab Machine data for ALL countries

    AllMachineData = read_all_eta_files(eta_fin_paths = get_eta_filepaths(machine_data_path)),
    MachineData = drake::target(AllMachineData %>%
                                  extract_country_data(countries = alloc_and_eff_couns, max_year = max_year),
                                dynamic = map(alloc_and_eff_couns)),

    # (2) Balance all final energy data.

    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    balanced_before = drake::target(IEAData %>%
                                      is_balanced(countries = alloc_and_eff_couns),
                                    dynamic = map(alloc_and_eff_couns)),
    # Balance all of the data by product and year.
    BalancedIEAData = drake::target(IEAData %>%
                                      make_balanced(countries = alloc_and_eff_couns),
                                    dynamic = map(alloc_and_eff_couns)),
    # Check that balancing was successful.
    balanced_after = drake::target(BalancedIEAData %>%
                                     is_balanced(countries = alloc_and_eff_couns),
                                   dynamic = map(alloc_and_eff_couns)),
    # Don't continue if there is a problem.
    # stopifnot returns NULL if everything is OK.
    OKToProceed = ifelse(is.null(stopifnot(all(balanced_after))), yes = TRUE, no = FALSE),

    # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.

    Specified = drake::target(BalancedIEAData %>%
                                specify(countries = alloc_and_eff_couns),
                              dynamic = map(alloc_and_eff_couns)),

    # (4) Arrange all the data into PSUT matrices with final stage data.

    PSUT_final = drake::target(Specified %>%
                                 make_psut(countries = countries),
                               dynamic = map(countries)),

    # (5) Load exemplar table and make lists for each country and year from disk.
    # These may be incomplete.

    ExemplarLists = drake::target(exemplar_table_path %>%
                                    load_exemplar_table(countries = alloc_and_eff_couns,
                                                        max_year = max_year) %>%
                                    exemplar_lists(alloc_and_eff_couns),
                                  dynamic = map(alloc_and_eff_couns)),

    # (5.5) Load phi (exergy-to-energy ratio) constants
    PhiConstants = drake::target(phi_constants_path %>%
                                    IEATools::load_phi_constants_table()),

    # (6) Load incomplete FU allocation tables

    IncompleteAllocationTables = drake::target(fu_analysis_folder %>%
                                                 load_fu_allocation_tables(specified_iea_data = Specified,
                                                                           countries = alloc_and_eff_couns),
                                               dynamic = map(alloc_and_eff_couns)),

    TidyIncompleteAllocationTables = drake::target(IncompleteAllocationTables %>%
                                                     IEATools::tidy_fu_allocation_table(),
                                                   dynamic = map(alloc_and_eff_couns)),

    # (7) Complete FU allocation tables

    CompletedAllocationTables = drake::target(assemble_fu_allocation_tables(incomplete_allocation_tables = IncompleteAllocationTables,
                                                                            exemplar_lists = ExemplarLists,
                                                                            specified_iea_data = Specified,
                                                                            countries = countries,
                                                                            max_year = max_year),
                                              dynamic = map(countries)),

    # (8) Complete efficiency tables

    CompletedEfficiencyTables = drake::target(assemble_eta_fu_tables(incomplete_eta_fu_tables = MachineData,
                                                                     exemplar_lists = ExemplarLists,
                                                                     completed_fu_allocation_tables = CompletedAllocationTables,
                                                                     countries = countries,
                                                                     max_year = max_year,
                                                                     which_quantity = IEATools::template_cols$eta_fu),
                                              dynamic = map(countries)),

    # (9) Complete phi_u tables

    CompletedPhiuTables = drake::target(assemble_phi_u_tables(incomplete_phi_u_table = MachineData,
                                                              phi_constants_table = PhiConstants,
                                                              completed_efficiency_table = CompletedEfficiencyTables,
                                                              countries = countries,
                                                              max_year = max_year),
                                        dynamic = map(countries)),


    # (9.5) Build matrices and vectors for extending to useful stage and exergy

    Cmats = drake::target(calc_C_mats(completed_allocation_tables = CompletedAllocationTables,
                                      countries = countries),
                          dynamic = map(countries)),

    EtafuPhiuvecs = drake::target(calc_eta_fu_phi_u_vecs(completed_efficiency_tables = CompletedEfficiencyTables,
                                                         completed_phi_tables = CompletedPhiuTables,
                                                         countries = countries),
                                  dynamic = map(countries)),

    Etafuvecs = drake::target(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                               keep = IEATools::template_cols$eta_fu,
                                               countries = countries),
                              dynamic = map(countries)),

    Phiuvecs = drake::target(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                              keep = IEATools::template_cols$phi_u,
                                              countries = countries),
                             dynamic = map(countries)),


    Phipfvecs = drake::target(calc_phi_pf_vecs(phi_u_vecs = Phiuvecs,
                                               phi_constants = PhiConstants,
                                               countries = countries),
                              dynamic = map(countries)),

    Phivecs = drake::target(sum_phi_vecs(phi_pf_vecs = Phipfvecs,
                                         phi_u_vecs = Phiuvecs,
                                         countries = countries),
                            dynamic = map(countries)),

    # (10) Extend to useful stage

    PSUT_useful = drake::target(move_to_useful(psut_final = PSUT_final,
                                               C_mats = Cmats,
                                               eta_phi_vecs = EtafuPhiuvecs,
                                               countries = countries),
                                dynamic = map(countries)),

    # (-) Add other methods


    # (-) Add exergy quantifications of energy

    PSUT_useful_exergy = drake::target(move_to_exergy(psut_energy = PSUT_useful,
                                                      phi_vecs = Phivecs,
                                                      countries = countries),
                                       dynamic = map(countries)),


    # (-) Off to the races!  Do other calculations:



    # (11) Final demand sectors

    FinalDemandSectors = drake::target(get_fd_sectors()),


    # (12) Primary industry prefixes

    PrimaryIndustryPrefixes = drake::target(get_p_industry_prefixes()),


    # (13a) Aggregate of primary energy/exergy by total (total energy supply (TES)), product, and flow

    AggregatePrimaryData = drake::target(calculate_primary_ex_data(.sutdata = PSUT_useful_exergy,
                                                                   p_industry_prefixes = PrimaryIndustryPrefixes)),



    # (13b) Aggregate final and useful energy/exergy by total (total final consumption (TFC)), product, and sector

    AggregateFinalUsefulData = drake::target(calculate_finaluseful_ex_data(.sutdata = PSUT_useful_exergy,
                                                                           fd_sectors = FinalDemandSectors)),



    # (14) Socioeconomic Data for selected countries

    SocioEconData = drake::target(get_all_pwt_data(countries = countries) %>%
                                    get_L_K_GDP_data()),




    # (N) Build reports

    # Build Allocation Graphs
    AllocationGraphs = drake::target(alloc_plots_df(CompletedAllocationTables, countries = countries),
                                     dynamic = map(countries)),

    # Build Non-Stationary Allocation Graphs
    NonStationaryAllocationGraphs = drake::target(nonstat_alloc_plots_df(CompletedAllocationTables, countries = countries),
                                     dynamic = map(countries)),

    # Build Efficiency Graphs
    EfficiencyGraphs = drake::target(eta_fu_plots_df(CompletedEfficiencyTables, countries = countries)),

    # Build Exergy-to-energy ratio graphs
    # ExergyEnergyGraphs = drake::target(phi_u_plots_df(CompletedEfficiencyTables, countries = countries))




    # reports_source_paths = drake::target(drake::file_in(report_source_paths(report_source_folders = report_source_folders))),
    # reports_dest_path = drake::target(drake::file_out(report_dest_paths(report_source_paths))),
    # reports_complete = drake::target(generate_reports(report_source_files = report_source_paths,
    #                                                   report_dest_folder = report_dest_folder))

  )
  if (how_far != "all_targets") {
    # Find the last row of the plan to keep.
    last_row_to_keep <- p %>%
      tibble::rowid_to_column(var = ".rownum") %>%
      dplyr::filter(.data[["target"]] == how_far) %>%
      dplyr::select(".rownum") %>%
      unlist() %>%
      unname()
    p <- p %>%
      dplyr::slice(1:last_row_to_keep)
  }
  return(p)
}


