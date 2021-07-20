#' Read a subtarget based on country
#'
#' It is sometimes convenient to read a target out of the drake cache by country (or countries).
#' Technically, this process involves reading a sub-target,
#' because the plans created by `get_plan()` are all mapped by country.
#' But subtargets are extracted based on country index in the cache rather than country name, and
#' it is cumbersome to keep track of cache indices for countries.
#' This function de-references the indices based on the value of `country` and extracts
#' the correct subtarget.
#'
#' @param target The name of the drake target as a string. See `get_plan()` for names of targets.
#' @param country The 3-letter ISO abbreviation (a string) of the country
#'                (or vector of 3-letter countries)
#'                for whom `target` is to be readd from the drake cache.
#' @param alloc_and_eff_couns_trgt See `SEAPSUTWorkflow::target_names`.
#' @param cache_path See `SEAPSUTWorkflow::cache_info`.
#'
#' @return the country-specific version of `target`
#'
#' @export
readd_by_country <- function(target,
                             country,
                             alloc_and_eff_couns_trgt = SEAPSUTWorkflow::target_names$alloc_and_eff_couns,
                             cache_path = SEAPSUTWorkflow::cache_info$cache_path) {
  known_countries <- drake::readd(alloc_and_eff_couns_trgt, path = cache_path, character_only = TRUE)
  country_indices <- which(known_countries %in% country, arr.ind = TRUE)
  drake::readd(target, path = cache_path, character_only = TRUE, subtargets = country_indices)
}


#' Pipe-amenable directory creation
#'
#' This function is small wrapper on `dir.create()`,
#' returning the path created.
#' If the directory creation process was unsuccessful, a warning is emitted.
#'
#' @param path a character vector containing a single path name. Tilde expansion is done.
#' @param showWarnings logical; should the warnings on failure be shown?
#' @param recursive logical. Should elements of the path other than the last be created?
#'                  If true, like the Unix command `mkdir -p`.
#' @param mode the mode to be used on Unix-alikes. Default is "0777".
#'
#' @return The `path` argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   td <- tempdir()
#'   dir_create_pipe(td, recursive = TRUE)
#'   unlink(td, force = TRUE, recursive = TRUE)
#' }
dir_create_pipe <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777") {
  success <- dir.create(path = path, showWarnings = showWarnings, recursive = recursive, mode = mode)
  if (!success & showWarnings) {
    warning(paste("Unsuccessful creation of directory: ", path))
  }
  return(path)
}


#' Set up directory structure for testing
#'
#' This is a private helper function that nobody should need to call,
#' except during automated testing.
#'
#' A typical way to use this function is
#'
#' ```
#' testing_setup <- SEAPSUTWorkflow:::set_up_for_testing()
#' tryCatch({
#'   # Make the plan in the temp_cache
#'   drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
#'   # Do stuff here, including test expectations
#' }, finally = {
#'   SEAPSUTWorkflow:::clean_up_after_testing(temp_cache = testing_setup$temp_cache,
#'                                            cache_path = testing_setup$cache_path)
#' })
#' ```
#'
#' @param countries The countries of interest. Default is `c("GHA", "ZAF")`.
#' @param max_year The last year to be analyzed. Default is `2000.`
#' @param how_far The last target to include in the drake plan. Default is "all_targets"
#' @param iea_data_path The path to IEA data. Default is `IEATools::sample_iea_data_path()`.
#' @param ceda_data_folder The path to CEDA data. Default is `CEDATools::ceda_data_folder()`.
#' @param `machine_data_path`: The path to the machine data excel files, supplied in the `machine_data_path` argument.
#' @param fu_analysis_folder The path to the final-to-useful analysis folder. Default is `tempdir()`.
#' @param fd_sectors Example final demand sectors for testing. Default is `c("Residential", "Transport")`.
#' @param p_industry_prefixes Example primary industry prefixes. Default is `c("Resources")`.
#' @param reports_output_folder The path into which reports will be written. Default is `tempdir()`.
#' @param exemplar_folder The path to a temporary folder to contain an exemplar table. Default is `tempdir()`.
#' @param machine_data_folder The path to a temporary folder to contain machine-specific data. Default is `tempdir()`.
#' @param cache_path The path to the temporary drake cache used for testing. Default is `tempfile("drake_cache_for_testing")`.
#' @param setup_exemplars Tells whether GHA allocation data should be adjusted to allow exemplars and
#'                        if ZAF allocations will be duplicated and called "World".
#'                        Default is `FALSE`.
#'
#' @return A list containing a drake plan (`$plan`),
#'         the path to the temporary drake cache (`$cache_path`), and
#'         the temporary drake cache itself (`$temp_cache`).
#'
#' @noRd
set_up_for_testing <- function(countries = c("GHA", "ZAF"),
                               additional_exemplar_countries = NULL,
                               max_year = 2000,
                               how_far = "all_targets",
                               iea_data_path = IEATools::sample_iea_data_path(),
                               ceda_data_folder = CEDATools::sample_ceda_data_folder(),
                               machine_data_path = system.file("extdata", "Machines - Data", package = "SEAPSUTWorkflow"),
                               fu_analysis_folder = tempdir(),
                               reports_source_folders = system.file("reports", package = "SEAPSUTWorkflow"),
                               reports_output_folder = tempdir(),
                               exemplar_folder = tempdir(),
                               # machine_data_folder = tempdir(),
                               machine_data_folder = file.path(tempdir(), "Machines - Data"),
                               cache_path = tempfile("drake_cache_for_testing"),
                               setup_exemplars = FALSE) {
  # We sometimes forget to include "World" when using exemplars.
  # This if statement avoids that problem.
  if (setup_exemplars & is.null(additional_exemplar_countries)) {
    additional_exemplar_countries = "World"
  }
  set_up_temp_analysis(fu_analysis_folder, exemplar_folder, machine_data_folder, reports_output_folder,
                       iea_data_path, machine_data_examples_path = machine_data_path,
                       setup_exemplars = setup_exemplars)
  plan <- get_plan(countries = countries,
                   additional_exemplar_countries = additional_exemplar_countries,
                   max_year = max_year,
                   how_far = how_far,
                   iea_data_path = file.path(fu_analysis_folder, "IEAData.csv"),
                   ceda_data_folder = ceda_data_folder,
                   machine_data_path = machine_data_folder,
                   exemplar_table_path = file.path(exemplar_folder, "Exemplar_Table.xlsx"),
                   fu_analysis_folder = fu_analysis_folder,
                   reports_source_folders = reports_source_folders,
                   reports_dest_folder = reports_output_folder)
  temp_cache <- drake::new_cache(path = cache_path)
  list(plan = plan, cache_path = cache_path, temp_cache = temp_cache)
}


#' Create files and data to mimic a final-to-useful analysis directory
#'
#' This function creates a temporary directory structure for final-to-useful analyses
#' in `fu_folder`.
#'
#' Sample data from the `IEATools` package are used when creating the directory structure.
#'
#' This function is helpful during testing, but not at any other times.
#'
#' @param fu_folder The folder in which the final-to-useful analysis structure will be created.
#'                  This should be a temporary directory.
#' @param exemplar_folder The folder in which a small exemplar table will be created.
#'                        This should be a temporary directory.
#' @param machine_data_folder The folder into which temporary machine data will be written.
#'                            This should be a temporary directory.
#' @param reports_output_folder The folder into which reports will be written.
#' @param iea_data_path The path to an IEA data file.
#' @param machine_data_path The path to a folder of existing machine data files.
#' @param setup_exemplars Tells whether GHA allocation data should be adjusted to allow exemplars and
#'                        if ZAF allocations will be duplicated and called "World".
#'                        Default is `FALSE`.
#' @param fin_eta The name of the tab in a MachineData Excel file that contains
#'                efficiency information.
#'                Default is `SEAPSUTWorkflow$machine_constants$efficiency_tab_name`.
#'
#' @return `NULL`. This function should be called for its side effect of creating a temporary final-to-useful directory structure.
#'
#' @noRd
set_up_temp_analysis <- function(fu_folder, exemplar_folder, machine_data_folder, reports_output_folder,
                                 iea_data_path, machine_data_examples_path,
                                 setup_exemplars = FALSE,
                                 fin_eta = SEAPSUTWorkflow::machine_constants$efficiency_tab_name) {
  # Set up IEA data
  iea_df <- IEATools::iea_df(iea_data_path)
  if (setup_exemplars) {
    # Set up IEA data for "World", using a copy of the ZAF data as world.
    world <- iea_df %>%
      dplyr::filter(.data[["COUNTRY"]] == "South Africa") %>%
      dplyr::mutate(
        COUNTRY = "World"
      )
    iea_df <- iea_df %>%
      dplyr::bind_rows(world)
  }
  # Write the iea_df to disk in the temporary directory (fu_folder)
  dir.create(fu_folder, showWarnings = FALSE)
  utils::write.csv(iea_df, file.path(fu_folder, "IEAData.csv"), row.names = FALSE)

  # Duplicate the Machines - Data directory from the SEAPSUTWorkflow example data
  # into the temporary directory.
  copied_correctly <- file.copy(from = machine_data_examples_path, to = fu_folder, recursive = TRUE)
  assertthat::assert_that(copied_correctly)

  # Get a list of all the files.
  files <- list.files(path = machine_data_folder, pattern = ".xlsx$", full.names = TRUE, recursive = TRUE)

  # Loop over all Machine data files.
  lapply(files, FUN = function(path) {
    # In each Machine data file, read the FIN_ETA tab, if it exists.
    all_sheets <- readxl::excel_sheets(path)
    if (fin_eta %in% all_sheets) {
      this_data <- openxlsx::read.xlsx(xlsxFile = path, sheet = fin_eta, startRow = 2)
      # Find the ZAF data, duplicate it, and set the country to World
      world_rows <- this_data %>%
        dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF") %>%
        dplyr::mutate(
          "{IEATools::iea_cols$country}" := "World"
        )
      # Do some math to find the bottom of the table.
      # First row is the date.
      # Second row is the header.
      # Then we have nrow(this_data) rows of the data.
      # So we should start writing World data on row
      first_world_row <- nrow(this_data) + 3
      # Verify that first_world_row is empty
      first_col <- openxlsx::read.xlsx(xlsxFile = path, sheet = fin_eta, cols = 1, colNames = FALSE)
      # Find out how many values in the first column
      # Verify that the first column is the right length
      assertthat::assert_that(nrow(first_col) + 1 == first_world_row)
      # Write the World data to the bottom of the sheet
      this_wb <- openxlsx::loadWorkbook(path)
      openxlsx::writeData(this_wb, sheet = fin_eta, x = world_rows,
                          startCol = 1, startRow = first_world_row,
                          colNames = FALSE, rowNames = FALSE)
      openxlsx::saveWorkbook(this_wb, file = path, overwrite = TRUE)
    }
  })

  # Eliminate wood cookstoves from GHA if we want to we're testing exemplars
  if (setup_exemplars) {
    # Create the file name for GHA cookstoves
    cookstoves_path <- file.path(machine_data_folder, "Wood_cookstoves", "Wood_cookstoves.xlsx")
    cookstoves_wb <- openxlsx::loadWorkbook(cookstoves_path)
    # Delete the row that contains efficiency data for wood cookstoves in GHA
    cookstove_fineta_worksheet <- openxlsx::read.xlsx(xlsxFile = cookstoves_wb, sheet = fin_eta, startRow = 2, colNames = TRUE)
    without_gha <- cookstove_fineta_worksheet %>%
      dplyr::filter(.data[[IEATools::iea_cols$country]] != "GHA")
    # Erase all the lines (except for the date) in this workbook.
    openxlsx::deleteData(cookstoves_wb, sheet = fin_eta, rows = 2:100, cols = 1:100, gridExpand = TRUE)
    # Write the new data into the workbook.
    openxlsx::writeData(cookstoves_wb, sheet = fin_eta, x = without_gha,
                        startCol = 1, startRow = 2,
                        colNames = TRUE, rowNames = FALSE)
    openxlsx::saveWorkbook(cookstoves_wb, file = cookstoves_path, overwrite = TRUE)
  }




  # # Set up the MachineData
  # # Establishes the path to the folder containing individual machine data files
  # # associated with the IEATools sample data for GHA and ZAF
  # eta_fin_sample_path <- system.file("extdata", "Machines - Data",
  #                                    package = "SEAPSUTWorkflow")
  # # Reads data from the machine files
  # etas <- read_all_eta_files(eta_fin_paths = eta_fin_sample_path)
  #
  # # Create a data frame for world etas
  # if (setup_exemplars) {
  #   # Create a World data set that is ZAF, only renamed.
  #   etas_World <- etas %>%
  #     dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF") %>%
  #     dplyr::mutate(
  #       "{IEATools::iea_cols$country}" := "World"
  #     )
  #   etas <- etas %>%
  #     dplyr::bind_rows(etas_World)
  # }

  # Create other temporary folders
  dir.create(exemplar_folder, showWarnings = FALSE)
  dir.create(reports_output_folder, showWarnings = FALSE)

  # Set up FU allocation tables. Need to split file that is stored in IEATools.
  GHAZAF_FU_allocation_tables <- IEATools::sample_fu_allocation_table_path() %>%
    openxlsx::read.xlsx()
  GHA_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  ZAF_FU_allocation_table <- GHAZAF_FU_allocation_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
  # Set up eta FU tables. Need to split file that is stored in IEATools.
  GHAZAF_eta_FU_tables <- IEATools::sample_eta_fu_table_path() %>%
    openxlsx::read.xlsx()
  GHA_FU_etas_table <- GHAZAF_eta_FU_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  ZAF_FU_etas_table <- GHAZAF_eta_FU_tables %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")

  if (setup_exemplars) {
    # Set up for using World as an exemplar for GHA.
    World_FU_allocation_table <- ZAF_FU_allocation_table %>%
      dplyr::mutate(
        "{IEATools::iea_cols$country}" := "World"
      )
    World_FU_etas_table <- ZAF_FU_etas_table %>%
      dplyr::mutate(
        "{IEATools::iea_cols$country}" := "World"
      )
    # Trim the GHA allocation table to make a missing row that will be filled by an exemplar.
    # Get rid of Residential Primary Solid biofuels consumption
    # so that it can be picked up from an exemplar country.
    GHA_FU_allocation_table <- GHA_FU_allocation_table %>%
      dplyr::filter(!(.data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels &
                        .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential))
    # Trim the GHA efficiency table to eliminate Wood cookstoves that produce MTH.100.C.
    # The efficiency of Wood stoves will be picked up from an exemplar country.
    GHA_FU_etas_table <- GHA_FU_etas_table %>%
      dplyr::filter(!(.data[[IEATools::template_cols$machine]] == "Wood cookstoves" &
                        .data[[IEATools::template_cols$eu_product]] == "MTH.100.C"))
  }

  # Build FU analysis workbooks for each country.
  GHA_fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(GHA_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name)
  openxlsx::writeData(GHA_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name, GHA_FU_allocation_table)
  openxlsx::addWorksheet(GHA_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name)
  openxlsx::writeData(GHA_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name, GHA_FU_etas_table)

  ZAF_fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(ZAF_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name)
  openxlsx::writeData(ZAF_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name, ZAF_FU_allocation_table)
  openxlsx::addWorksheet(ZAF_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name)
  openxlsx::writeData(ZAF_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name, ZAF_FU_etas_table)

  if (setup_exemplars) {
    World_fu_wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(World_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name)
    openxlsx::writeData(World_fu_wb, IEATools::fu_analysis_file_info$fu_allocation_tab_name, World_FU_allocation_table)
    openxlsx::addWorksheet(World_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name)
    openxlsx::writeData(World_fu_wb, IEATools::fu_analysis_file_info$eta_fu_tab_name, World_FU_etas_table)

  }

  # Write the workbooks back to the temp directory.
  dir.create(file.path(fu_folder, "GHA"), showWarnings = FALSE)
  dir.create(file.path(fu_folder, "ZAF"), showWarnings = FALSE)
  openxlsx::saveWorkbook(GHA_fu_wb, file = file.path(fu_folder, "GHA", "GHA FU Analysis.xlsx"), overwrite = TRUE)
  openxlsx::saveWorkbook(ZAF_fu_wb, file = file.path(fu_folder, "ZAF", "ZAF FU Analysis.xlsx"), overwrite = TRUE)
  if (setup_exemplars) {
    dir.create(file.path(fu_folder, "World"), showWarnings = FALSE)
    openxlsx::saveWorkbook(World_fu_wb, file = file.path(fu_folder, "World", "World FU Analysis.xlsx"), overwrite = TRUE)
  }

  # Create an exemplar table
  sample_exemplar_file <- sample_exemplar_table_path() %>%
    openxlsx::read.xlsx()
  # Fill with information for our example
  year_colnames <- sample_exemplar_file %>%
    IEATools::year_cols(return_names = TRUE)
  years_to_remove <- setdiff(year_colnames, c("1971", "2000"))
  if (setup_exemplars) {
    countries_to_keep <- c("GHA", "ZAF", "World")
  } else {
    countries_to_keep <- c("GHA", "ZAF")
  }
  small_exemplar_table <- sample_exemplar_file %>%
    # Keep only the years we want
    dplyr::select(!tidyselect::any_of(years_to_remove)) %>%
    # Keep only the countries we want
    dplyr::filter(.data[["2000"]] %in% countries_to_keep)
  # Write out to disk in our sample directory structure.
  exemplar_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(exemplar_wb, SEAPSUTWorkflow::exemplar_names$exemplar_tab_name)
  openxlsx::writeData(exemplar_wb, SEAPSUTWorkflow::exemplar_names$exemplar_tab_name, small_exemplar_table)
  openxlsx::saveWorkbook(exemplar_wb, file = file.path(exemplar_folder, "Exemplar_Table.xlsx"), overwrite = TRUE)

  return(NULL)
}


#' Clean up after testing with a temporary drake cache.
#'
#' This is a private helper function that nobody should need to call,
#' except during automated testing.
#'
#' A typical way to use this function is
#'
#' ```
#' testing_setup <- SEAPSUTWorkflow:::set_up_for_testing()
#' tryCatch({
#'   # Make the plan in the temp_cache
#'   drake::make(testing_setup$plan, cache = testing_setup$temp_cache, verbose = 0)
#'   # Do stuff here, including test expectations
#' }, finally = {
#'   SEAPSUTWorkflow:::clean_up_after_testing(temp_cache = testing_setup$temp_cache,
#'                                            cache_path = testing_setup$cache_path)
#' })
#' ```
#'
#' @param testing_setup A testing setup list generated by `set_up_for_testing()`.
#' @param cache_path The path to the temporary cache. Default is `testing_setup$cache_path`.
#' @param temp_cache The temporary cache itself. Default is `testing_setup$temp_cache`.
#'
#' @return `NULL`. This function should be called for its side effect of cleaning up after drake testing.
#'
#' @noRd
clean_up_after_testing <- function(testing_setup, cache_path = testing_setup$cache_path, temp_cache = testing_setup$temp_cache) {
  # Clean up the fu analysis folder
  fu_analysis_folder <- drake::readd(SEAPSUTWorkflow::target_names$fu_analysis_folder, path = cache_path, character_only = TRUE)
  if (file.exists(fu_analysis_folder)) {
    unlink(fu_analysis_folder, recursive = TRUE, force = TRUE)
  }
  # Clean up the cache.
  temp_cache$destroy()
  return(NULL)
}


#' Retrieve a list of final demand sectors
#'
#' Retrieve a list of final demand sectors for calculation of the total final consumption
#' of final, useful, or services energy in gross or net terms.
#'
#' @return A list of final demand sectors
#' @export
#'
#' @examples
#' fd_sectors <- get_fd_sectors()
get_fd_sectors <- function(){

  fd_sectors <- IEATools::fd_sectors

  return(fd_sectors)
}


#' Create a list containing final demand sectors
#'
#' This function creates a list equal to the length of any data frame supplied.
#' It is typically used on a data frame containing Physical Supply-Use Tables (PSUT)
#' with the associated final demand sectors in the nested `Y` and `U_EIOU` matrices.
#'
#' @param fd_sectors A character vector of final demand sectors.
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#'
#' @return A list the length of a desired data frame containing final demand vectors
#' @export
#'
#' @examples
#' library(Recca)
#' final_demand_sector_list <- create_fd_sectors_list(fd_sectors = c("Residential"),
#' .sutdata = Recca::UKEnergy2000mats)
create_fd_sectors_list <- function(fd_sectors, .sutdata) {

  fd_sectors_list <- rep(x = list(c(fd_sectors)), times = nrow(.sutdata))

  return(fd_sectors_list)

}


#' Retrieve primary industry prefixes
#'
#' Retrieve primary industry prefixes for use by `Recca::find_p_industry_names`.
#' Contains "Resources", "Imports", and "Stock changes".
#'
#' @return A list of primary industry prefixes
#' @export
#'
#' @examples
#' p_industry_prefixes <- get_p_industry_prefixes()
get_p_industry_prefixes <- function() {

  p_industry_prefixes <- IEATools::prim_agg_flows %>% unname() %>% unlist() %>% list()

  return(p_industry_prefixes)

}


#' Cleans drake targets
#'
#' It is often helpful to clean only a few targets.
#' This function allows specification of targets to be cleaned by names.
#' By default, this function cleans all targets *after* `IEAData`,
#' according to the list `SEAPSUTWorkflow::target_names`.
#'
#' @param to_clean The first target to clean. Default is "CEDAData", thereby cleaning everything after "IEAData".
#' @param first_cleaned_target The string name of the first target to clean. Default is "CEDAData".
#' @param last_cleaned_target The string name of the last target to clean. Default is the last target in the workflow.
#' @param path The path to the drake cache. Default is `NULL`, meaning that drake should look in the ".drake" folder for cache information.
#'
#' @return
#' @export
clean_targets <- function(to_clean = SEAPSUTWorkflow::target_names[seq(which(SEAPSUTWorkflow::target_names == first_cleaned_target),
                                                                        which(SEAPSUTWorkflow::target_names == last_cleaned_target))],
                          first_cleaned_target = "CEDAData",
                          last_cleaned_target = SEAPSUTWorkflow::target_names[[length(SEAPSUTWorkflow::target_names)]],
                          path = NULL) {
  drake::clean(list = to_clean, path = path)
}

