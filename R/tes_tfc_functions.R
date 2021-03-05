# This script contains functions which establish the following constants,
# and calculate the:
# Total energy supply (TES) of primary energy/exergy
# Total final consumption (TFC) of final and useful energy/exergy
# Primary-Final, Final-Useful, and Primary-Final efficiencies

# Must load matsbyname in this function file as Recca::finaldemand_aggregates depends on matsbyname functions
# but incuding matsbyname in imports is not enough to load the functions in matsbyname required.
library(matsbyname)


#' Calculate total energy supply
#'
#' Calculate the total energy supply (TES) in primary energy terms. This metric
#' was formerly called the total primary energy supply (TPES).
#' This function first uses the uses `Recca::find_p_industry_names()` function,
#' with a user-supplied set of primary industry prefixes `p_industry_prefixes`
#' to identify the primary industries desired for analysis.
#' The `Recca::primary_aggregates()` function is then applied to `.sutdata`
#' data frame by total, to calculate the total energy supply across all products and flows.
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices.
#' @param p_industry_prefixes A character vector of primary energy industry prefixes.
#'                            Usually "Resources", "Imports", and "Stock changes".
#' @param country,method,energy_type,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,p_ind_comp,p_ind_prefix,ex,ex_p See `SEAPSUTWorkflow::sea_cols`.
#' @param primary The string "Primary", representing the Primary stage of the energy conversion chain, see `IEATools::all_stages`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#' @param total The string "Total", indicating that calculate_p_ex_total calls `Recca::primary_aggregates` by total, i.e. across all products and flows
#'
#' @return A data frame containing aggregate primary energy/exergy data by total (total energy supply (TES))
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_total(.sutdata = Recca::UKEnergy2000mats,
#'                                             p_industry_prefixes = c("Resources", "Imports"))
calculate_p_ex_total <- function(.sutdata, p_industry_prefixes,
                                 country = IEATools::iea_cols$country,
                                 method = IEATools::iea_cols$method,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 year = IEATools::iea_cols$year,
                                 flow = IEATools::iea_cols$flow,
                                 e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                 stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                 gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                 agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                 p_ind_comp = SEAPSUTWorkflow::sea_cols$p_ind_comp,
                                 p_ind_prefix = SEAPSUTWorkflow::sea_cols$p_ind_prefix,
                                 ex = SEAPSUTWorkflow::sea_cols$ex,
                                 ex_p = SEAPSUTWorkflow::sea_cols$ex_p,
                                 primary = IEATools::all_stages$primary,
                                 all = "All",
                                 total = "Total"
                                 ) {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate("{p_ind_prefix}" := p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(.data[[p_ind_comp]], .after = .data[[p_ind_prefix]]) #

  # Removes duplicate entries. Primary energy/exergy data stored in R matrices
  # are the same for each of the final, useful and services stages
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .keep_all = TRUE) # Last.stage???

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_total <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                        p_industries = p_ind_comp,
                                        by = total) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .data[[ex_p]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, year, ex))

  # Add additional metadata columns
  p_total <- p_total %>%
    dplyr::mutate(
      "{stage_col}" := primary,
      "{gross_net}" := NA,
      "{e_product}" := all,
      "{flow}" := all,
      "{agg_by}" := total,
      "{ex}" := as.numeric(.data[[ex]])
    ) %>%
    dplyr::relocate(.data[[year]], .after = .data[[agg_by]]) %>%
    dplyr::relocate(.data[[ex]], .after = .data[[year]]) ##

  return(p_total)

}


#' Calculate total primary energy by product
#'
#' Calculate the total energy supply (TES) in primary energy terms by product. This metric
#' was formerly called the total primary energy supply (TPES).
#' This function first uses the uses `Recca::find_p_industry_names()` function,
#' with a user-supplied set of primary industry prefixes `p_industry_prefixes`
#' to identify the primary industries desired for analysis.
#' The `Recca::primary_aggregates()` function is then applied to `.sutdata`
#' data frame by product, to calculate the total energy supply across all flows
#' for each product.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices.
#' @param p_industry_prefixes A character vector of primary energy industry prefixes.
#'                            Usually "Resources", "Imports", and "Stock changes".
#' @param country,method,energy_type,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,p_ind_comp,p_ind_prefix,ex,ex_p See `SEAPSUTWorkflow::sea_cols`.
#' @param primary The string "Primary", representing the Primary stage of the energy conversion chain, see `IEATools::all_stages`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#' @param product The string "Product", indicating that calculate_p_ex_product calls `Recca::primary_aggregates` by product.
#'
#' @return A data frame containing aggregate primary energy/exergy data by product
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_product(.sutdata = Recca::UKEnergy2000mats,
#'                                               p_industry_prefixes = c("Resources", "Imports"))
#'
calculate_p_ex_product <- function(.sutdata, p_industry_prefixes,
                                   country = IEATools::iea_cols$country,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   year = IEATools::iea_cols$year,
                                   flow = IEATools::iea_cols$flow,
                                   e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                   stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                   gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                   agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                   p_ind_comp = SEAPSUTWorkflow::sea_cols$p_ind_comp,
                                   p_ind_prefix = SEAPSUTWorkflow::sea_cols$p_ind_prefix,
                                   ex = SEAPSUTWorkflow::sea_cols$ex,
                                   ex_p = SEAPSUTWorkflow::sea_cols$ex_p,
                                   primary = IEATools::all_stages$primary,
                                   all = "All",
                                   product = "Product"
) {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate("{p_ind_prefix}" := p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(.data[[p_ind_comp]], .after = .data[[p_ind_prefix]])

  # Removes duplicate entries. Primary energy/exergy is the same regardless of whether
  # it is at the final, useful, or services stage as it is calculated from the same matrices
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .keep_all = TRUE)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_product <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                         p_industries = p_ind_comp,
                                         by = product) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .data[[ex_p]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, year, ex))

  # Expands matrices
  p_product_expanded <- p_product %>%
    matsindf::expand_to_tidy(matvals = ex,
                             rownames = e_product) %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Add additional metadata columns
  p_product_expanded <- p_product_expanded %>%
    dplyr::mutate("{stage_col}" := primary,
                  "{gross_net}" := NA,
                  "{flow}" := all,
                  "{agg_by}" := product,
                  "{ex}" := as.numeric(.data[[ex]])) %>%
    dplyr::relocate(.data[[e_product]], .after = .data[[gross_net]]) %>%
    dplyr::relocate(.data[[year]], .after = .data[[agg_by]]) %>%
    dplyr::relocate(.data[[ex]], .after = .data[[year]])

  return(p_product_expanded)

}

#' Calculate total primary energy by flow
#'
#' Calculate the total energy supply (TES) in primary energy terms by flow. This metric
#' was formerly called the total primary energy supply (TPES).
#' This function first uses the uses `Recca::find_p_industry_names()` function,
#' with a user-supplied set of primary industry prefixes `p_industry_prefixes`
#' to identify the primary industries desired for analysis.
#' The `Recca::primary_aggregates()` function is then applied to `.sutdata`
#' data frame by flow, to calculate the total energy supply across all products
#' for each flow.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices.
#' @param p_industry_prefixes A character vector of primary energy industry prefixes.
#'                            Usually "Resources", "Imports", and "Stock changes".
#' @param country,method,energy_type,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,p_ind_comp,p_ind_prefix,ex,ex_p See `SEAPSUTWorkflow::sea_cols`.
#' @param primary The string "Primary", representing the Primary stage of the energy conversion chain, see `IEATools::all_stages`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#'
#' @return A data frame containing aggregate primary energy/exergy data by flow
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_flow(.sutdata = Recca::UKEnergy2000mats,
#'                                            p_industry_prefixes = c("Resources", "Imports"))
#'
calculate_p_ex_flow <- function(.sutdata, p_industry_prefixes,
                                country = IEATools::iea_cols$country,
                                method = IEATools::iea_cols$method,
                                energy_type = IEATools::iea_cols$energy_type,
                                year = IEATools::iea_cols$year,
                                flow = IEATools::iea_cols$flow,
                                e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                p_ind_comp = SEAPSUTWorkflow::sea_cols$p_ind_comp,
                                p_ind_prefix = SEAPSUTWorkflow::sea_cols$p_ind_prefix,
                                ex = SEAPSUTWorkflow::sea_cols$ex,
                                ex_p = SEAPSUTWorkflow::sea_cols$ex_p,
                                primary = IEATools::all_stages$primary,
                                all = "All") {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate("{p_ind_prefix}" := p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(.data[[p_ind_comp]], .after = .data[[p_ind_comp]])

  # Removes duplicate entries. Primary energy/exergy is the same regardless of whether
  # it is at the final, useful, or services stage as it is calculated from the same matrices
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .keep_all = TRUE)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_flow <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                      p_industries = p_ind_comp,
                                      by = flow) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .data[[ex_p]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, year, ex))

  # Expands matrices
  p_flow_expanded <- p_flow %>%
    matsindf::expand_to_tidy(matvals = ex,
                             colnames = flow) %>%
    dplyr::select(-rownames, -rowtypes, -coltypes)

  # Add additional metadata columns
  p_flow_expanded <- p_flow_expanded %>%
    dplyr::mutate("{stage_col}" := primary,
                  "{gross_net}" := NA,
                  "{e_product}" := all,
                  "{agg_by}" := Flow,
                  "{ex}" := as.numeric(.data[[ex]])) %>%
    dplyr::relocate(.data[[flow]], .after = .data[[e_product]]) %>%
    dplyr::relocate(.data[[year]], .after = .data[[agg_by]]) %>%
    dplyr::relocate(.data[[ex]], .after = .data[[year]])

  return(p_flow_expanded)

}

#' Calculate total final consumption of final and useful energy
#'
#' Calculate the total final consumption (TFC) at the final and useful stages
#' (along with any additional stages).
#' This function first uses the uses `create_fd_sectors_list()` function,
#' with a user-supplied set of final demand sectors `fd_sectors`
#' to identify the final demand sectors desired for analysis.
#' The `Recca::finaldemand_aggregates()` function is then applied to `.sutdata`
#' data frame, to calculate the total final consumption across all products and sectors.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param fd_sectors A character vector of final demand sectors.
#'
#' @param country,method,energy_type,last_stage,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,ex,ex_net,ex_gross See `SEAPSUTWorkflow::sea_cols`.
#' @param net The string "Net" indicating that the final demand aggregate values include only non-energy industry own use sectors supplied in `fd-sectors`.
#' @param gross The string "Gross" indicating that the final demand aggregate value includes all sectors stipulated in supplied in `fd_sectors`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#' @param total The string "Total", indicating that calculate_fu_ex_total calls `Recca::finaldemand_aggregates` by total.
#'
#' @return A data frame containing aggregate final and useful energy/exergy data by total
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_total <- calculate_fu_ex_total(.sutdata = Recca::UKEnergy2000mats,
#'                                    fd_sectors = c("Residential"))
#'
calculate_fu_ex_total <- function(.sutdata, fd_sectors,
                                  country = IEATools::iea_cols$country,
                                  method = IEATools::iea_cols$method,
                                  energy_type = IEATools::iea_cols$energy_type,
                                  last_stage = IEATools::iea_cols$last_stage,
                                  year = IEATools::iea_cols$year,
                                  sector = "Sector",
                                  fd_sectors_col = SEAPSUTWorkflow::sea_cols$fd_sectors_col,
                                  e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                  stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                  gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                  agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                  ex = SEAPSUTWorkflow::sea_cols$ex,
                                  ex_net = SEAPSUTWorkflow::sea_cols$ex_net,
                                  ex_gross = SEAPSUTWorkflow::sea_cols$ex_gross,
                                  net = "Net",
                                  gross = "Gross",
                                  all = "All",
                                  total = "Total"
                                  ) {

  library(matsbyname)

  # Creates a list of the final demand sector list equal to the length of the supplied data frame
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate(
      "{fd_sectors_col}" := fd_sector_list
    )

  # Calculates final demand by total
  fu_total <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = fd_sectors_col, by = total) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ex_net]], .data[[ex_gross]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, stage, year, ex_net, ex_gross)) %>%
    tidyr::pivot_longer(cols = ex_net:ex_gross, # EX.d_net:EX.d_gross # .data[[ex_net:ex_gross]] # .data[[ex_net]]:.data[[ex_gross]]
                        names_to = gross_net,
                        values_to = ex) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_net, net)) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_gross, gross)) %>%
    dplyr::relocate(.data[[gross_net]], .after = .data[[stage_col]])

  # Add additional metadata columns
  fu_total <- fu_total %>%
    dplyr::mutate("{e_product}" := all,
                  "{sector}" := all,
                  "{agg_by}" := total,
                  "{ex}" := as.numeric(.data[[ex]]))
  # %>%
  #   dplyr::relocate(.data[[year]], .after = .data[[agg_by]]) %>%
  #   dplyr::relocate(.data[[ex]], .after = .data[[year]])

  return(fu_total)

}

#' Calculate total final consumption of final and useful energy by product
#'
#' Calculate the total final consumption (TFC) at the final and useful stages
#' (along with any additional stages) by product.
#' This function first uses the uses `create_fd_sectors_list()` function,
#' with a user-supplied set of final demand sectors `fd_sectors`
#' to identify the final demand sectors desired for analysis.
#' The `Recca::finaldemand_aggregates()` function is then applied to `.sutdata`
#' data frame by product, to calculate the total final consumption across
#' all of the sectors supplied in `fd_sectors` for each product.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param fd_sectors A character vector of final demand sectors.
#'
#' @param country,method,energy_type,last_stage,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,ex,ex_net,ex_gross See `SEAPSUTWorkflow::sea_cols`.
#' @param net The string "Net" indicating that the final demand aggregate values include only non-energy industry own use sectors supplied in `fd-sectors`.
#' @param gross The string "Gross" indicating that the final demand aggregate value includes all sectors stipulated in supplied in `fd_sectors`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#' @param product The string "Product", indicating that calculate_fu_ex_product calls `Recca::finaldemand_aggregates` by product.
#'
#' @return A data frame containing aggregate final and useful energy/exergy data by product
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_product <- calculate_fu_ex_product(.sutdata = Recca::UKEnergy2000mats,
#'                                        fd_sectors = c("Residential"))
#'
calculate_fu_ex_product <- function(.sutdata, fd_sectors,
                                    country = IEATools::iea_cols$country,
                                    method = IEATools::iea_cols$method,
                                    energy_type = IEATools::iea_cols$energy_type,
                                    last_stage = IEATools::iea_cols$last_stage,
                                    year = IEATools::iea_cols$year,
                                    sector = "Sector",
                                    fd_sectors_col = SEAPSUTWorkflow::sea_cols$fd_sectors_col,
                                    e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                    stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                    gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                    agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                    ex = SEAPSUTWorkflow::sea_cols$ex,
                                    ex_net = SEAPSUTWorkflow::sea_cols$ex_net,
                                    ex_gross = SEAPSUTWorkflow::sea_cols$ex_gross,
                                    net = "Net",
                                    gross = "Gross",
                                    all = "All",
                                    product = "Product") {

  library(matsbyname)

  # Creates a list of final demand sectors
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate("{fd_sectors_col}" := fd_sector_list)

  # Calculates final demand by _product
  fu_product <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = fd_sectors_col, by = product) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ex_net]], .data[[ex_gross]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, stage, year, ex_net, ex_gross)) %>%
    tidyr::pivot_longer(cols = ex_net:ex_gross, # EX.d_net:EX.d_gross # .data[[ex_net:ex_gross]] # .data[[ex_net]]:.data[[ex_gross]]
                        names_to = gross_net,
                        values_to = ex) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_net, net)) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_gross, gross)) %>%
    dplyr::relocate(.data[[gross_net]], .after = .data[[stage_col]])

  # Expands matrices
  fu_product_expanded <- fu_product %>%
    matsindf::expand_to_tidy(matvals = ex,
                             rownames = e_product) %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Add additional metadata columns
  fu_product_expanded <- fu_product_expanded %>%
    dplyr::mutate("{sector}" := all,
                  "{agg_by}" = product,
                  "{ex}" := as.numeric(.data[[ex]]))

    # dplyr::relocate(.data[[e_product]], .after = .data[[gross_net]])


  return(fu_product_expanded)

}

#' Calculate total final consumption of final and useful energy by sector
#'
#' Calculate the total final consumption (TFC) at the final and useful stages
#' (along with any additional stages) by sector.
#' This function first uses the uses `create_fd_sectors_list()` function,
#' with a user-supplied set of final demand sectors `fd_sectors`
#' to identify the final demand sectors desired for analysis.
#' The `Recca::finaldemand_aggregates()` function is then applied to `.sutdata`
#' data frame by sector, to calculate the total final consumption of all products
#' in each of the sectors supplied in `fd_sectors`.
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param fd_sectors A character vector of final demand sectors.
#'
#' @param country,method,energy_type,last_stage,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,ex,ex_net,ex_gross See `SEAPSUTWorkflow::sea_cols`.
#' @param net The string "Net" indicating that the final demand aggregate values include only non-energy industry own use sectors supplied in `fd-sectors`.
#' @param gross The string "Gross" indicating that the final demand aggregate value includes all sectors stipulated in supplied in `fd_sectors`.
#' @param all The string "All", indicating that one or both of `flow` and `product` were aggregated.
#' @param sector The string "Product", indicating that calculate_fu_ex_sector calls `Recca::finaldemand_aggregates` by sector.
#'
#' @return A data frame containing total final and useful consumption by sector
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_sector <- calculate_fu_ex_sector(.sutdata = Recca::UKEnergy2000mats,
#'                                      fd_sectors = c("Residential"))
#'
calculate_fu_ex_sector <- function(.sutdata, fd_sectors,
                                   country = IEATools::iea_cols$country,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   last_stage = IEATools::iea_cols$last_stage,
                                   year = IEATools::iea_cols$year,
                                   sector = "Sector",
                                   fd_sectors_col = SEAPSUTWorkflow::sea_cols$fd_sectors_col,
                                   e_product = SEAPSUTWorkflow::sea_cols$e_product,
                                   stage_col = SEAPSUTWorkflow::sea_cols$stage_col,
                                   gross_net = SEAPSUTWorkflow::sea_cols$gross_net,
                                   agg_by = SEAPSUTWorkflow::sea_cols$agg_by,
                                   ex = SEAPSUTWorkflow::sea_cols$ex,
                                   ex_net = SEAPSUTWorkflow::sea_cols$ex_net,
                                   ex_gross = SEAPSUTWorkflow::sea_cols$ex_gross,
                                   net = "Net",
                                   gross = "Gross",
                                   all = "All") {

  library(matsbyname)

  # Creates a list of final demand sectors
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate("{fd_sectors_col}" := fd_sector_list)

  # Calculates final demand by _product
  fu_sector <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = fd_sectors_col, by = sector) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ex_net]], .data[[ex_gross]]) %>%
    magrittr::set_colnames(c(country, method, energy_type, stage, year, ex_net, ex_gross)) %>%
    tidyr::pivot_longer(cols = ex_net:ex_gross, # EX.d_net:EX.d_gross # .data[[ex_net:ex_gross]] # .data[[ex_net]]:.data[[ex_gross]]
                        names_to = gross_net,
                        values_to = ex) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_net, net)) %>%
    dplyr::mutate("{gross_net}" := stringr::str_replace(.data[[gross_net]], ex_gross, gross)) %>%
    dplyr::relocate(.data[[gross_net]], .after = .data[[stage]])

  # Expands matrices
  fu_sector_expanded <- fu_sector %>%
    matsindf::expand_to_tidy(matvals = ex,
                             rownames = sector) %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Asserts that the length of the character vector containing the sectors present
  # in the expanded data is less than or equal to the length of fd_sectors.
  assertthat::assert_that(length(unique(fu_sector_expanded$Sector)) <= length(fd_sectors),
                          msg = "There are more final demand sectors present than stipulated in fd_sectors")

  # Asserts that the final demand sectors present in fu_sector_expanded are present in fd_sectors
  assertthat::assert_that(isTRUE(unique(unique(fu_sector_expanded$Sector) %in% fd_sectors)),
                          msg = "There are final demand sectors present that were not stipulated in fd_sectors")

  # Add additional metadata columns
  fu_sector_expanded <- fu_sector_expanded %>%
    dplyr::mutate("{e_product}" := all,
                  "{agg_by}" := sector,
                  "{ex}" := as.numeric(.data[[ex]]))


    # dplyr::relocate(Sector, .after = "Gross.Net") %>%

  return(fu_sector_expanded)

}


#' Create a data frame containing primary aggregate energy/exergy data
#'
#' This functions creates a single data frame containing the total energy/exergy by country,
#' year, method, energy quantification, and grouping variable (Total, Product, and Flow),
#' for the Primary stage using the functions:
#' `calculate_p_ex_total`, `calculate_p_ex_flow`, `calculate_p_ex_product`,
#' and binding the outputs of these functions into a single data frame.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param p_industry_prefixes A character vector of primary energy industry prefixes.
#'                            Usually "Resources", "Imports", and "Stock changes".
#'
#' @return A data frame containing primary energy/exergy values aggregated by total,
#'         flow and product.
#' @export
#'
#' @examples
#' library(Recca)
#' all_data <- calculate_primary_ex_data(.sutdata = Recca::UKEnergy2000mats,
#'                                       p_industry_prefixes = c("Resources"))
#'
calculate_primary_ex_data <- function(.sutdata, p_industry_prefixes) {

  library(matsbyname)

  # Calculates total primary energy/exergy
  p_total <- calculate_p_ex_total(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes)

  # Calculates primary energy/exergy by flow
  p_flow <- calculate_p_ex_flow(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes)

  # Calculates primary energy/exergy by product
  p_product <- calculate_p_ex_product(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes)

  # Bind all data together
  all_data <- p_total %>%
    rbind(p_flow) %>%
    rbind(p_product)

  # Set EX column type to numeric
  all_data$EX <- as.numeric(all_data$EX)

  return(all_data)

}


#' Create a data frame containing primary aggregate energy/exergy data
#'
#' This functions creates a single data frame containing final and useful
#' energy/exergy by country, year, method, energy quantification,
#' and grouping variable (Total, Product, and Sector) using the functions:
#' `calculate_fu_ex_total`, `calculate_fu_ex_sector`, `calculate_fu_ex_product`
#' and binding the outputs of these functions into a single data frame.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param fd_sectors A character vector of final demand sectors.
#'
#' @return A data frame containing final and useful energy/exergy values aggregated by total,
#'         sector and product.
#' @export
#'
#' @examples
#' library(Recca)
#' all_data <- calculate_all_ex_data(.sutdata = Recca::UKEnergy2000mats,
#'                                   fd_sectors = c("Residential"))
#'
calculate_finaluseful_ex_data <- function(.sutdata, fd_sectors) {

  library(matsbyname)

  # Calculates total final demand of energy/exergy
  fu_total <- calculate_fu_ex_total(.sutdata = .sutdata, fd_sectors = fd_sectors)

  # Calculates final demand of energy/exergy by sector
  fu_sector <- calculate_fu_ex_sector(.sutdata = .sutdata, fd_sectors = fd_sectors)

  # Calculates final demand of energy/exergy by product
  fu_product <- calculate_fu_ex_product(.sutdata = .sutdata, fd_sectors = fd_sectors)

  # Bind all data together
  all_data <- fu_total %>%
    rbind(fu_sector) %>%
    rbind(fu_product)

  # Set EX column type to numeric
  all_data$EX <- as.numeric(all_data$EX)

  return(all_data)

}

