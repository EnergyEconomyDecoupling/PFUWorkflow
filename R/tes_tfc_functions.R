# This script contains functions which establish the following constants,
# and calculate the following metrics:
# Final demand sectors
# Primary industries
# Total energy supply (TES) of primary energy/exergy
# Total final consumption (TFC) of final and useful energy/exergy
# Primary-Final, Final-Useful, and Primary-Final efficiencies

# Must load matsbyname in this function file as Recca::finaldemand_aggregates depends on matsbyname functions
# but incuding matsbyname in imports is not enough to load the functions in matsbyname required.
library(matsbyname)


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
#'
create_fd_sectors_list <- function(fd_sectors, .sutdata) {

  rep(x = list(c(fd_sectors)), times = nrow(.sutdata))

}

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
#'                            Usually "Resources", "Production", "Imports", and "Stock changes".
#' @param country,method,energy_type,year,flow See `IEATools::iea_cols`.
#' @param e_product,stage_col,gross_net,agg_by,p_ind_comp,p_ind_prefix,ex,ex_p See `SEAPSUTWorkflow::sea_cols`.
#' @param primary ???
#'
#' @return A data frame containing total energy supply data
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_total(.sutdata = Recca::UKEnergy2000mats,
#' p_industry_prefixes = c("Resources", "Imports"))
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
                                 primary = "Primary",
                                 all = "All",
                                 total = "Total"
                                 ) {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate("{p_ind_prefix}" := p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(.data[[p_ind_comp]], .after = p_ind_prefix)

  # Removes duplicate entries. Primary energy/exergy data stored in R matrices
  # are the same for each of the final, useful and services stages
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(.data[[country]], .data[[method]], .data[[energy_type]], .data[[year]], .keep_all = TRUE)

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
      # Sets EX column type to numeric
      "{ex}" := as.numeric(.data[[ex]])
    ) %>%
    dplyr::relocate(year, .after = agg_by) %>%
    dplyr::relocate(ex, .after = year)

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
#'                            Usually "Resources", "Production", "Imports", and "Stock changes".
#'
#' @return A data frame containing total energy supply data by primary energy product
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_product(.sutdata = Recca::UKEnergy2000mats,
#' p_industry_prefixes = c("Resources", "Imports"))
#'
calculate_p_ex_product <- function(.sutdata, p_industry_prefixes) {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate(p_industry_prefixes = p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(p_industries_complete, .after = p_industry_prefixes)

  # Removes duplicate entries. Primary energy/exergy is the same regardless of whether
  # it is at the final, useful, or services stage as it is calculated from the same matrices
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(Country, Method, Energy.type, Year, .keep_all = TRUE)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_product <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                         p_industries = "p_industries_complete",
                                         by = "Product") %>%
    dplyr::select(Country, Method, Energy.type, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Year", "EX"))

  # Expands matrices
  p_product_expanded <- p_product %>%
    matsindf::expand_to_tidy(matvals = "EX",
                             rownames = "Product") %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Add additional metadata columns
  p_product_expanded <- p_product_expanded %>%
    dplyr::mutate(Stage = "Primary", .after = "Energy.type") %>%
    dplyr::mutate(Gross.Net = "Gross", .after = "Stage") %>%
    dplyr::mutate(Flow = "Total", .before = "Year") %>%
    dplyr::relocate(Product, .after = "Gross.Net") %>%
    dplyr::mutate(Grouping = "Product", .after = "Flow")

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
#'                            Usually "Resources", "Production", "Imports", and "Stock changes".
#'
#' @return A data frame containing total energy supply data by primary energy flow
#' @export
#'
#' @examples
#' library(Recca)
#' total_energy_supply <- calculate_p_ex_flow(.sutdata = Recca::UKEnergy2000mats,
#' p_industry_prefixes = c("Resources", "Imports"))
#'
calculate_p_ex_flow <- function(.sutdata, p_industry_prefixes) {

  library(matsbyname)

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- .sutdata %>%
    dplyr::mutate(p_industry_prefixes = p_industry_prefixes) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(p_industries_complete, .after = p_industry_prefixes)

  # Removes duplicate entries. Primary energy/exergy is the same regardless of whether
  # it is at the final, useful, or services stage as it is calculated from the same matrices
  PSUT_DF_p <- PSUT_DF_p %>%
    dplyr::distinct(Country, Method, Energy.type, Year, .keep_all = TRUE)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_flow <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                      p_industries = "p_industries_complete",
                                      by = "Flow") %>%
    dplyr::select(Country, Method, Energy.type, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Year", "EX"))

  # Expands matrices
  p_flow_expanded <- p_flow %>%
    matsindf::expand_to_tidy(matvals = "EX",
                             colnames = "Flow") %>%
    dplyr::select(-rownames, -rowtypes, -coltypes)

  # Add additional metadata columns
  p_flow_expanded <- p_flow_expanded %>%
    dplyr::mutate(Stage = "Primary", .after = "Energy.type") %>%
    dplyr::mutate(Gross.Net = "Gross", .after = "Stage") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::relocate(Flow, .after = "Product") %>%
    dplyr::mutate(Grouping = "Flow", .after = "Flow")

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
#' @return A data frame containing total final and useful consumption by total
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_total <- calculate_fu_ex_total(.sutdata = Recca::UKEnergy2000mats,
#'                                    fd_sectors = c("Residential"))
#'
calculate_fu_ex_total <- function(.sutdata, fd_sectors) {

  library(matsbyname)

  # Creates a list of final demand sectors
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate(fd_sectors = fd_sector_list)

  # Calculates final demand by total
  fu_total <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = "fd_sectors", by = "Total") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.d_net, EX.d_gross) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX.d_net", "EX.d_gross")) %>%
    tidyr::pivot_longer(cols = EX.d_net:EX.d_gross,
                        names_to = "Gross.Net",
                        values_to = "EX") %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_net", "Net")) %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_gross", "Gross")) %>%
    dplyr::relocate(Gross.Net, .after = "Stage")

  # Add additional metadata columns
  fu_total <- fu_total %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::mutate(Sector = "Total", .after = "Product") %>%
    dplyr::mutate(Grouping = "Total", .after = "Sector")

  # Sets EX column type to numeric
  fu_total$EX <- as.numeric(fu_total$EX)

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
#' @return A data frame containing total final and useful consumption by product
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_product <- calculate_fu_ex_product(.sutdata = Recca::UKEnergy2000mats,
#'                                        fd_sectors = c("Residential"))
#'
calculate_fu_ex_product <- function(.sutdata, fd_sectors) {

  library(matsbyname)

  # Creates a list of final demand sectors
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate(fd_sectors = fd_sector_list)

  # Calculates final demand by _product
  fu_product <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = "fd_sectors", by = "Product") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.d_net, EX.d_gross) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX.d_net", "EX.d_gross")) %>%
    tidyr::pivot_longer(cols = EX.d_net:EX.d_gross,
                        names_to = "Gross.Net",
                        values_to = "EX") %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_net", "Net")) %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_gross", "Gross")) %>%
    dplyr::relocate(Gross.Net, .after = "Stage")

  # Expands matrices
  fu_product_expanded <- fu_product %>%
    matsindf::expand_to_tidy(matvals = "EX",
                             rownames = "Product") %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Add additional metadata columns
  fu_product_expanded <- fu_product_expanded %>%
    dplyr::relocate(Product, .after = "Gross.Net") %>%
    dplyr::mutate(Sector = "Total", .after = "Product") %>%
    dplyr::mutate(Grouping = "Product", .after = "Sector")

  # Sets EX column type to numeric
  fu_product_expanded$EX <- as.numeric(fu_product_expanded$EX)

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
#' @return A data frame containing total final and useful consumption by sector
#' @export
#'
#' @examples
#' library(Recca)
#' tfc_sector <- calculate_fu_ex_sector(.sutdata = Recca::UKEnergy2000mats,
#'                                      fd_sectors = c("Residential"))
#'
calculate_fu_ex_sector <- function(.sutdata, fd_sectors) {

  library(matsbyname)

  # Creates a list of final demand sectors
  fd_sector_list <- create_fd_sectors_list(fd_sectors = fd_sectors, .sutdata = .sutdata)

  # Adds a column which each observation containing the list of final demand sectors
  PSUT_DF_fu <- .sutdata %>%
    dplyr::mutate(fd_sectors = fd_sector_list)

  # Calculates final demand by _product
  fu_sector <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = "fd_sectors", by = "Sector") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.d_net, EX.d_gross) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX.d_net", "EX.d_gross")) %>%
    tidyr::pivot_longer(cols = EX.d_net:EX.d_gross,
                        names_to = "Gross.Net",
                        values_to = "EX") %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_net", "Net")) %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_gross", "Gross")) %>%
    dplyr::relocate(Gross.Net, .after = "Stage")

  # Expands matrices
  fu_sector_expanded <- fu_sector %>%
    matsindf::expand_to_tidy(matvals = "EX",
                             rownames = "Sector") %>%
    dplyr::select(-colnames, -rowtypes, -coltypes)

  # Asserts that the length of the character vector containing the sectors present
  # in the expanded data is less than or equal to the length of fd_sectors.
  assertthat::assert_that(length(unique(fu_sector_expanded$Sector)) <= length(fd_sectors),
                          msg = "There are more final demand sectors present than stipulated in fd_sectors")

  # # Asserts that the final demand sectors present in fu_sector_expanded are present in fd_sectors
  assertthat::assert_that(isTRUE(unique(unique(fu_sector_expanded$Sector) %in% fd_sectors)),
                          msg = "There are final demand sectors present that were not stipulated in fd_sectors")

  # Add additional metadata columns
  fu_sector_expanded <- fu_sector_expanded %>%
    dplyr::relocate(Sector, .after = "Gross.Net") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::mutate(Grouping = "Sector", .after = "Sector")

  # Sets EX column type to numeric
  fu_sector_expanded$EX <- as.numeric(fu_sector_expanded$EX)

  return(fu_sector_expanded)

}


#' Create a data frame containing all aggregate energy/exergy data
#'
#' This functions creates a single data frame containing the total energy by country,
#' year, method, energy quantification, stage (Primary, Final, Useful....),
#' and grouping variable (Total, Product, and Flow or Sector) using the functions:
#' `calculate_fu_ex_total`, `calculate_fu_ex_sector`, `calculate_fu_ex_product`,
#' `calculate_p_ex_total`, `calculate_p_ex_flow`, `calculate_p_ex_product`,
#' and binding the outputs of these functions into a single data frame.
#'
#'
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#' @param fd_sectors A character vector of final demand sectors.
#' @param p_industry_prefixes A character vector of primary energy industry prefixes.
#'                            Usually "Resources", "Production", "Imports", and "Stock changes".
#'
#' @return A data frame containing energy/exergy values aggregated by total,
#'         sector and product at each stage of the energy conversion chain.
#' @export
#'
#' @examples
#' library(Recca)
#' all_data <- calculate_all_ex_data(.sutdata = Recca::UKEnergy2000mats,
#'                                   fd_sectors = c("Residential"),
#'                                   p_industry_prefixes = c("Resources"))
#'
calculate_all_ex_data <- function(.sutdata, fd_sectors, p_industry_prefixes) {

  library(matsbyname)

  # Calculates total final demand of energy/exergy
  fu_total <- calculate_fu_ex_total(.sutdata = .sutdata, fd_sectors = fd_sectors) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))

  # Calculates final demand of energy/exergy by sector
  fu_sector <- calculate_fu_ex_sector(.sutdata = .sutdata, fd_sectors = fd_sectors) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))

  # Calculates final demand of energy/exergy by product
  fu_product <- calculate_fu_ex_product(.sutdata = .sutdata, fd_sectors = fd_sectors) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))

  # Calculates total primary energy/exergy
  p_total <- calculate_p_ex_total(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))
  # Calculates primary energy/exergy by flow
  p_flow <- calculate_p_ex_flow(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))
  # Calculates primary energy/exergy by product
  p_product <- calculate_p_ex_product(.sutdata = .sutdata, p_industry_prefixes = p_industry_prefixes) %>%
    # Change name from Flow to Flow.Sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Flow.Sector",
                             "Grouping", "Year", "EX"))

  # Bind all data together
  all_data <- fu_total %>%
    rbind(fu_sector) %>%
    rbind(fu_product) %>%
    rbind(p_total) %>%
    rbind(p_flow) %>%
    rbind(p_product)

  # Set EX column type to numeric
  all_data$EX <- as.numeric(all_data$EX)

  return(all_data)

}

