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


#' Create a list containing final demand sectors equal to the length of a dataframe
#'
#' @param fd_sectors A vector of final demand sectors, usually produced by
#'                   `create_fd_sectors`
#' @param PSUT_DF  A data frame containing PSUT matrices that require associated
#'                 final demand sector names
#'
#' @return A list the length of a desired data frame containing final demand vectors
#' @export
#'
#' @examples
create_fd_sectors_list <- function(fd_sectors, .sutdata) {

  rep(x = list(c(fd_sectors)), times = nrow(.sutdata))

}



#' Calculate total primary energy
#'
#'
#' @param .sutdata
#' @param p_industry_prefixes
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_total <- function(.sutdata, p_industry_prefixes) {

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
  p_total <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                        p_industries = "p_industries_complete",
                                        by = "Total") %>%
    dplyr::select(Country, Method, Energy.type, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Year", "EX"))

  # Add additional metadata columns
  p_total <- p_total %>%
    dplyr::mutate(Stage = "Primary", .after = "Energy.type") %>%
    dplyr::mutate(Gross.Net = "Gross", .after = "Stage") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::mutate(Flow = "Total", .after = "Product") %>%
    dplyr::mutate(Grouping = "Total", .after = "Flow")

  # Sets EX column type to numeric
  p_total$EX <- as.numeric(p_total$EX)

  return(p_total)

}


#' Calculate total primary energy by product
#'
#'
#' @param .sutdata
#' @param p_industry_prefixes
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_product <- function(.sutdata, p_industry_prefixes) {

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
#'
#' @param .sutdata
#' @param p_industry_prefixes
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_flow <- function(.sutdata, p_industry_prefixes) {

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

#' Calculate total final demand of final and useful energy
#'
#'
#' @param .sutdata
#' @param fd_sectors
#'
#' @return A data frame containing final and useful demand by total
#' @export
#'
#' @examples
calculate_fu_ex_total <- function(.sutdata, fd_sectors) {

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

#' Calculate total final demand of final and useful energy by product
#'
#' @param .sutdata
#' @param fd_sectors
#'
#' @return A data frame containing final and useful demand by total, by product
#' @export
#'
#' @examples
calculate_fu_ex_product <- function(.sutdata, fd_sectors) {

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

#' Calculate total final demand of final and useful energy by sector
#'
#' @param .sutdata
#' @param fd_sectors
#'
#' @return A data frame containing final and useful demand by total, by product
#' @export
#'
#' @examples
calculate_fu_ex_sector <- function(.sutdata, fd_sectors) {

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
#' @param .sutdata
#' @param fd_sectors
#' @param p_industry_prefixes
#'
#' @return A data frame containing energy/exergy values aggregated by total,
#'         sector and product at each stage of the energy conversion chain.
#' @export
#'
#' @examples
calculate_all_ex_data <- function(.sutdata, fd_sectors, p_industry_prefixes) {

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

