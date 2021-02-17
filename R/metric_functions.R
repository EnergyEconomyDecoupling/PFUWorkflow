# This script contains functions which establish the following constants,
# and calculate the following metrics:
# Final demand sectors
# Primary industries
# Total energy supply (TES) of primary energy
# Total final consumption (TFC) of final and useful energy
# Primary-Final, Final-Useful, and Primary-Final efficiencies

# Loads required packages
library(matsbyname)
library(matsindf)
library(tidyverse)
library(IEATools)
library(Recca)


#' Create a vector of final demand sectors
#'
#' @return A vector of final demand sectors
#' @export
#'
#' @examples
create_fd_sectors <- function() {

  fd_sectors <- c(IEATools::industry_flows,
                  IEATools::transport_flows,
                  IEATools::other_flows,
                  IEATools::non_energy_flows) %>%
    as.character()

  # fd_sectors <- c(IEATools::industry_net_flows,
  #                 IEATools::transport_domestic_flows,
  #                 IEATools::other_flows,
  #                 IEATools::non_energy_flows) %>%
  #   as.character()

}


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
create_fd_sectors_list <- function(fd_sectors, PSUT_DF) {

  rep(x = list(c(fd_sectors)), times = nrow(PSUT_DF))

}


#' Create a list of primary industry prefixes
#'
#' @return A list of primary industry prefixes
#' @export
#'
#' @examples
create_p_industry_prefixes <- function() {

  p_industry_prefixes = list(as.character(IEATools::tpes_flows))

}

#' Calculate total primary energy
#'
#' This function is applied to a data frame which must contain the following columns:
#' Year, Method, Energy.type, Stage, Country, R, r_EIOU, U, V, Y.
#' Where R, r_EIOU, U, V, and Y are nested matrices.
#'
#' @param PSUT_DF
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_total <- function(PSUT_DF) {

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- PSUT_DF %>%
    dplyr::mutate(p_industry_prefixes = create_p_industry_prefixes()) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(p_industries_complete, .after = p_industry_prefixes)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_total <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                        p_industries = "p_industries_complete",
                                        by = "Total") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX")) %>%
    dplyr::mutate(Stage = str_replace(Stage, "Final", "Primary")) %>%
    dplyr::mutate(Gross.Net = "Gross") %>%
    dplyr::relocate(Gross.Net, .after = "Stage") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::mutate(Sector = "Total", .after = "Product")

  p_total$EX <- as.numeric(p_total$EX)

  return(p_total)

}


#' Calculate total primary energy by product
#'
#' This function is applied to a data frame which must contain the following columns:
#' Year, Method, Energy.type, Stage, Country, R, r_EIOU, U, V, Y.
#' Where R, r_EIOU, U, V, and Y are nested matrices.
#'
#' @param PSUT_DF
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_product <- function(PSUT_DF) {

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- PSUT_DF %>%
    dplyr::mutate(p_industry_prefixes = create_p_industry_prefixes()) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(p_industries_complete, .after = p_industry_prefixes)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_product <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                         p_industries = "p_industries_complete",
                                         by = "Product") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX")) %>%
    dplyr::mutate(Stage = str_replace(Stage, "Final", "Primary")) %>%
    dplyr::mutate(Gross.Net = "Gross") %>%
    dplyr::relocate(Gross.Net, .after = "Stage")

  p_product_expanded <- p_product %>%
    matsindf::expand_to_tidy(matrix.names = "EX",
                             matvals = "EX",
                             rownames = "Product",
                             colnames = "colnames") %>%
    dplyr::select(-matrix.names, -colname, -rowtype, -Energy)



  #### Not working yet ####

  return(p_product)

}

#' Calculate total primary energy by flow
#'
#' This function is applied to a data frame which must contain the following columns:
#' Year, Method, Energy.type, Stage, Country, R, r_EIOU, U, V, Y.
#' Where R, r_EIOU, U, V, and Y are nested matrices.
#'
#' @param PSUT_DF
#'
#' @return
#' @export
#'
#' @examples
calculate_p_ex_flow <- function(PSUT_DF) {

  # Adds primary industry name prefixes to DF and creates a complete list of
  # primary industries
  PSUT_DF_p <- PSUT_DF %>%
    dplyr::mutate(p_industry_prefixes = create_p_industry_prefixes()) %>%
    Recca::find_p_industry_names() %>%
    dplyr::relocate(p_industries_complete, .after = p_industry_prefixes)

  # Call Recca::primary_aggregates() to obtain the IEA version of aggregate primary energy
  # from the R, V, and Y matrices (which includes imported final energy, effect of bunkers),
  p_flow <- Recca::primary_aggregates(.sutdata = PSUT_DF_p,
                                      p_industries = "p_industries_complete",
                                      by = "Flow") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.p) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX")) %>%
    dplyr::mutate(Stage = str_replace(Stage, "Final", "Primary")) %>%
    dplyr::mutate(Gross.Net = "Gross") %>%
    dplyr::relocate(Gross.Net, .after = "Stage")

  p_flow_expanded <- p_flow %>%
    matsindf::expand_to_tidy(matvals = "EX",
                             colnames = "Flow") %>%
    dplyr::select(-rownames) %>%
    dplyr::relocate(Flow, .before = "Year") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net")


  return(p_flow_expanded)

}

#' Calculate total final demand of final and useful energy
#'
#' This function is applied to a data frame which must contain the following columns:
#' Year, Method, Energy.type, Stage, Country, r_EIOU, U, Y.
#' Where r_EIOU, U, and Y are nested matrices.
#'
#' @param PSUT_DF
#'
#' @return A data frame containing final and useful demand by total, by product,
#'         and by sector.
#' @export
#'
#' @examples
calculate_fu_ex_total <- function(PSUT_DF) {

  fd_sector_list <- create_fd_sectors_list(fd_sectors = create_fd_sectors(), PSUT_DF = PSUT_DF)

  PSUT_DF_fu <- PSUT_DF %>%
    dplyr::mutate(fd_sectors = fd_sector_list)

  fu_total <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF_fu, fd_sectors = "fd_sectors", by = "Total") %>%
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, EX.d_net, EX.d_gross) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Year", "EX.d_net", "EX.d_gross")) %>%
    tidyr::pivot_longer(cols = EX.d_net:EX.d_gross,
                        names_to = "Gross.Net",
                        values_to = "EX") %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_net", "Net")) %>%
    dplyr::mutate(Gross.Net = stringr::str_replace(Gross.Net, "EX.d_gross", "Gross")) %>%
    dplyr::relocate(Gross.Net, .after = "Stage") %>%
    dplyr::mutate(Product = "Total", .after = "Gross.Net") %>%
    dplyr::mutate(Sector = "Total", .after = "Product")

  fu_total$EX <- as.numeric(fu_total$EX)

  return(fu_total)

}


#' Create a data frame containing all aggregate energy/exergy data
#'
#'
#'
#' @param PSUT_DF
#'
#' @return
#' @export
#'
#' @examples
calculate_all_ex_data <- function(PSUT_DF) {

  fu_total <- calculate_fu_ex_total(PSUT_DF = PSUT_DF)

  p_total <- calculate_p_ex_total(PSUT_DF = PSUT_DF)

  p_flow <- calculate_p_ex_flow(PSUT_DF = PSUT_DF) %>%
    # Change name from Flow to sector so data frames can be bound
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage",
                             "Gross.Net", "Product", "Sector", "Year", "EX"))

  # Add data by product here

  # Add data by group here

  # Bind all data together
  all_data <- fu_total %>%
    rbind(p_total) %>%
    rbind(p_flow)

  all_data$EX <- as.numeric(all_data$EX)

  return(all_data)


}

