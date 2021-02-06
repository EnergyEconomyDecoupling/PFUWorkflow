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


#' Calculate final demand of final and useful energy
#'
#' This function is applied to a data frame which contains the following columns:
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
calculate_final_demand <- function(PSUT_DF) {

  fd_sector_list <- create_fd_sectors_list(fd_sectors = create_fd_sectors(), PSUT_DF = PSUT_DF)

  PSUT_DF <- PSUT_DF %>%
    dplyr::mutate(fd_sectors = fd_sector_list)

  fd_total <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF, fd_sectors = "fd_sectors", by = "Total")

  fd_product <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF, fd_sectors = "fd_sectors", by = "Product")

  fd_sector <- Recca::finaldemand_aggregates(.sutdata = PSUT_DF, fd_sectors = "fd_sectors", by = "Sector")

  # Binds data frames containing final demand by total, product and sector into
  # a single data frame



}


