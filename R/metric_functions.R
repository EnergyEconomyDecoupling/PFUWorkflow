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


#' Create a list of vectors containing primary industry names
#'
#' @param p_industry_prefixes A list of Primary industry prefixes, usually
#'                            created by `create_p_industry_prefixes`
#' @param PSUT_DF A data frame containing at least an R PSUT matrix from which
#'                Primary industry names may be obtained
#'
#' @return A list of vectors containing the primary industries associated with
#'         each of the R matrices present in `PSUT_DF`
#'
#' @export
#'
#' @examples
create_p_industries <- function(p_industry_prefixes, PSUT_DF) {

  p_industries <- Recca::find_p_industry_names(p_industry_prefixes = p_industry_prefixes)

}



