% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tes_tfc_functions.R
\name{calculate_finaluseful_ex_data}
\alias{calculate_finaluseful_ex_data}
\title{Create a data frame containing final and useful aggregate energy/exergy data}
\usage{
calculate_finaluseful_ex_data(.sutdata, fd_sectors)
}
\arguments{
\item{.sutdata}{A data frame containing Physical Supply-Use Table (PSUT)
matrices with associated final demand sector names}

\item{fd_sectors}{A character vector of final demand sectors.}
}
\value{
A data frame containing final and useful energy/exergy values aggregated by total,
sector and product.
}
\description{
This functions creates a single data frame containing final and useful
energy/exergy by country, year, method, energy quantification,
and grouping variable (Total, Product, and Sector) using the functions:
\code{calculate_fu_ex_total}, \code{calculate_fu_ex_sector}, \code{calculate_fu_ex_product}
and binding the outputs of these functions into a single data frame.
}
\examples{
library(Recca)
finaluseful_data <- Recca::UKEnergy2000mats \%>\%
                      tidyr::pivot_wider(names_from = matrix.name,
                                         values_from = matrix) \%>\%
                      dplyr::mutate(Method = "PCM") \%>\%
                      calculate_finaluseful_ex_data(fd_sectors = c("Residential"))
}
