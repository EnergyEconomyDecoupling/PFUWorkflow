% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tes_tfc_functions.R
\name{calculate_p_ex_product}
\alias{calculate_p_ex_product}
\title{Calculate total primary energy by product}
\usage{
calculate_p_ex_product(
  .sutdata,
  p_industry_prefixes,
  country_colname = IEATools::iea_cols$country,
  method_colname = IEATools::iea_cols$method,
  energy_type_colname = IEATools::iea_cols$energy_type,
  year_colname = IEATools::iea_cols$year,
  flow_colname = PFUWorkflow::sea_cols$flow_colname,
  e_product_colname = PFUWorkflow::sea_cols$e_product_colname,
  stage_colname = PFUWorkflow::sea_cols$stage_colname,
  gross_net_colname = PFUWorkflow::sea_cols$gross_net_colname,
  agg_by_colname = PFUWorkflow::sea_cols$agg_by_colname,
  p_ind_comp_colname = PFUWorkflow::sea_cols$p_ind_comp_colname,
  p_ind_prefix_colname = PFUWorkflow::sea_cols$p_ind_prefix_colname,
  ex_colname = PFUWorkflow::sea_cols$ex_colname,
  ex_p_colname = PFUWorkflow::sea_cols$ex_p_colname,
  primary_value = IEATools::all_stages$primary,
  all_value = PFUWorkflow::agg_metadata$all_value,
  product_value = PFUWorkflow::agg_metadata$product_value
)
}
\arguments{
\item{.sutdata}{A data frame containing Physical Supply-Use Table (PSUT)
matrices.}

\item{p_industry_prefixes}{A character vector of primary energy industry prefixes.
Usually "Resources", "Imports", and "Stock changes".}

\item{country_colname, method_colname, energy_type_colname, year_colname}{See \code{IEATools::iea_cols}.}

\item{flow_colname, e_product_colname, stage_colname, gross_net_colname, agg_by_colname, p_ind_comp_colname, p_ind_prefix_colname, ex_colname, ex_p_colname}{See \code{PFUWorkflow::sea_cols}.}

\item{primary_value}{The string "Primary", representing the Primary stage of the energy conversion chain, see \code{IEATools::all_stages}.}

\item{all_value, product_value}{See \code{PFUWorkflow::agg_metadata}.}
}
\value{
A data frame containing aggregate primary energy/exergy data by product
}
\description{
Calculate the total energy supply (TES) in primary energy terms by product. This metric
was formerly called the total primary energy supply (TPES).
This function first uses the uses \code{Recca::find_p_industry_names()} function,
with a user-supplied set of primary industry prefixes \code{p_industry_prefixes}
to identify the primary industries desired for analysis.
The \code{Recca::primary_aggregates()} function is then applied to \code{.sutdata}
data frame by product, to calculate the total energy supply across all flows
for each product.
}
\examples{
library(Recca)
total_energy_supply <- Recca::UKEnergy2000mats \%>\%
                         tidyr::pivot_wider(names_from = matrix.name,
                                            values_from = matrix) \%>\%
                         dplyr::mutate(Method = "PCM") \%>\%
     calculate_p_ex_product(p_industry_prefixes = list(c("Resources", "Imports")))

}
