% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gross_net_metadata}
\alias{gross_net_metadata}
\title{Gross or Net metadata information}
\format{
A string list with 2 entries.
\describe{
\item{gross_value}{The string "Gross" indicating that final demand was calculated for both EIOU and non-EIOU sectors. See \code{Recca::finaldemand_aggregates}.}
\item{net_value}{The string "Net" indicating that final demand was calculated for only non-EIOU sectors. See \code{Recca::finaldemand_aggregates}.}
}
}
\usage{
gross_net_metadata
}
\description{
A string list containing values indicating whether the output of the functions \code{Recca::finaldemand_aggregates}, \code{PFUWorkflow::calculate_fu_ex_total},
\code{PFUWorkflow::calculate_fu_ex_product}, \code{PFUWorkflow::calculate_fu_ex_sector}, and \code{PFUWorkflow::calculate_finaluseful_ex_data}
are in Gross or Net terms. To be supplied to the metadata columns \code{PFUWorkflow::sea_cols$gross_net_colname}.
}
\examples{
gross_net_metadata
}
\keyword{datasets}
