% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_to_exergy.R
\name{move_to_exergy}
\alias{move_to_exergy}
\title{Move from all exergy quantities to all energy quantities in energy conversion chains}
\usage{
move_to_exergy(
  psut_energy,
  phi_vecs,
  countries,
  country = IEATools::iea_cols$country,
  phi_colname = IEATools::phi_constants_names$phi_colname
)
}
\arguments{
\item{psut_energy}{A wide-by-matrices data frame of energy conversion chain data.}

\item{phi_vecs}{A data frame of vectors of phi (exergy-to-energy ratios)}

\item{countries}{The countries for which this task should be performed.}

\item{country}{See \code{IEATools::iea_cols}.}

\item{phi_colname}{See \code{IEATools::phi_constants}.}
}
\value{
A version of \code{psut_energy} with additional rows
}
\description{
Converts energy conversion chains represented by the matrices
in the data frame of \code{psut_energy} from energy quantities to exergy quantities.
}
\details{
The steps in this calculation are to join phi_vecs to psut_energy.
Thereafter, we call into the \code{IEATools} package to do the matrix multiplications.
}
