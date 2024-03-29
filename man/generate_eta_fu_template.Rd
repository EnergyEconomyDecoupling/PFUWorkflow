% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template_functions.R
\name{generate_eta_fu_template}
\alias{generate_eta_fu_template}
\title{Create a final-to-useful efficiencies template}
\usage{
generate_eta_fu_template(
  country,
  country_col = IEATools::iea_cols$country,
  cache_path = PFUWorkflow::cache_info$cache_path,
  fu_analysis_path_target = PFUWorkflow::target_names$fu_analysis_folder,
  fu_allocation_table_file_name = paste0(country,
    IEATools::fu_analysis_file_info$fu_analysis_file_suffix),
  ext = ".xlsx",
  fu_allocation_table_path = file.path(drake::readd(fu_analysis_path_target,
    character_only = TRUE, path = cache_path), country) \%>\%
    dir_create_pipe(showWarnings = FALSE, recursive = TRUE) \%>\%
    file.path(fu_allocation_table_file_name),
  fu_allocation_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name,
  eta_fu_template_file_name = paste0(country, " FU etas Template"),
  output_path = file.path(drake::readd(fu_analysis_path_target, character_only = TRUE,
    path = cache_path), country) \%>\% dir_create_pipe(showWarnings = FALSE, recursive =
    TRUE) \%>\% file.path(paste0(eta_fu_template_file_name, ext)),
  overwrite = FALSE
)
}
\arguments{
\item{country}{A string of the 3-letter ISO country code}

\item{country_col}{A string containing the name of the country column in the final-to-useful allocation table.
Default is \code{IEATools::iea_cols$country}.}

\item{cache_path}{See \code{PFUWorkflow::cache_info}.}

\item{fu_analysis_path_target}{See \code{PFUWorkflow::target_names}.}

\item{fu_allocation_table_file_name}{The name of the file containing the final-to-useful allocations for \code{country}.
Default is "\code{country} FU Analysis.\code{ext}".}

\item{ext}{The file name extension for both the allocation table and the efficiency template.
Default is ".xlsx" for an Excel file.}

\item{fu_allocation_table_path}{The path the the}

\item{fu_allocation_tab_name}{The name of the tab containing final-to-useful allocation information in \code{fu_analysis_file_name}. Default is "FU Allocations".}

\item{eta_fu_template_file_name}{The name of the efficiency template file written by this function.
Default is "\code{country} FU etas Template.\code{ext}".}

\item{output_path}{The path to the output file.
Default is constructed from the drake cache (\code{fu_analysis_path_target})
and \code{country}, and \code{eta_fu_template_file_name} arguments.}

\item{overwrite}{Tells whether to overwrite an existing file named \code{eta_fu_template_file_name}.\code{ext} in location \code{fu_analysis_path_name}.
Default is \code{FALSE}.}
}
\value{
the path to the blank final-to-useful efficiency template
}
\description{
The only arguments used internally are \code{country}, \code{fu_allocation_table_path}, \code{fu_allocation_tab_name}k
\code{eta_fu_template_file_name}
\code{overwrite}, and \code{output_path},
allowing the caller to specify only output_path to override all of the
path information stored in the drake cache.
}
