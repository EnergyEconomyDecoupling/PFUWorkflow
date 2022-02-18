#' Save the drake cache to a zip file, then to `workflow_output_folder`
#'
#' @param workflow_output_folder The folder into which the drake cache will be saved.
#' @param dependency The target you want to be sure is executed before
#'                   calling this function.
#'
#' @return A logical saying whether the saving operation was successful.
#'
#' @export
stash_cache <- function(workflow_output_folder, dependency) {
  # Zip the drake cache
  zipped_cache_filename <- paste0("pfu_workflow_cache_", parsedate::format_iso_8601(Sys.time()), ".zip") %>%
    # Change file name format to be equivalent to the pins file format.
    # Eliminate "-" characters
    gsub(pattern = "-", replacement = "") %>%
    # Eliminate ":" characters, because they cause problems on some OSes.
    gsub(pattern = ":", replacement = "") %>%
    # Change "+0000" to "Z", where "Z" means Zulu time (GMT offset of 00:00)
    gsub(pattern = "\\+0000", replacement = "Z")
  invisible(utils::zip(zipfile = zipped_cache_filename, files = ".drake"))
  # Calculate the folder structure for the output
  year <- lubridate::year(Sys.Date())
  month <- lubridate::month(Sys.Date())
  output_year_dir <- file.path(workflow_output_folder, year)
  dir.create(output_year_dir, showWarnings = FALSE)
  output_month_dir <- file.path(output_year_dir, month)
  dir.create(output_month_dir, showWarnings = FALSE)
  # Copy the file to the workflow output folder
  copy_successful <- file.copy(from = zipped_cache_filename,
                               to = output_month_dir)
  if (!copy_successful) {
    stop(paste("copying of drake cache unsuccessful in stach_cache():",
               zipped_cache_filename))
  }
  if (file.exists(zipped_cache_filename)) {
    # To keep things clean
    file.remove(zipped_cache_filename)
  }
  return(copy_successful)
}



#' Save the `PSUT` target in a pinboard.
#'
#' Releases (`release = TRUE`)
#' or not (`relase = FALSE`)
#' a new version of the PSUT target
#' using the `pins` package.
#'
#' Released versions of the `PSUT` target can be obtained
#' as shown in examples.
#'
#' @param workflow_releases_folder The folder that contains the pinboard.
#' @param psut The PSUT object, the final target for this workflow.
#' @param release A boolean telling whether to do a release.
#'                Default is `FALSE`.
#'
#' @return If `release` is `TRUE`,
#'         the fully-qualified path name of the PSUT file in the pinboard.
#'         If `release` is `FALSE`,
#'         `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Establish the pinboard
#' pinboard <- pins::board_folder("~/Dropbox/Fellowship 1960-2015 PFU database/PFUWorkflowReleases/")
#' # Get information about the `PSUT` target in the pinboard
#' pinboard %>%
#'   pins::pin_meta(name = "psut")
#' # Find versions of the `PSUT` target
#' pinboard %>%
#'   pins::pin_versions(name = "psut")
#' # Get the latest copy of the `PSUT` target.
#' my_psut <- pinboard %>%
#'   pins::pin_read(name = "psut")
#' # Retrieve a previous version of the `PSUT` target.
#' my_old_psut <- pinboard %>%
#'   pins::pin_read(name = "psut", version = "20220218T023112Z-1d9e1")}
release_psut <- function(workflow_releases_folder, psut, release = FALSE) {
  if (release) {
    # Establish the pinboard
    out <- pins::board_folder(workflow_releases_folder, versioned = TRUE) %>%
      # Returns the fully-qualified name of the file written to the pinboard.
      pins::pin_write(psut)
  } else {
    out <- NULL
  }
  return(out)
}
