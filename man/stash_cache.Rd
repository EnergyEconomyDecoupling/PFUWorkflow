% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/storing.R
\name{stash_cache}
\alias{stash_cache}
\title{Save the cache to a zip file, then to \code{pipeline_caches_folder}}
\usage{
stash_cache(pipeline_caches_folder, cache_folder, file_prefix, dependency)
}
\arguments{
\item{pipeline_caches_folder}{The folder into which the pipeline cache will be saved as a .zip file.}

\item{cache_folder}{The cache folder that is to be zipped and saved.
This path is interpreted relative to the working directory.}

\item{file_prefix}{The prefix for the output file name.}

\item{dependency}{The last target that should be executed before saving the cache.
Not used internally.}
}
\value{
A logical saying whether the saving operation was successful.
}
\description{
Saves a pipeline cache to a zip file into the \code{pipeline_caches_folder}.
}
\details{
Note that the \code{dependency} argument is not used internally.
Rather, \code{dependency} exists to ensure that the pipeline
executes the right targets before saving the cache.
}
