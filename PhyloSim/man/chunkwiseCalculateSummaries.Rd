% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runPhylosimAnalysis.R
\name{chunkwiseCalculateSummaries}
\alias{chunkwiseCalculateSummaries}
\title{Cunkwise summary statistics calculation}
\usage{
chunkwiseCalculateSummaries(dir = ".", nCores = "auto")
}
\arguments{
\item{dir}{Directory from which the "out_" files should be read and to which the "summary_" files will be saved.}

\item{nCores}{"auto" or number of parallel cores.}
}
\description{
Calculates summary statistics for all "out_" files created with runPhylosimAnalysis and saves the results to "summary_" files in the same folder.
}
\author{
Tankred Ott
}
