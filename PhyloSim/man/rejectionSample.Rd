% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runPhylosimAnalysis.R
\name{rejectionSample}
\alias{rejectionSample}
\title{Rejection Sampling}
\usage{
rejectionSample(params, summaries, nDraws = 10, eQuantile = 0.025,
  nCores = "auto", sumFun = median)
}
\arguments{
\item{summaries}{A data.frame of the summary statistics (different summaries in colums)}

\item{nDraws}{Number of ABC rejection sampling draws}

\item{eQuantile}{Epsilon expressed as quantile. E.g. 0.025 are only the nearest 2.5\\%.}

\item{nCores}{Number of parallel cores, or "auto".}

\item{sumFun}{Function to summarize the accepted simulated parameters, e.g. mean or median}

\item{pars}{A data.frame of the parameters (different parameters in colums)}
}
\value{
A data.frame containing the true parameters postfixed with "_t" and the estimated parameters postfixed with "_e".
}
\description{
Applies rejection sampling.
}
\author{
Tankred Ott
}
