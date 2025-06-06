% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traitsGetter.R
\name{getAverageTraits}
\alias{getAverageTraits}
\title{Get Average Traits}
\usage{
getAverageTraits(simu, which.result = NULL, orderPhylo = T)
}
\arguments{
\item{simu}{Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the spatial species matrix ($specMat) the environment matrix ($envMat) and the three different trait matrizes ($traitMat,$compMat, $neutMat)}

\item{which.result}{Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.}

\item{orderPhylo}{defines if the mean traits shall be calculated for extinct species as well. default is yes (TRUE)}
}
\value{
A matrix containing the means of env trait, comp trait, neutral trait, and the environment for each species
}
\description{
calculates the arithmetic mean of the trait values for each species.
}
