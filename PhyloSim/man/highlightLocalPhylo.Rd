% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotHighlightLocalPhylo.R
\name{highlightLocalPhylo}
\alias{highlightLocalPhylo}
\title{Phylogenetic clades}
\usage{
highlightLocalPhylo(simu, which.result = NULL, size, n, subplot = NULL,
  phylogeny = NULL)
}
\arguments{
\item{simu}{Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the phylogeny ($phylogeny) and species matrix ($specMat). The species matrix is used as metacommunity}

\item{which.result}{Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.}

\item{size, }{size of the local plots.}

\item{n}{The number of local communities. For each community a new plot is created. Needs to be greater than 1}

\item{subplot}{if no simu object is given, a communitytable of a local community can be given here}

\item{phylogeny}{if no simu object is ginve, an object of the class 'phylo' can be given here to describe the communites phylogeny}
}
\description{
Visualises the phylogenetic clades of a local community within a metacommunity.
}
\details{
This function works with a single simu object (class "phylosim"). If no simu object is given, it is also possible to calculate the clades from a given phylogeny and localc plots. Here the phylogeny represents the metacommunity wihtin which the local plots create the clades
}
\examples{

#Load data
data(simu.neutral.global)
data(simu.neutral.local)
data(simu.envcom.local)



#Plot clades for neutral model with global dispersal with three subplots
par(mfrow=c(1,3))
highlightLocalPhylo(simu.neutral.global, size = 50, n = 3)



#Plot clades for neutral model with local dispersal with three subplots
par(mfrow=c(1,3))
highlightLocalPhylo(simu.neutral.local, size = 50, n = 3)



#Plot clades for environment and competition model with global dispersal with three subplots
par(mfrow=c(1,3))
highlightLocalPhylo(simu.envcom.local, size = 50, n = 3)



#increasing plot size
par(mfrow=c(2,2))
highlightLocalPhylo(simu.envcom.local, size = 50, n =2)
highlightLocalPhylo(simu.envcom.local, size = 1000, n =2)
}
