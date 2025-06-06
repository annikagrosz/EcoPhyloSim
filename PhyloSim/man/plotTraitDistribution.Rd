% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTraitDistribution.R
\name{plotTraitDistribution}
\alias{plotTraitDistribution}
\title{Trait plots}
\usage{
plotTraitDistribution(simu, type = "hist", which.result = NULL)
}
\arguments{
\item{simu}{Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the three trait matrices ($traitMat;$compMat;$neutMat) and the environment matrix ($envMat)}

\item{type}{defines wether the histogram should be plotted standalone or with the trait ~ Environment relationship and the spatial distribution. Latter is done by type = "all". Default is "hist"}

\item{which.result}{Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.}
}
\description{
Plots trait-histograms, trait ~ Environment relationship and the spatial distribution of the different traits
}
\examples{
#Load data
data(simu.neutral.global)
data(simu.neutral.local)
data(simu.envcom.local)



#neutral model with global dispersal
plotTraitDistribution(simu=simu.neutral.global)
plotTraitDistribution(simu=simu.neutral.global, type ="all")


#neutral model with local dispersal
plotTraitDistribution(simu=simu.neutral.local)
plotTraitDistribution(simu=simu.neutral.local, type = "all")



#evironment and competition model with local dispersal
plotTraitDistribution(simu=simu.envcom.local)
plotTraitDistribution(simu=simu.envcom.local, type = "all")

}
