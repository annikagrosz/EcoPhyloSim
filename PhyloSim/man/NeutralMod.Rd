% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neutralModelR.R
\name{NeutralMod}
\alias{NeutralMod}
\title{Model of neutral community assembly}
\usage{
NeutralMod(xdim = 100, ydim = 100, specRate = 2, seed = NULL,
  runs = 500)
}
\arguments{
\item{xdim}{Integer, Dimension of the model landscape in x-direction}

\item{ydim}{Integer, Dimension of the model landscape in y-direction}

\item{specRate}{The speciation rate in total speciation events per generation}

\item{seed}{The random seed (do not change this option if you previously set the seed!)}

\item{runs}{The number of generations the model runs through}
}
\description{
Runs a simple model of species community assembly under neutral conditions as defined by Hubbel (2001). Usually called by \code{\link{runSimulation}}.
}
\details{
This model is to be seen only for test and teaching purpose. However, if you want to use the results for further analysis please call the model with \code{\link{runSimulation}}. \cr\cr  Be careful with the dimensions you choose. Large grids and many generations may take very long to compute. You can assume to have reached the equilibrium state of the model after dim^2/2.
}
\examples{
# Run the model
metaCom <- NeutralMod(xdim=50, ydim=50, specRate=2, seed=1500, runs=500)
image(metaCom)

# Usually the function is called by the runSimualtion function
# Define a parameter set
parNeut <- createCompletePar(x = 50, y = 50, dispersal = FALSE , runs = 500,
        density = 1, environment = 0.5, specRate = 1, type="Rneutral")

# Run the model
simuNeut <- runSimulation(parNeut)

# Visualize the grid
image(simuNeut$Output[[1]]$specMat)
}
\references{
Hubbell, Stephen P. The unified neutral theory of biodiversity and biogeography (MPB-32). Vol. 32. Princeton University Press, 2001.
}
