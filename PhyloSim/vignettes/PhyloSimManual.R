## ----setup, cache=FALSE, include=FALSE-----------------------------------
library(knitr)
opts_knit$set(tidy = T, fig=TRUE, fig.height = 4, fig.width=4, fig.align='center')
render_listings()

## ----eval = F------------------------------------------------------------
#  # install.packages(c("devtools","Rcpp")) # I case you don't have them installed
#  library(devtools)
#  library(Rcpp)
#  
#  install_url("https://dl.dropboxusercontent.com/s/zkdof0b5b523qxt/PhyloSim_0.3.tar.gz")
#  
#  ?PhyloSim
#  browseVignettes("PhyloSim")

## ------------------------------------------------------------------------
citation('PhyloSim')

## ----message=FALSE-------------------------------------------------------
library(PhyloSim)

## ------------------------------------------------------------------------
par <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = c(500,1000), density = 1, environment = 0.5, specRate = 1, type="base")

## ----eval=FALSE----------------------------------------------------------
#  ?createCompletePar

## ----results='hide'------------------------------------------------------
simu <- runSimulation(par)

## ------------------------------------------------------------------------
Output1 <- simu$Output[[1]]
str(Output1)

## ------------------------------------------------------------------------
str(simu$Model)

## ----results='hide'------------------------------------------------------
par1 <- createCompletePar(x = 50, y = 50, dispersal = "global", runs = 1000, density = 1, environment = 0.5, specRate = 1, type="base")

par2 <- createCompletePar(x = 50, y = 50, dispersal = "global", runs = 1000, density = 1, environment = 2, specRate = 1, type="base")

parmBatch <- list(par1, par2)

simuBatch <- runSimulationBatch(parmBatch, parallel = 2)

## ------------------------------------------------------------------------
simu1 <- simuBatch[[1]]

## ----fig.cap = "The plotSpatialPhylo plot consist of three different figures, a phylogeny that shows the evolutionary history of the species, a trait plot that shows the traits values for each of the species (coded by size), and the spatial distribution of individuals, with colors matching the colors in the phylogeny. Which of these figures are plotted can be triggered with the arguments *plot* and *plotTraits* (see help)"----

plotSpatialPhylo(simu, plot = "both", plotTraits = T, which.result = NULL)

