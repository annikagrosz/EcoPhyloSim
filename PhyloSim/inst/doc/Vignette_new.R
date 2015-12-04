## ----setup, cache=FALSE, include=FALSE-----------------------------------
library(knitr)
opts_knit$set(tidy = T, fig=TRUE, fig.height = 4, fig.width=4, fig.align='center')
render_listings()

## ----message=FALSE-------------------------------------------------------
library(PhyloSim)

## ----eval=FALSE----------------------------------------------------------
#  ?createCompletePar

## ------------------------------------------------------------------------
par <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = c(500,1000), density = 1, environment = 0.5, specRate = 1, type="base")

## ----results='hide'------------------------------------------------------
simu <- runSimulation(par)

## ------------------------------------------------------------------------
Output1 <- simu$Output[[1]]

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
plotSpatialPhylo(simu, plot = "both", plotTraits = T, which.simulation = NULL)

## ------------------------------------------------------------------------
plotTraitDistribution(simu = simu, which.simulation = NULL, type ="all")

## ------------------------------------------------------------------------
Phyl   <- ape::drop.fossil(simu$Output[[2]]$phylogeny)
rePhyl <- phyloReconstruct(simu)

par(mfrow=c(1,2))
plot(rePhyl, main="reconstructed Phylogeny")
plot(Phyl, main="real Phylogeny")

## ------------------------------------------------------------------------
highlightLocalPhylo(simu, size = 50, n = 1)

## ------------------------------------------------------------------------
highlightLocalPhylo(simu, size = 5, n = 1)

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold'----
sac(simu, which.simulation = NULL)

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold'----
rac(simu, which.simulation = NULL)

## ------------------------------------------------------------------------
subPlots <- localPlots(simu = simu, size=100, n=10)

## ------------------------------------------------------------------------
pValues <- nullModel(simu = simu, abundance = FALSE, localPlotSize = 100, numberOfPlots = 10, repetitions = 100, fun="mpd")

## ------------------------------------------------------------------------
pValuesBatch <- calculatePhylogeneticDispersion(simuBatch, plotlength=20, plots=20, replicates=20, type="PhylMeta")

