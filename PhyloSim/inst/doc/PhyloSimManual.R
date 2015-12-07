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

## ----fig.cap = "Shows the plot created by plotSpatialPhylo with all three possibilites (phylogeny tree, traits, and spatial distribution)"----

plotSpatialPhylo(simu, plot = "both", plotTraits = T, which.simulation = NULL)

## ----fig.cap= "Shows the different traits in dependency to the environment (upper), their spatial distribution (middle) and the frequency of the traits' values per species."----
plotTraitDistribution(simu = simu, which.simulation = NULL, type ="all")

## ----fig.cap="Shows the reconstructed as well as the real phylogeny. It is visible that there are differences in the speces that make up the resulting communities as well as in their evolution / assembly"----
Phyl   <- ape::drop.fossil(simu$Output[[2]]$phylogeny)
rePhyl <- phyloReconstruct(simu)

par(mfrow=c(1,2))
plot(rePhyl, main="reconstructed Phylogeny")
plot(Phyl, main="real Phylogeny")

## ----fig.cap="Highlights a relatively large local community",fig.height=5.4----
highlightLocalPhylo(simu, size = 50, n = 1)

## ----fig.cap="Highlights a relatively small local community",fig.height=5.4----
highlightLocalPhylo(simu, size = 5, n = 1)

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold', fig.cap="Shows the species richness in dependency to the plotsize of a local community. A positively bent curve indicates clustering of a species community. An increase in plot size leads to an increase in specis richness. A negatively bent curve indicates a more neutral distribution of species within the community."----
sac(simu, which.simulation = NULL)

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold', fig.cap="Shows the rank-abundance curve (RAC). For that, each species is given a rank according to its abundance (highest abundance = rank 1). Then the abundance is plotted in dependency to the species' rank. RACs display the amount of equally abundand species that the community can support. A linear curve indicates a less stable or neutral community supporting only a few highly abundand species, whereas an S-shaped curve indicates a more stable community. In the latter case several species of the same abundance can be supported."----
rac(simu, which.simulation = NULL)

## ------------------------------------------------------------------------
subPlots <- localPlots(simu = simu, size=100, n=10)

## ------------------------------------------------------------------------
pValues <- nullModel(simu = simu, abundance = FALSE, localPlotSize = 100, numberOfPlots = 10, repetitions = 100, fun="mpd")

## ------------------------------------------------------------------------
pValuesBatch <- calculatePhylogeneticDispersion(simuBatch, plotlength=20, plots=20, replicates=20, type="PhylMeta")

## ----fig.cap = "Shows different combinations of dispersal limitation (y-axis), environmental trait(upper x-axis) and density dependnce (lower x-axis). The respective colour shows the phylogenetic pattern (over- or underdispersed)."----
data("pPD.pvalues")
data("pPD.positions")

plotPhylogeneticDispersion(pvalues = pPD.pvalues, positions = pPD.positions, title = "Null Meta")

