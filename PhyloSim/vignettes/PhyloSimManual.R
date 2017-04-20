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

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold', fig.cap="Shows the species richness in dependency to the plotsize of a local community. A positively bent curve indicates clustering of a species community. An increase in plot size leads to an increase in specis richness. A negatively bent curve indicates a more neutral distribution of species within the community."----
sac(simu, which.result = NULL)

## ----results='hide', out.width='6cm', out.height='6cm', fig.show='hold', fig.cap="Shows the rank-abundance curve (RAC). For that, each species is given a rank according to its abundance (highest abundance = rank 1). Then the abundance is plotted in dependency to the species' rank. RACs display the amount of equally abundand species that the community can support. A linear curve indicates a less stable or neutral community supporting only a few highly abundand species, whereas an S-shaped curve indicates a more stable community. In the latter case several species of the same abundance can be supported."----
rac(simu, which.result = NULL)

## ----fig.cap= "The plotTraitDistribution plot consists of three different figures for the three traits in the model (environmental trait, competition trait, neutral trait). The upper panel illustrates the trait's magnitude in dependency of the environment for different species. The middle panel illustrates the spatial distribution of the given trait. The lower panel shows a histogram of the given trait."----
plotTraitDistribution(simu = simu, which.result = NULL, type ="all")

## ----fig.cap="Shows the reconstructed as well as the real phylogeny. It is visible that there are differences in the speces that make up the resulting communities as well as in their evolution / assembly"----
Phyl   <- ape::drop.fossil(simu$Output[[2]]$phylogeny)
rePhyl <- phyloReconstruct(simu)

par(mfrow=c(1,2))
plot(rePhyl, main="reconstructed Phylogeny")
plot(Phyl, main="real Phylogeny")

## ----fig.cap="Highlights a relatively large local community",fig.height=5.4----
highlightLocalPhylo(simu, size = 10,n = 1)

## ----fig.cap="Highlights a relatively small local community",fig.height=5.4----
highlightLocalPhylo(simu, size = 5, n = 1)

## ------------------------------------------------------------------------
localPlots(simu, size = 10, n = 2)

## ----NullModel,eval = F--------------------------------------------------
#  pValues <- nullModel(simu = simu, abundance = FALSE, localPlotSize = 10, numberOfPlots = 20, repetitions = 100, fun="mpd")

## ----CalcPhylDis,eval = F------------------------------------------------
#  pValuesBatch <- calculatePhylogeneticDispersion(simuBatch, plotlength=3, plots=20, replicates=20, type="PhylMeta")

## ----PlotPhylDis, fig.cap = "Shows different combinations of dispersal limitation (y-axis), environmental trait(upper x-axis) and density dependnce (lower x-axis). The respective colour shows the phylogenetic pattern (over- or underdispersed)."----
data("pPD.pvalues")
data("pPD.positions")

plotPhylogeneticDispersion(pvalues = pPD.pvalues, positions = pPD.positions, title = "Null Meta")

