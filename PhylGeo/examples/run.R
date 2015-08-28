

library(PhylGeo)
# Run the model

# dispersal = 1 
# dispersal = 3
# dispersal = 2 

myModel <- fullMod(x = 100, y = 100, dispersal = 0.5, runs = 1000, specRate = 1.0, density = F, environment = F, seed = 1, fitnessBaseMortalityRatio = 5)

# Look at the phylogeny (requires package 'ape')
require(ape)

# Get the phylogeny
phylogeny <- myModel$phylogeny

# Get the phylogeny
#plot(myModel$phylogeny)

# Only extant taxa
extantPhylogeny <- drop.fossil(phylogeny)

#plot(extantPhylogeny)

plotTraitDistribution(myModel)

plotSpatialPhylo(myModel$specMat, extantPhylogeny, plot = "both")

specRich(myModel$specMat)


par(mfrow = c(1,2))
rac(myModel$specMat)
sac(myModel$specMat)
