

library(PhylGeo)
# Run the model

# dispersal = 1 
# dispersal = 3
# dispersal = 2 

myModel <- fullMod(x = 100, y = 100, dispersal = 3, dispersalCut = 4, runs = 200, specRate = 1.0, density = T, environment = F, neutral = F, seed = 1510, mortalityFitness = T, reproductiveFitness = F, mortalityStrength = 10, saveLocation = "./")

# Look at the phylogeny (requires package 'ape')
require(ape)

# Get the phylogeny
phylogeny <- myModel$phylogeny

# Get the phylogeny
#plot(myModel$phylogeny)

# Only extant taxa
extantPhylogeny <- drop.fossil(phylogeny)

#plot(extantPhylogeny)


plotSpatialPhylo(myModel$specMat, extantPhylogeny, plot = "both")

par(mfrow = c(1,2))
rac(myModel$specMat)
sac(myModel$specMat)
