

library(PhylGeo)
# Run the model

# dispersal = 1 
# dispersal = 3
# dispersal = 2 

myModel <- fullMod(x = 50, y = 50, dispersal = 0.5, runs = 2000, specRate = 1.0, density = T, environment = T, seed = sample(1:1000,1), mortalityStrength = 2, saveLocation = "./")

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

specRich(myModel$specMat)

#par(mfrow = c(1,2))
#rac(myModel$specMat)
#sac(myModel$specMat)
