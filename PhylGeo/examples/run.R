

library(PhylGeo)
# Run the model

# dispersal = 1 
# dispersal = 3
# dispersal = 2 

simu <- fullMod(x = 10, y = 10, dispersal = 0.5, runs = 1000, specRate = 1.0, density = F, environment = F, seed = 1, fitnessBaseMortalityRatio = 5)

str(simu)

# Look at the phylogeny (requires package 'ape')
require(ape)


plotSpatialPhylo(simu, plot = "both")

specRich(simu$specMat)


par(mfrow = c(1,2))
rac(simu$specMat)
sac(simu$specMat)

plotTraitDistribution(simu)


