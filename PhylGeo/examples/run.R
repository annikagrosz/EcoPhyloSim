

library(PhylGeo)
 

par <- createCompletePar(x = 50, y = 50, dispersal = 1, runs = 1000, density = 0, environment = 0, specRate = 1)

simu <- runSimulation(par)

plotTraitDistribution(simu)



plotSpatialPhylo(simu, plot = "both")






plot(out, time = 1)

par(mfrow = c(1,2))
rac(simu$specMat)
sac(simu$specMat)


par <- createCompletePar(runs = c(100,200,300))
simu <- runSimulation(par)
str(simu)



par <- createCompletePar(type = "Rneutral")
simu <- runSimulation(par)
str(simu)
