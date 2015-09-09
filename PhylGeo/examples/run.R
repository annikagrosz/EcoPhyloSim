

library(PhylGeo)
 

par <- createCompletePar(x = 50, y = 50, dispersal = 0, runs = 10000, density = 0, environment = 1, specRate = 1)



system.time(simu <- runSimulation(par))

plotSpatialPhylo(simu, plot = "both")

plotTraitDistribution(simu)


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
