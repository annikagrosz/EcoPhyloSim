

library(PhylGeo)
 

par <- createCompletePar(dispersal = 1, runs = 1000, density = 0.7)

simu <- runSimulation(par)

plotSpatialPhylo(simu, plot = "both")

par(mfrow = c(1,2))
rac(simu$specMat)
sac(simu$specMat)


par <- createCompletePar(runs = c(100,200,300))
simu <- runSimulation(par)
str(simu)



par <- createCompletePar(type = "Rneutral")
simu <- runSimulation(par)
str(simu)
