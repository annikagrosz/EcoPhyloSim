require(PhylGeo)

# SET WD TO SOURCE FILE LOCATION !!!!

modes <- c(rep("global",4), rep("local",16))
modesDists <- paste(modes, c(rep("0",4),as.character(sort(rep(c(0.5,1,2,4),4)))),sep = "")
modesDistsMechs <- paste(modesDists, rep(c("Envi", "Neut", "Dens", "Hybr"),4), sep = "")


pars <-data.frame(
  n <- length(modes),
  modes = modes,
  scenarios = modesDistsMechs,
  x = rep(256,n),
  y = rep(256,n),
  runs = rep(100000,n),
  dispersal = c(rep(0,4), rep(0.5,4), rep(1,4), rep(2,4), rep(4,4)),
  specRate = rep(1.0,n),
  density = rep(c(F,F,T,T),5),
  environment = rep(c(T,F,F,T),5),
  fitnessBaseMortalityRatio = rep(10,n),
  densityCut = rep(1,n),
  seed <- rep(1000,n)
)



system.time(simulationOut <- fullModBatch(pars, parallel = T)) # Set cores depending on cluster size (optimal: nCores = nScenarios) !!

save(simulationOut, pars, file = "results3.Rdata" )

