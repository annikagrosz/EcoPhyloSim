require(PhylGeo)

# SET WD TO SOURCE FILE LOCATION !!!!

modes <- c(rep("global",4), rep("local",16))
modesDists <- paste(modes, c(rep("0",4),as.character(sort(rep(c(0.5,1,2,4),4)))),sep = "")
modesDistsMechs <- paste(modesDists, rep(c("Envi", "Neut", "Dens", "Hybr"),4), sep = "")


pars <- list()
n <- length(modes)

runs = seq(25000, 30000, len = 10)

for (i in 1:20){pars[[i]] = list(x = 50,y = 50,runs = 500,modes = modes[i],scenario = modesDistsMechs[i],dispersal = c(rep(0,4), rep(0.5,4), rep(1,4), rep(2,4), rep(4,4))[i],specRate = 1.0,density = rep(c(F,F,T,T),5)[i],environment = rep(c(T,F,F,T),5)[i], fitnessBaseMortalityRatio = 10, densityCut = 1,seed = 1000)}





system.time(simulationOut <- fullModBatch(pars, parallel = T)) # Set cores depending on cluster size (optimal: nCores = nScenarios) !!

save(simulationOut, pars, file = "results6.Rdata" )
