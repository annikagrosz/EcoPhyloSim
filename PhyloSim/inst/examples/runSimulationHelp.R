# Define a parameter set
par <- createCompletePar(x = 50, y = 50, dispersal = 1 , runs = 1000,
                         density = 0, environment = 0, specRate = 1, 
                         fission = 0, redQueen=0, redQueenStrength=0,
                         protracted=0)

# Run the model
simu <- runSimulation(par)

plot(simu)

# Look at the phylogeny (requires package 'ape')
require(ape)

# Get the phylogeny of the last run. In this example this is after 1000 runs.
phylogeny <- simu$Output[[2]]$phylogeny
  
# Only extant taxa
extantPhylogeny <- drop.fossil(phylogeny)
  
# Display the results  
plot(extantPhylogeny)
  
# Look at the species area relation
sac(simu, area = c(1,10,100,1000), rep = 100, plot= TRUE)

## Define two parameter sets
par1 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),   
                          density = 0, environment = 0.5, specRate = 1) 
par2 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
                          density = 1, environment = 0.5, specRate = 1)
 
## Merge the parameter sets. It is important to note, that the function
## needs a list of parameter sets.
par <- list(par1,par2)
 
## Run the model
simu <- runSimulationBatch(par, parallel = 2) 

## Compare the results, here the SAC is shown exemplarily.
par(mfrow=c(1,2))
sac(simu[[1]])
sac(simu[[2]])

