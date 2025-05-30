## First the experimental design has to be defined.
## In this example we will test seven different combinations of the parameters
## 'density and 'environment' under four different dispersal distances.
## This design leads to 28 experiments.

dispOptions <- 4
fitOptions <- 7

# These are the parameters that are tested.
# We are testing seven combinations of the parameters 'density' and 'environment'...
density <- rep(c(0,seq(0,1,len = fitOptions-2),1),dispOptions)
environment <- rep(c(0,seq(1,0,len = fitOptions-2),1),dispOptions)

# ... for four different dispersal ranges.
dispersal <-  rep(c(0,0.5, 2,4), each = fitOptions)

## The modes and scenarios are only defined to label the plots. They have no influence on the
## calculations.
modes <- ifelse(dispersal == 0, "global", "local")
scenarios <- paste(modes, " dens=", density, " env=", environment, sep = "")

## Now we can define a list of parameter sets.
pars <- list()
for (i in 1:length(scenarios)){pars[[i]] = createCompletePar(x = 20,y = 20, runs = 1000,scenario = scenarios[i], dispersal = dispersal[i],specRate = 2,density = density[i],environment = environment[i], fitnessBaseMortalityRatio = 5, densityCut = 1,seed = 1000)}

## Run the simulations
simulationOut <- runSimulationBatch(pars, parallel = 2)

## Calculate null models. For each model run 9 plot sizes are tested. In the plot they are
## used to calculate the confidence interval for the results.
nullMeta <- calculatePhylogeneticDispersion(simulationOut, plotlengths = c(2,3,4,5,6,7,8,9,10), reduce = TRUE)

## Now we can define the positions and labels of the plots.
## Note that here the 28 simulations need to be covered in the right order.
## In this example there will be 7 rows and 4 collumns to represent the 28 cases.
positions <- list(x= c(1:7),
   y = c(4, 1:3), # this is beacause dispersal = "global" is equal to dispersal = "0"
   zname = c("neutral", "1", "0.75", "0.5", "0.25", "0", "both"), # = environment
   yname = c("global", "0.5", "2", "4"),  # = dispersal
   xname=c("neutral", "0", "0.25", "0.5","0.75","1","both")) # = density

## Now you can plot the results.
plotPhylogeneticDispersion(pvalues = nullMeta, positions = positions, title = "Null Meta")

