## Define a parameter set
par <- createCompletePar(x = 50, y = 50, dispersal = FALSE , runs = c(500,1000),
        density = 1, environment = 0.5, specRate = 1)

## Run the model
simu <- runSimulation(par)
