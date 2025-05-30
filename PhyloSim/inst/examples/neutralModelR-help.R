# Run the model
metaCom <- NeutralMod(xdim=50, ydim=50, specRate=2, seed=1500, runs=500)
image(metaCom)

# Usually the function is called by the runSimualtion function
# Define a parameter set
parNeut <- createCompletePar(x = 50, y = 50, dispersal = FALSE , runs = 500,
        density = 1, environment = 0.5, specRate = 1, type="Rneutral")

# Run the model
simuNeut <- runSimulation(parNeut)

# Visualize the grid
image(simuNeut$Output[[1]]$specMat)
