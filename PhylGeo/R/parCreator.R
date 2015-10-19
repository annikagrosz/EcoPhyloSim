#' @title  parameter generator
#' @description Function to create a list with parameters for the biogeographical simulations
#' @param x  Integer, Dimension of the model landscape in x-direction
#' @param y  Integer, Dimension of the model landscape in y-direction
#' @param dispersal Integer, 0 or "global" = Gloabl dispersal, all integers >=1 = Nearest neighbor dispersal  
#' @param runs  Integer, Number of generations the model runs over. You can also define a sequence of runs. Then the intermediate and end results are saved.
#' @param specRate Integer, Number of Individuals introduced to the community in each generation
#' @param density Logical, determining whether or not density dependence influences the model
#' @param environment Logical, determining whether or not the environment influences the model
#' @param mortalityFitness Logical, determining whether or not the individual fitness influences mortality
#' @param mortalityStrength Integer determining the influence of fitness on mortality (see Details)
#' @param reproductiveFitness Logical, determining whether or not the individual fitness influences reproduction
#' @param neutral Logical, determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param dispersalCut Integer, defines the dispersal distance for local dispersal (ignored if dispersal = 1 (Global))
#' @param densityCut Integer, defines the effective range of the competition (ignored if density = FALSE)
#' @param seed numerical, sets the random seed
#' @param saveLocation Path of the folder to save the backup files to
#' @details If mortalityFitness = TRUE, the number of reproduction events per generation is doubled. The mortality strength is an Integer value which determines the number of events per generation in which the mortality depends on fitness. \cr E.g. if mortalityStrengt = 100 for every 100 events with fitness dependent mortality there is 1 run with random mortality. \cr This way the random base mortality can be increased or decreased. The higher the frequency of the random base mortality the more neutral the conditions get. 
#' @return A List with parameters
createCompletePar <- function(x = 50, y = 50, dispersal = "global", runs = 100, specRate = 1.0, density = F, environment = F, fitnessActsOn = "mortality" , fitnessBaseMortalityRatio = 10, densityCut = 1, seed=NULL,  type = "base", modes = NULL, scenario = NULL){
    
  
  if (length(runs)>1){
    if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
  }
  
  if (is.null(seed)) seed = sample(1:10000,1)
  
  par = list(x=x,y=y,dispersal = dispersal, runs = runs, specRate = specRate, density = density, environment = environment, fitnessActsOn=fitnessActsOn, fitnessBaseMortalityRatio=fitnessBaseMortalityRatio, densityCut = densityCut, seed = seed, type = type, scenario = scenario)
  

  return(par)
  
}

