#' @title  Parameter Generator 
#' @description Function to create a list with parameters for biogeographical simulations with \code{\link[PhylGeo]{runSimulation}} or \code{\link[PhylGeo]{runSimulationBatch}} 
#' @param x  Integer, Dimension of the model landscape in x-direction
#' @param y  Integer, Dimension of the model landscape in y-direction
#' @param dispersal Integer. Type 0 or "global" for global dispersion. For local dispersion all integers >=1 are possible. A higer the value represents a larger dispersal distance.
#' @param runs  Integer, Number of generations the model runs over. You can also define a sequence of runs. Then the intermediate and end results are saved.
#' @param specRate Integer, Number of Individuals introduced to the community in each generation
#' @param density Logical, determining whether or not density dependence influences the model
#' @param environment Logical, determining whether or not the environment influences the model
#' @param fitnessActsOn Character, determining how the fitness influences the individuals. Possible inputs are "mortality" (default), "reproduction" or "both"
#' @param fitnessBaseMortalityRatio Integer, TODO
#' @param densityCut Integer, defines the effective range of the competition (ignored if density = FALSE)
#' @param seed numerical, sets the random seed
#' @param type Character, determining which model should be used. "base" is running the default model. Other possibilities are "Leipzig" and "Rneutral" which will run a neutral model purely in R.
#' @details If type = "Rneutral" the model will run entirely in R. This model is to be seen only for test and teaching purpose. To be used in practice it is far too slow. Also the output is reduced in comparision with the other models. Only the species landscape and the parameter settings will be displayed in the output.
#' @return A List with parameters

createCompletePar <- function(x = 50, y = 50, dispersal = "global", runs = 100, specRate = 1.0, density = F, environment = F, fitnessActsOn = "mortality" , fitnessBaseMortalityRatio = 10, densityCut = 1, seed=NULL,  type = "base", modes = NULL, scenario = NULL){
    
  
  if (length(runs)>1){
    if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
  }
  
  if (is.null(seed)) seed = sample(1:10000,1)
  
  par = list(x=x,y=y,dispersal = dispersal, runs = runs, specRate = specRate, density = density, environment = environment, fitnessActsOn=fitnessActsOn, fitnessBaseMortalityRatio=fitnessBaseMortalityRatio, densityCut = densityCut, seed = seed, type = type, scenario = scenario)
  

  return(par)
  
}

