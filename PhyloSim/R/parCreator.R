#' @title  Parameter Generator 
#' @description Function to create a list of parameters for biogeographical simulations with \code{\link{runSimulation}} or \code{\link{runSimulationBatch}}
#' @param x  Integer, Dimension of the model landscape in x-direction
#' @param y  Integer, Dimension of the model landscape in y-direction
#' @param dispersal Integer. Type 0 or "global" for global dispersion. For local dispersion all integers >=1 set the dispersal distance.
#' @param runs  Integer, Number of generations or sequence of generations the model runs over (see Details). 
#' @param specRate Integer, Number of Individuals introduced to the community in each generation
#' @param density Float, determining whether or how strong the density dependence influences the model. By default (density=0) there is no density dependence. The higher the value of the parameter, the stronger is the density dependence.
#' @param environment Float, determining whether or how strong the environment influences the model.  By default (environment=0) there is no influence of the environment. The higher the value of the parameter, the stronger is the influence of the environment.
#' @param fitnessActsOn Character, determining how the fitness influences the individuals. Possible inputs are "mortality" (default), "reproduction" or "both"
#' @param fitnessBaseMortalityRatio Integer, determines the fitness based mortality ratio
#' @param densityCut Integer, defines the effective range of the competition (ignored if density = FALSE)
#' @param fission Integer, determining which fission type should be used. Options are 0 (none = default), 1 (every second individual becomes part of new species) and 2 (population is geographically split in two parts).
#' @param protracted Integer, determining the time span in generation new species stays 'incipient' befre turning into a 'good' species. Default is 0.
#' @param redQueenStrength Float, determining the strength of the Red Queen effect. A value > 0 mean a new species gets a fitness boost due to its novelty.
#' @param redQueen Float, determining the strength of the fitness decline of an aging species. 
#' @param seed numerical, sets the random seed
#' @param type Character, determining which model should be used. "base" is running the default model. Other possibilities are "Leipzig" and "Rneutral" which will run a neutral model purely in R.
#' @param scenario String, further information you want to add to the parameter set in order to refer to a model run more conveniently. 
#' @details If runs is a sequence of generations the intermediate and end results are saved in the output of \code{runSimulation}. \cr\cr If type = "Rneutral" the model will run entirely in R. This model is to be seen only for test and teaching purpose. To be used in practice it is far too slow. Also the output is reduced. Only the species landscape and the parameter settings will be displayed in the output.
#' @return A List with parameters
#' @examples 
#' ## Define a parameter set
#' par <- createCompletePar(x = 50, y = 50, dispersal = FALSE , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1)
#'
#' ## Run the model
#' simu <- runSimulation(par)
#' 
#' @export

createCompletePar <- function(x = 50, y = 50, dispersal = "global", runs = 100, specRate = 1.0, density = 0, environment = 0, fitnessActsOn = "mortality" , fitnessBaseMortalityRatio = 10, densityCut = 1, seed=NULL,  type = "base", fission = 1, redQueen = 0, redQueenStrength = 0, protracted = 0, airmat = 1, soilmat = 1, scenario = NULL){
    
  
  if (length(runs)>1){
    if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
  }
  
  if(length(airmat)!= 1){
    if((nrow(airmat) != y) | (ncol(airmat)!= x)) stop("Environment and matrix size do not match")
  }
  
  if(length(soilmat) != 1){
    if((nrow(soilmat) != y) | (ncol(soilmat)!= x)) stop("Environment and matrix size do not match")
  }
  
  airmat <- as.numeric(airmat)
  soilmat <- as.numeric(soilmat)
  
  if (is.null(seed)) seed = sample(1:10000,1)
  
  par = list(x=x,y=y,dispersal = dispersal, runs = runs, specRate = specRate, 
             density = density, environment = environment, fitnessActsOn=fitnessActsOn,
             fitnessBaseMortalityRatio=fitnessBaseMortalityRatio, densityCut = densityCut, 
             seed = seed, type = type, scenario = scenario, fission = fission, 
             redQueen = redQueen, redQueenStrength = redQueenStrength, protracted = protracted,
             airmatR = airmat, soilmatR=soilmat)
  


  return(par)
  
}

