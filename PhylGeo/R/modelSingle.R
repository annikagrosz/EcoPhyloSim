#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms
#' @param x  Integer, Dimension of the model landscape in x-direction
#' @param y  Integer, Dimension of the model landscape in y-direction
#' @param dispersal Integer, 1 = Gloabl dispersal, 2 = Nearest neighbor dispersal 
#' @param runs  Integer, Number of generations the model runs over
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
#' @return A List containing sveral numerical matrices with species (each cell represents an individual of a species), environment and different traits as well as the phylogeny as an object of class 'phylo'.
#' @examples 
#' # Run the model
#' myModel <- fullMod(x = 50, y = 50, dispersal = 1, runs = 100, specRate = 1.0, density = FALSE, environment = FALSE, neutral = TRUE, seed = 1500)
#' 
#'  # Look at the phylogeny (requires package 'ape')
#'  require(ape)
#'  
#'  # Get the phylogeny
#'  phylogeny <- myModel$phylogeny
#'  
#'  # Only extant taxa
#'  extantPhylogeny <- drop.fossil(phylogeny)
#'  
#'  plot(extantPhylogeny)
#'  
#'  #Look at the species area relation
#'  
#'  species <- myModel$specMat
#'  sac(area = c(1,10,100,1000), matrix = species, rep = 100, plot= T)
fullMod <- function(x = 100, y = 100, dispersal = "global", runs = 100, specRate = 1.0, density = F, environment = F, fitnessActsOn = "mortality" , fitnessBaseMortalityRatio = 10, densityCut = 1, seed=NULL, saveTimes = "last")
{
  
  if (is.null(seed)) seed = sample(1:10000,1)
  
  if (dispersal == "global" | dispersal == 0){
    dispersal = 1
    dispersalCut = 1
  }else if (is.numeric(dispersal)){
    if (dispersal < 0.5) stop("dispersal parameter too small") 
    dispersalCut = 2*dispersal
    dispersal = 3
  }else stop("wrong dispersal parameter") 
  
  if(fitnessActsOn == "mortality"){
    reproductiveFitness = F
    mortalityFitness = T    
  }else if(fitnessActsOn == "reproduction"){
    reproductiveFitness = T
    mortalityFitness = F    
  }else if (fitnessActsOn == "both"){
    reproductiveFitness = T
    mortalityFitness = T    
  }else stop("wrong fitness parameters")
  
  # TODO exists

  ptm <- proc.time()
  outVec <- rep.int(0,x*y)
  if(density == FALSE & environment == FALSE)
  {
    neutral = TRUE
  }else{
    neutral = F
  }
  
  # saveTimes TODO implement save time!!!!
    
  
  saveLocation = "./" # only for backword compatibility, remove save location from call 
  
  
  #out <- .C("callModel", as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), #1-5
#             as.logical(density),as.logical(environment) ,as.logical(neutral), #6-8
#             as.logical(mortalityFitness), as.integer(mortalityStrength), as.logical(reproductiveFitness),   #9-11
#             as.integer(dispersalCut), as.integer(densityCut), as.integer(seed), as.character(saveLocation), #12-15
#             specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec), #16-18 Output starts here
#             compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length = 1),
#             PACKAGE = "PhylGeo")[16:21] #19-21
#   
  out <- callModel( x,  y,  dispersal,  runs,  specRate, density, 
                    environment, neutral, mortalityFitness, fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, 
                    densityCut, seed, saveLocation, 
                    specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec),
                    compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length = 1)
    )  

  print("simulation is done")
  
  print("writing specMat")
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  print("writing traitMat")
  traitMat = matrix(out[[2]],ncol=x, nrow=y)
  print("writing neutMat")
  neutMat = matrix(out[[3]],ncol=x, nrow=y)
  print("writing compMat")
  compMat = matrix(out[[4]],ncol=x, nrow=y)
  print("writing envMat")
  envMat = matrix(out[[5]],ncol=x, nrow=y)
  print("writing phylogeny")
  
  phylogeny = ape::read.tree(text = out[[6]])
  
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  
  return(list(
    specMat = specMat, 
    traitMat=traitMat, 
    envMat = envMat, 
    compMat = compMat, 
    neutMat = neutMat, 
    phylogeny = phylogeny, 
    phyloTXT = out[[6]],
    seed = seed
    
    ))
}