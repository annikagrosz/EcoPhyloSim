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

  if(density == FALSE & environment == FALSE)
  {
    neutral = TRUE
  }else{
    neutral = F
  }
  if (length(runs)>1){
    if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
  }
  
  
  ptm <- proc.time()
  
  out <- callModel( x,  y,  dispersal,  runs,  specRate, density, environment, neutral, mortalityFitness, fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, densityCut, seed)  
  
  print (paste("Core simulation finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s). Converting data"))
  
  for (i in 1:length(runs)){
    specMat = matrix(out[[i]][[1]],ncol=x, nrow=y)
    traitMat = matrix(out[[i]][[2]],ncol=x, nrow=y)
    neutMat = matrix(out[[i]][[3]],ncol=x, nrow=y)
    compMat = matrix(out[[i]][[4]],ncol=x, nrow=y)
    envMat = matrix(out[[i]][[5]],ncol=x, nrow=y)
    phylogeny = ape::read.tree(text = out[[i]][[6]])
    temp = list(
      specMat = specMat, 
      traitMat=traitMat, 
      envMat = envMat, 
      compMat = compMat, 
      neutMat = neutMat, 
      phylogeny = phylogeny, 
      phyloTXT = out[[i]][[6]],
      seed = seed,
      runs = i)
    out[[i]] = temp
  }
  if (length(runs) == 1)out = out[[i]]
  cat("done! \n")
  return(out)
}



# neutral = T
# x = 50
# y = 50
# dispersal = 0.5
# runs = c(100,200)
# specRate = 1.0
# density = F
# environment = F
# seed = 1
# fitnessBaseMortalityRatio = 5
# fitnessActsOn = "mortality" 
# densityCut = 1
# saveTimes = "last"
# 
# 
# if (dispersal == "global" | dispersal == 0){
#   dispersal = 1
#   dispersalCut = 1
# }else if (is.numeric(dispersal)){
#   if (dispersal < 0.5) stop("dispersal parameter too small") 
#   dispersalCut = 2*dispersal
#   dispersal = 3
# }else stop("wrong dispersal parameter") 
# 
# if(fitnessActsOn == "mortality"){
#   reproductiveFitness = F
#   mortalityFitness = T    
# }else if(fitnessActsOn == "reproduction"){
#   reproductiveFitness = T
#   mortalityFitness = F    
# }else if (fitnessActsOn == "both"){
#   reproductiveFitness = T
#   mortalityFitness = T    
# }else stop("wrong fitness parameters")
# 
# if(density == FALSE & environment == FALSE)
# {
#   neutral = TRUE
# }else{
#   neutral = F
# }
# if (length(runs)>1){
#   if (any(runs[-length(runs)] > runs[-1])) stop( "wrong argument to runs")
# }
# 
# 
# out <- callModel( x,  y,  dispersal,  runs,  specRate, density, environment, neutral, mortalityFitness, fitnessBaseMortalityRatio, reproductiveFitness, dispersalCut, densityCut, seed)  


