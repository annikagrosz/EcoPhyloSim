#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms
#' @param x  Integer, Dimension of the model landscape in x-direction
#' @param y  Integer, Dimension of the model landscape in y-direction
#' @param dispersal Integer, 1 = Gloabl dispersal, 2 = Nearest neighbor dispersal 
#' @param runs  Integer, Number of generations the model runs over
#' @param specRate Integer, Number of Individuals introduced to the community in each generation
#' @param density Logical, determining whether or not density dependence influences the model
#' @param environment Logical, determining whether or not the environment influences the model
#' @param neutral Logical, determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param dispersalCut Integer, defines the dispersal distance for local dispersal (ignored if dispersal = 1 (Global))
#' @param densityCut Integer, defines the effective range of the competition (ignored if density = FALSE)
#' @param seed numerical, sets the random seed
#' @return A List containing sveral numerical matrices with species (each cell represents an individual of a species), environment and different traits as well as the phylogeny as an object of class 'phylo'.
#' @examples 
#' # Run the model
#' myModel <- fullMod(x = 50, y = 50, dispersal = 1, runs = 100, specRate = 1, density = FALSE, environment = FALSE, neutral = TRUE, seed = 1500)
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
fullMod <- function(x = NULL, y = NULL, dispersal = NULL, runs = NULL, specRate = NULL, density = NULL, environment = NULL, neutral = NULL, dispersalCut = 2, densityCut = 1, seed=NULL)
{
  ptm <- proc.time()
  outVec <- rep.int(0,x*y)
  if(neutral == TRUE)
  {
    density = FALSE
    environment  = FALSE
  }
  
  out <- .C(callModel, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), as.logical(density),as.logical(environment), as.logical(neutral), as.integer(dispersalCut), as.integer(densityCut), as.integer(seed), specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec),compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length = 1))[12:17]
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
  return(list(specMat = specMat, traitMat=traitMat, envMat = envMat, compMat = compMat, neutMat = neutMat, phylogeny = phylogeny, phyloTXT = out[[6]]))
}