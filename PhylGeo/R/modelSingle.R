#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 2 = Nearest neighbor dispersal 
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param environment Logical determining whether or not the environment influences the model
#' @param neutral Logical determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @return a numerical matrix (each cell represents an individual of a species)
fullMod <- function(x = NULL, y = NULL, dispersal = NULL, runs = NULL, specRate = NULL, density = NULL, environment = NULL, neutral = NULL, dispersalCut = NULL, densityCut = NULL, seed=NULL)
{
  ptm <- proc.time()
  outVec <- rep.int(0,x*y)
  if(neutral == TRUE)
  {
    density = FALSE
    environment  = FALSE
  }
  out <- .C(callModel, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), as.logical(density),as.logical(environment), as.logical(neutral), as.integer(dispersalCut), as.integer(densityCut), as.integer(seed), specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec),compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length=1))[12:17]
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  traitMat = matrix(out[[2]],ncol=x, nrow=y)
  neutMat = matrix(out[[3]],ncol=x, nrow=y)
  compMat = matrix(out[[4]],ncol=x, nrow=y)
  envMat = matrix(out[[5]],ncol=x, nrow=y)
  phylogeny = ape::read.tree(text = out[[6]])
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, traitMat=traitMat, envMat = envMat, neutMat = neutMat, phylogeny = phylogeny, spec = out[[1]]))
}