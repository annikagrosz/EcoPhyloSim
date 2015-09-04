#' @title  Species community model after chave et al (2000), as designed in leipzig
#' @description A simple model of species community assembly following the design used by Chave et al. (2000).
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 2 = Nearest neighbor dispersal 
#' @param nSpec  Number of initial Species in the model landscape
#' @param specRate Numerical the number of species introduced into the community during each generation
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param densityStrength Numerical (0-1) determining the strength of the density dependence
#' @return a numerical matrix (each cell represents an individual of a species)
#' @export
leip <- function(params)
{
  f <- XML::xmlToDataFrame(doc = params, colClasses = rep("numeric", 8))
  f$density <- as.logical(f$density)
  attach(f)
  ptm <- proc.time()
  outVec <- rep.int(0,x*y)
  out <- .C(callLeipzig, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(nSpec),as.integer(specRate), as.integer(runs), as.logical(density), as.numeric(densityStrength), specOut = as.integer(outVec), densOut = as.numeric(outVec))[9:10]
  
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  densMat = matrix(out[[2]],ncol=x, nrow=y)
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, densMat=densMat))
  detach(f)
}