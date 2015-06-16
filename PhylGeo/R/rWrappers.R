#' @title Integer multiplication
#' @description multiplies an integer with 2
#' @param x Integer value to be doubled
#' @return An Integer.
multiInt <- function(x){.C(multi_, as.integer(x), integer(1))[[2]]}

#' @title  Addition
#' @description sums up two values
#' @param x  value to be added
#' @param y  value to be added
#' @return a numerical value.
add <- function(x,y){.C(add_, x,y, numeric(1))[[3]]}


#' @title  Square
#' @description Squares the Value
#' @param x  value to be squared
#' @return a numerical value.
func <- function(x){.C(call_MyClass_func,x, numeric(1))[[2]]}

pois <- function(x){.C(callRandomGenRandomPoisson, x, numeric(1))[[2]]}



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
leip <- function(params){
  f <- xmlToDataFrame(doc = params, colClasses = rep("numeric", 8))
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
fullMod <- function(x, y, dispersal, runs, specRate, density, environment, neutral){
  ptm <- proc.time()
  outVec <- rep.int(0,x*y)
  if(neutral == TRUE){
    density = FALSE
    environment  = FALSE
  }
    out <- .C(callModel, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), as.logical(density),as.logical(environment), as.logical(neutral), specOut = as.integer(outVec), traitOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length=1))[9:12]
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  traitMat = matrix(out[[2]],ncol=x, nrow=y)
  envMat = matrix(out[[3]],ncol=x, nrow=y)
  phylogeny = read.tree(text = out[[4]])
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, traitMat=traitMat, envMat = envMat, phylogeny = phylogeny, spec = out[[1]]))
}