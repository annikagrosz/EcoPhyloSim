#' @title  Species community model scenarios (Leipzig version)
#' @description A function to calculate several scenarios of a simple model of species community assembly following the design used by Chave et al. (2000), using parallel computing.
#' @param params  Character, The .xml file containing the model parameters as set by \code{\link{setParametersXML}}
#' @param cores  Integer, The number of cores reserved for parallel computing
#' @details This function uses the \pkg{foreach} and \pkg{doParallel} package to compute the model scenarios parallel on several cores. if you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed. 
#' @return a list of numerical matrices of species distribution and local density values for each scenario (each cell in a matrix represents an individual of a species)
#' @examples
#' # Define parameter values
#' scenarios = c("globalDens", "globalNoDens", 
#'               "localDens", "localNoDens")
#'x = rep(50,4)
#'y = rep(50,4)
#'runs = rep(3000,4)
#'dispersal = c(1,1,2,2)
#'nSpec = rep(1,4)
#'specRate = rep(2,4)
#'density = c(T,F,T,F)
#'densityStrength =  rep(0.4,4)
#'path <- "C:/test.xml"
#'
#'# Set parameters for the model 
#'setParametersXML(scenarios = scenarios, x = x, y = y, runs = runs, 
#'                 dispersal = dispersal, nSpec = nSpec, 
#'                 specRate = specRate, 
#'                 density = density, densityStrength = densityStrength, 
#'                 location = path)
#'
#'# Run the model with the set parameters on 2 cores
#' Out <- leipScenarios(params = path, cores = 2)
leipScenarios <- function(params, cores=NULL){
  if (is.null(cores)) cores <- detectCores()
  cl <- makeCluster(cores)
  #Load the .xml file and set the parameters to the right class
  f <- xmlToDataFrame(doc = params, colClasses = c("character",rep("numeric", 8)))
  f$density <- as.logical(f$density)
 
  # Start timing
  ptm <- proc.time()
  
  #Run the model with different scenarios on severel cores (provided by the "cores" argument)
  registerDoParallel(cl)
  out <- foreach(i=1:nrow(f), .packages = c("PhylGeo", "kulife", "XML", "XML2R")) %dopar%{
    attach(f)
    outVec <- rep.int(0,x[i]*y[i])
   .C(callLeipzig, as.integer(x[i]),as.integer(y[i]), as.integer(dispersal[i]), as.integer(nSpec[i]),as.integer(specRate[i]), as.integer(runs[i]), as.logical(density[i]), as.numeric(densityStrength[i]), specOut = as.integer(outVec), densOut = as.numeric(outVec))
  detach(f)}
  
  #Extract the ouput matrices from the .C output
  specMat <- list()
  densMat <- list()
  for(j in 1:nrow(f)){
  specMat[[j]] = matrix(out[[j]][[9]],ncol=x[j], nrow=y[j])
  densMat[[j]] = matrix(out[[j]][[10]],ncol=x[j], nrow=y[j])
  }
  #Name output matrices accoridng to the scenarios
  names(specMat) <- scenarios
  names(densMat) <- scenarios
  #Stop timing
  stopCluster(cl)
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((time)[3])/60), "minute(s) and", ((time)[3])%%60, "second(s)."))

  return(list(specMat = specMat, densMat=densMat))
 }

########################################################
# Complex Model
########################################################

#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms
#' @param parameters .xml file containing all parameters for the model scenarios (default is NULL)
#' @param cores The number of cores to be used for parallel computing (default is NULL)
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 3 = Local dispersal 
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param environment Logical determining whether or not the environment influences the model
#' @param neutral Logical determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param dispersalCut Integer defining the maximum dispersal distance of the kernel
#' @param densityCut Integer defining the range for the density dependence
#' @param seed Integer setting the random seed for the model
#' @param backup Logical determining whether the results of the individual scenario runs should be saved as a workspace image (advised if the simulation takes a long time, or if the individual scenarios vary greatly in runtime.)
#' @return A list containing numerical matrices of species distribution, local trait values and the environment for each scenario (each cell in a matrix represents an individual of a species), plus a phylogeny object of class "phylo" for each scenario.
#' @details This function uses the \code{\link{foreach}} and \code{\link{doParallel}} package to compute the model scenarios parallel on several cores. if you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed.\cr \cr The phylogeny is passed to R in the newick format and parsed to an object of class "phylo" with the function \code{\link[ape]{read.tree}} from the \code{\link{ape}} package. \cr \cr If no .xml parameter file is supplied, a single scenario can be computed by manually setting the parameters.
fullModParallel <- function(parameters = NULL, cores = NULL,x = NULL, y = NULL, dispersal = NULL, runs = NULL, specRate = NULL, density = NULL, environment = NULL, neutral = NULL, dispersalCut = NULL, densityCut = NULL, seed=NULL, backup = FALSE){
  
  #start timing
  ptm <- proc.time() 
  # check if .xml file was passed
  if (is.null(parameters) == F){
  pars<- xmlToDataFrame(doc = parameters, colClasses = c("character",rep("numeric", 11)))
  

 pars$density <- as.logical(pars$density)
 pars$environment <- as.logical(pars$environment)
 pars$neutral <- as.logical(pars$neutral)
 
 for(k in 1:nrow(pars)){
   if(pars$neutral[k] == TRUE){
     pars$density[k] = FALSE
     pars$environment[k]  = FALSE
   }
 }

  if (is.null(cores)) cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  out <- foreach(i=1:nrow(pars), .packages = c("PhylGeo", "kulife", "XML", "XML2R")) %dopar%{
    
    outVec <- rep.int(0,pars$x[i]*pars$y[i])
   OUT <- try(.C(callModel, as.integer(pars$x[i]),as.integer(pars$y[i]), as.integer(pars$dispersal[i]), as.integer(pars$runs[i]), as.numeric(pars$specRate[i]), as.logical(pars$density[i]),as.logical(pars$environment[i]),    as.logical(pars$neutral[i]), as.integer(pars$dispersalCut[i]), as.integer(pars$densityCut[i]), as.integer(pars$seed[i]), specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec),compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length=1)), TRUE)
   if(backup == TRUE){
   name <- paste(pars$scenarios[i], ".RData", sep="", collapse="")
   save(OUT, file = name)
   }
   OUT
  }
  
  
  specMat <- list()
  traitMat <- list()
  neutralMat <- list()
  competitionMat <- list()
  envMat <- list()
  phylo <- list()
  for(j in 1:nrow(pars)){
    specMat[[j]] = matrix(out[[j]][[12]],ncol=pars$x[j], nrow=pars$y[j])
    traitMat[[j]] = matrix(out[[j]][[13]],ncol=pars$x[j], nrow=pars$y[j])
    neutralMat[[j]] = matrix(out[[j]][[14]],ncol=pars$x[j], nrow=pars$y[j])
    competitionMat[[j]] = matrix(out[[j]][[15]],ncol=pars$x[j], nrow=pars$y[j])
    envMat[[j]] = matrix(out[[j]][[16]],ncol=pars$x[j], nrow=pars$y[j])
    phylo[[j]] = read.tree(text = out[[j]][[17]])
  }
  #Name output matrices accoridng to the scenarios
  names(specMat) <- scenarios
  names(traitMat) <- scenarios
  names(neutralMat) <- scenarios
  names(competitionMat) <- scenarios
  names(envMat) <- scenarios
  names(phylo) <- scenarios
  #Stop cluster and timing
  stopCluster(cl)
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, traitMat=traitMat, neutralMat = neutralMat, competitionMat = competitionMat, environment = envMat, phylogeny = phylo, scenarios = pars))
  }
  
  else if(is.null(parameters) == TRUE){
   if(neutral == TRUE){
      density = FALSE
      environment  = FALSE
    }
  
  outVec <- rep.int(0,x*y)
  
  out <- .C(callModel, as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), as.logical(density),as.logical(environment), as.logical(neutral), as.integer(dispersalCut), as.integer(densityCut), as.integer(seed), specOut = as.integer(outVec), traitOut = as.numeric(outVec), neutralOut = as.numeric(outVec), competitionOut = as.numeric(outVec),  envOut = as.numeric(outVec), phyloOut = character(length=1))[12:17]
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  traitMat = matrix(out[[2]],ncol=x, nrow=y)
  neutralMat = matrix(out[[3]],ncol=x, nrow=y)
  competitionMat = matrix(out[[4]],ncol=x, nrow=y)
  envMat = matrix(out[[5]],ncol=x, nrow=y)
  phylogeny = read.tree(text = out[[6]])
  #stop timing  
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, traitMat=traitMat, neutralMat = neutralMat, competitionMat = competitionMat, environment = envMat, phylogeny = phylogeny))
  }
}


