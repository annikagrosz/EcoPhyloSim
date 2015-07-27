########################################################
# Complex Model
########################################################

#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms, using parallel computing to make use of multi core cpus and clusters in order to reduce computation time.
#' @param parameters .xml file containing all parameters for the model scenarios (default is NULL)
#' @param cores The number of cores to be used for parallel computing (default is NULL)
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 3 = Local dispersal 
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param environment Logical determining whether or not the environment influences the model
#' @param mortalityFitness Logical, determining whether or not the individual fitness influences mortality
#' @param mortalityStrength Integer determining the influence of fitness on mortality
#' @param reproductiveFitness Logical, determining whether or not the individual fitness influences reproduction
#' @param neutral Logical determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param dispersalCut Integer defining the maximum dispersal distance of the kernel
#' @param densityCut Integer defining the range for the density dependence
#' @param seed Integer setting the random seed for the model
#' @param saveLocation Path of the folder to save the extracted files to
#' @param backup Logical determining whether the results of the individual scenario runs should be saved as a workspace image (advised if the simulation takes a long time, or if the individual scenarios vary greatly in runtime. Default is FALSE)
#' @return A list containing numerical matrices of species distribution, local trait values and the environment for each scenario (each cell in a matrix represents an individual of a species), plus a phylogeny object of class "phylo" for each scenario.
#' @details This function uses the \code{\link{foreach}} and \code{\link{doParallel}} package to compute the model scenarios parallel on several cores. if you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed.\cr \cr The phylogeny is passed to R in the newick format and parsed to an object of class "phylo" with the function \code{\link[ape]{read.tree}} from the \code{\link{ape}} package. \cr \cr If no .xml parameter file is supplied, a single scenario can be computed by manually setting the parameters. \cr \cr If mortalityFitness = TRUE, the number of reproduction events per generation is doubled. The mortality strength is an Integer value which determines the number of events per generation in which the mortality depends on fitness. \cr E.g. if mortalityStrengt = 100 for every 100 events with fitness dependent mortality there is 1 run with random mortality. \cr This way the random base mortality can be increased or decreased. The higher the frequency of the random base mortality the more neutral the conditions get. 
fullModParallel <- function(parameters = NULL, cores = NULL,x = NULL, y = NULL, dispersal = NULL, runs = NULL, specRate = NULL, density = NULL, environment = NULL,  mortalityFitness = NULL, mortalityStrength = NULL, reproductiveFitness = NULL, neutral = NULL, dispersalCut = NULL, densityCut = NULL, seed=NULL, saveLocation = FALSE, backup = FALSE)
{
  #start timing
  ptm <- proc.time() 
  # check if .xml file was passed to function
  if (is.null(parameters) == F){
  pars<- XML::xmlToDataFrame(doc = parameters, colClasses = c("character",rep("numeric", 11)))
  

 pars$density <- as.logical(pars$density)
 pars$environment <- as.logical(pars$environment)
 pars$neutral <- as.logical(pars$neutral)
 pars$mortalityFitness <- as.logical(pars$mortalityFitness)
 pars$reproductiveFitness <- as.logical(pars$reproductiveFitness)
 
 for(k in 1:nrow(pars)){
   if(pars$neutral[k] == TRUE){
     pars$density[k] = FALSE
     pars$environment[k]  = FALSE
   }
 }

  if (is.null(cores)) cores <- detectCores()
  cl <- parallel::makeCluster(cores)
  doParalell::registerDoParallel(cl)
  out <- foreach::foreach(i=1:nrow(pars), .packages = c("PhylGeo")) %dopar%{
    
    outVec <- rep.int(0,pars$x[i]*pars$y[i])
   OUT <- try(.C(callModel, 
                 as.integer(pars$x[i]),as.integer(pars$y[i]), as.integer(pars$dispersal[i]), 
                 as.integer(pars$runs[i]), as.numeric(pars$specRate[i]),
                 as.logical(pars$density[i]),as.logical(pars$environment[i]),as.logical(pars$mortalityFitness[i]),
                 as.integer(pars$mortalityStrength[i]), as.logical(pars$reproductiveFitness[i]), as.logical(pars$neutral[i]), 
                 as.integer(pars$dispersalCut[i]), as.integer(pars$densityCut[i]),
                 as.integer(pars$seed[i]), as.character(pars$saveLocation[i]),
                 specOut = as.integer(outVec), traitOut = as.numeric(outVec),neutralOut = as.numeric(outVec),
                 compOut = as.numeric(outVec), envOut = as.numeric(outVec), phyloOut = character(length=1)), TRUE)
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
    specMat[[j]] = matrix(out[[j]][[16]],ncol=pars$x[j], nrow=pars$y[j])
    traitMat[[j]] = matrix(out[[j]][[17]],ncol=pars$x[j], nrow=pars$y[j])
    neutralMat[[j]] = matrix(out[[j]][[18]],ncol=pars$x[j], nrow=pars$y[j])
    competitionMat[[j]] = matrix(out[[j]][[19]],ncol=pars$x[j], nrow=pars$y[j])
    envMat[[j]] = matrix(out[[j]][[20]],ncol=pars$x[j], nrow=pars$y[j])
    phylo[[j]] = ape::read.tree(text = out[[j]][[21]])
  }
  #Name output matrices accoridng to the scenarios
  names(specMat) <- scenarios
  names(traitMat) <- scenarios
  names(neutralMat) <- scenarios
  names(competitionMat) <- scenarios
  names(envMat) <- scenarios
  names(phylo) <- scenarios
  #Stop cluster and timing
  parallel::stopCluster(cl)
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
  
  out <- .C(callModel, 
            as.integer(x),as.integer(y), as.integer(dispersal), as.integer(runs), as.numeric(specRate), 
            as.logical(density),as.logical(environment), as.logical(mortalityFitness), as.integer(mortalityStrength), 
            as.logical(reproductiveFitness), as.logical(neutral), 
            as.integer(dispersalCut), as.integer(densityCut), as.integer(seed), as.character(saveLocation),
            specOut = as.integer(outVec), traitOut = as.numeric(outVec), neutralOut = as.numeric(outVec), 
            competitionOut = as.numeric(outVec),  envOut = as.numeric(outVec), phyloOut = character(length=1))[16:21]
    if(backup == TRUE){
     save(out, file = "Backup.RData")
   }
  specMat = matrix(out[[1]],ncol=x, nrow=y)
  traitMat = matrix(out[[2]],ncol=x, nrow=y)
  neutralMat = matrix(out[[3]],ncol=x, nrow=y)
  competitionMat = matrix(out[[4]],ncol=x, nrow=y)
  envMat = matrix(out[[5]],ncol=x, nrow=y)
  phylogeny = ape::read.tree(text = out[[6]])
  #stop timing  
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return(list(specMat = specMat, traitMat=traitMat, neutralMat = neutralMat, competitionMat = competitionMat, environment = envMat, phylogeny = phylogeny))
  }
}


