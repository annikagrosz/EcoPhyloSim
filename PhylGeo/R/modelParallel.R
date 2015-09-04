########################################################
# Complex Model
########################################################

#' @title  Species community Simulation
#' @description A model of species community assembly under different assembly mechanisms, using parallel computing to make use of multi core cpus and clusters in order to reduce computation time.
#' @param XMLfile .xml file containing all parameters for the model scenarios (must be set)
#' @param parallel The number of cores to be used for parallel computing (default null is no parallelization)
#' @param saveLocation Path of the folder to save the extracted files to
#' @param backup Logical determining whether the results of the individual scenario runs should be saved as a workspace image (advised if the simulation takes a long time, or if the individual scenarios vary greatly in runtime. Default is FALSE)
#' @return A list containing numerical matrices of species distribution, local trait values and the environment for each scenario (each cell in a matrix represents an individual of a species), plus a phylogeny object of class "phylo" for each scenario.
#' @details This function uses the \code{\link{foreach}} and \code{\link{doParallel}} package to compute the model scenarios parallel on several cores. if you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed.\cr \cr The phylogeny is passed to R in the newick format and parsed to an object of class "phylo" with the function \code{\link[ape]{read.tree}} from the \code{\link{ape}} package. \cr \cr If no .xml parameter file is supplied, a single scenario can be computed by manually setting the parameters. \cr \cr If mortalityFitness = TRUE, the number of reproduction events per generation is doubled. The mortality strength is an Integer value which determines the number of events per generation in which the mortality depends on fitness. \cr E.g. if mortalityStrengt = 100 for every 100 events with fitness dependent mortality there is 1 run with random mortality. \cr This way the random base mortality can be increased or decreased. The higher the frequency of the random base mortality the more neutral the conditions get. 
#' @export
fullModBatch <- function(pars, parallel = F, backup = FALSE){
  #start timing
  ptm <- proc.time() 
  
  # getParametersXML(XMLfile)
  
  if (parallel != F){
    cat("running ",length(pars), " batch simualations with parallelization", "\n")
    
    if (parallel == T | parallel == "auto") cores <- parallel::detectCores() - 1
    if (is.numeric(parallel)) cores <- parallel
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    out <- foreach(i=1:length(pars), .packages = c("PhylGeo")) %dopar%{
      
      par = pars[[i]]
      
      OUT <- fullMod(x = par$x, y = par$y, dispersal = par$dispersal, runs = par$runs, specRate = par$specRate, density = par$density, environment = par$environment, fitnessBaseMortalityRatio = par$fitnessBaseMortalityRatio, densityCut = par$densityCut, seed=par$seed, saveTimes = "last")
      
      if(backup == TRUE){
        name <- paste(par$scenarios, ".RData", sep="", collapse="")
        save(OUT, file = name)
      }
      OUT$par = par
      OUT
    }
  }else{
    cat("running ",nrow(pars), " batch simualations without parallelization", "\n")
    out <- foreach(i=1:length(pars), .packages = c("PhylGeo")) %do%{
      
      par = pars[[i]]
      
      cat("running parameter", i , "\n")
      
      OUT <- fullMod(x = par$x, y = par$y, dispersal = par$dispersal, runs = par$runs, specRate = par$specRate, density = par$density, environment = par$environment, fitnessBaseMortalityRatio = par$fitnessBaseMortalityRatio, densityCut = par$densityCut, seed=par$seed)
      
      if(backup == TRUE){
        name <- paste(par$scenarios, ".RData", sep="", collapse="")
        save(OUT, file = name)
      }
      OUT$par = par
      OUT
    }
  }
  
  for (i in 1:length(pars)) names(out)[i] <- pars[i]$scenario
  #Stop cluster and timing
  parallel::stopCluster(cl)
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  
  return(out)
}




