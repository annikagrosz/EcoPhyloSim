
#' @title Batch runner
#' @description A model of species community assembly under different assembly mechanisms, using parallel computing to make use of multi core cpus and clusters in order to reduce computation time. The function is an extension to \link{runSimulation} in order to simulate multiple scenarios.
#' @param pars A single set of parameters as created by \code{\link{createCompletePar}} or a list of parameter sets
#' @param parallel Logical, determining whether parallel computing should be executed.
#' @param backup Logical determining whether the results of the individual scenario runs should be saved as a workspace image (advised if the simulation takes a long time, or if the individual scenarios vary greatly in runtime. Default is FALSE)
#' @return A list of Phylosim-Objects as created by \code{\link{runSimulation}}
#' @details This function uses the \code{\link{foreach}} and \code{\link{doParallel}} package to compute the model scenarios parallel on several cores. If you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed.\cr \cr The phylogeny is passed to R in the newick format and parsed to an object of class "phylo" with the function \code{\link[ape]{read.tree}} from the \code{\link{ape}} package. 
#' @examples 
#' ## Define two parameter sets
#' par1 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 0, environment = 0.5, specRate = 1) 
#' par2 <- createCompletePar(x = 50, y = 50, dispersal = 5 , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1)
#' 
#' ## Merge the parameter sets. It is important to note, that the funktion
#' ## needs a list of parameter sets.
#' par <- list(par1,par2)
#' 
#' ## Run the model
#' simu <- runSimulationBatch(par, parallel=T) 
#'
#' ## Compare the results
#' ##

#' @export
runSimulationBatch <- function(pars, parallel = F, backup = FALSE){
  #start timing
  ptm <- proc.time() 
  
  # TODO getParametersXML(XMLfile)
  
  if (parallel != F){
    cat("running ",length(pars), " batch simualations with parallelization", "\n")
    
    if (parallel == T | parallel == "auto") cores <- parallel::detectCores() - 1
    if (is.numeric(parallel)) cores <- parallel
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    out <- foreach(i=1:length(pars), .packages = c("PhylGeo")) %dopar%{
      
      OUT <- runSimulation(pars[[i]])
      
      if(backup == TRUE){
        name <- paste(pars[[i]]$scenario, ".RData", sep="", collapse="")
        save(OUT, file = name)
      }
      OUT
    }
   parallel::stopCluster(cl)
  }else{
    cat("running ",nrow(pars), " batch simualations without parallelization", "\n")
    out <- foreach(i=1:length(pars), .packages = c("PhylGeo")) %do%{
      
      cat("running parameter", i , "\n")
      
      OUT <- runSimulation(pars[[i]])
      
      if(backup == TRUE){
        name <- paste(pars[[i]]$scenario, ".RData", sep="", collapse="")
        save(OUT, file = name)
      }
      OUT
    }
  }
  
  for (i in 1:length(pars)) names(out)[i] <- pars[i]$scenario
  class(out) <- append(class(out),"PhylosimBatch")
  #Stop cluster and timing
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  
  return(out)
}




