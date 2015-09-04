#' @title  Species community model scenarios (Leipzig version)
#' @description A function to calculate several scenarios of a simple model of species community assembly following the design used by Chave et al. (2000), using parallel computing.
#' @param params  Character, The .xml file containing the model parameters as set by \code{\link{setParametersXML}}
#' @param cores  Integer, The number of cores reserved for parallel computing
#' @details This function uses the \pkg{foreach} and \pkg{doParallel} package to compute the model scenarios parallel on several cores. if you want to keep working on your computer make sure to reserve at least one core for your other endevors (by assigning n-1 cores to the function). By default all cores are employed to ensure maximum speed. 
#' @return a list of numerical matrices of species distribution and local density values for each scenario (each cell in a matrix represents an individual of a species)
#' @export
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
#'setLeipzigParametersXML(scenarios = scenarios, x = x, y = y, runs = runs, 
#'                 dispersal = dispersal, nSpec = nSpec, 
#'                 specRate = specRate, 
#'                 density = density, densityStrength = densityStrength, 
#'                 location = path)
#'
#'# Run the model with the set parameters on 2 cores
#' Out <- leipScenarios(params = path, cores = 2)
leipScenarios <- function(params, cores=NULL){
  if (is.null(cores)) cores <- detectCores()
  cl <- parallel::makeCluster(cores)
  #Load the .xml file and set the parameters to the right class
  f <- XML::xmlToDataFrame(doc = params, colClasses = c("character",rep("numeric", 8)))
  f$density <- as.logical(f$density)
  
  # Start timing
  ptm <- proc.time()
  
  #Run the model with different scenarios on severel cores (provided by the "cores" argument)
  doParallel::registerDoParallel(cl)
  out <- foreach::foreach(i=1:nrow(f), .packages = c("PhylGeo")) %dopar%{
    attach(f)
    outVec <- rep.int(0,x[i]*y[i])
    .C(callLeipzig, as.integer(x[i]),as.integer(y[i]), as.integer(dispersal[i]), as.integer(nSpec[i]),as.integer(specRate[i]), as.integer(runs[i]), as.logical(density[i]), as.numeric(densityStrength[i]), specOut = as.integer(outVec), densOut = as.numeric(outVec))
    detach(f)}
  print(out)
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
  parallel::stopCluster(cl)
  time <- proc.time() - ptm
  print (paste("Finished after",floor(((time)[3])/60), "minute(s) and", ((time)[3])%%60, "second(s)."))
  
  return(list(specMat = specMat, densMat=densMat))
}