#' @title Null Model (sample abundance)
#' @description Generates a null model based on the abundance distribution of the sample plots to compare the observed results against.
#' @param simu an object of class "phylosim" as created by \code{\link{runSimulation}} or \code{\link{runSimulationBatch}}
#' @param which.simulation Integer, determines which result should be used. This argument is only usefull if interim steps are saved in the Phylosim object.
#' @param localPlotSize number of grid cells of the plots drawn within the metacommunity
#' @param numberOfPlots number of plots drawn within the metacommunity
#' @param repetitions number of generated null model plots
#' @return A numeric vector with pValues for each plot in the observed metacommunity
#' @examples 
#' 
#' ### Usage with runSimulation
#' ## Define a parameter set
#' par <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000), density = 1, environment = 0.5, specRate = 1)
#' 
#' ## Run Model
#' simu <- runSimulation(par)
#' 
#' ## Compare to null model
#' nullModelSample(simu, localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' 
#' ## Usage with runSimulationBatch
#' ## Define two (or multiple) parameter sets
#' par1 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 0, environment = 0.5, specRate = 1) 
#' par2 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1)
#' 
#' ## Merge the parameter sets. It is important to note, that the funktion
#' ## needs a list of parameter sets.
#' par <- list(par1,par2)
#' 
#' ## Run the model
#' simubatch <- runSimulationBatch(par, parallel=T) 
#' 
#' ## Compare results to null model. Here it us mandatory to specify which Phylosim object 
#' ## should be used.
#' 
#' nullModelSample(simubatch[[1]],localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' nullModelSample(simubatch[[2]],localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' 
#' @export
nullModelSample <- function(simu, which.simulation = NULL, localPlotSize, numberOfPlots, repetitions){
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  simu <- simu[[which.simulation]]
  speciesMatrix <- simu$specMat
  phylogeny <- simu$phylogeny
  
  extantPhyloCophen <- stats::cophenetic(phylogeny) # create distance matrix from phylogeny (requires picante)
  comMat <- localPlots(size = localPlotSize, n = numberOfPlots, matrix = speciesMatrix, community = T)$communityTable # create community matrix from local communities (requires PhylGeo)
  
  metaabundance <- colSums(comMat) # calculate abundances of species in metacommunity
  # names(metaabundance) <- paste("s", names(metaabundance), sep="") # set names equal to those in the local communities and the distance matrix
  
  
  distribution <- rmultinom(n = repetitions, size=localPlotSize,  prob=metaabundance) # create multinomial distributed matrix
  
  
  NullDistribution = picante::mpd(samp = t(distribution), dis = extantPhyloCophen, abundance.weighted = T) # calculate mpd for null matrix
  observedMPD = picante::mpd(samp = comMat, dis = extantPhyloCophen, abundance.weighted = T) # calculate mpd for "real matrix
  
  observedMPD[which(is.na(observedMPD))] <- 0
  
  CummulativeDensityFunction = ecdf(NullDistribution) # create cumulative density function from null matrix
  
  
  pValues = CummulativeDensityFunction(observedMPD) # use  cumulative density function on "real" mpd
  
  return(pValues)
}