#' @title Null Model
#' @description Generates a null model and compares the observed results against.
#' @param simu an object of class "phylosim" as created by \code{\link{runSimulation}} or \code{\link{runSimulationBatch}}
#' @param which.simulation Integer, determines which result should be used. This argument is only usefull if interim steps are saved in the Phylosim object. By default (NULL), the end result is used.
#' @param abundance Logical, determining wheather the null model should be based on the abundance distribution of the sample plots. Default is FALSE.
#' @param localPlotSize number of grid cells of the plots drawn within the metacommunity
#' @param numberOfPlots number of plots drawn within the metacommunity
#' @param repetitions number of generated null model plots
#' @param fun String, phylogenetic measure that is used. Possible inputs are "mpd" (\code{\link[picante]{mpd}}) and "pd" (\code{\link[picante]{pd}}). Only used if abundance = FALSE.  
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
#' nullModel(simu, abundance= FALSE, localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' 
#' ## Usage with runSimulationBatch
#' ## Define two (or multiple) parameter sets
#' par1 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1) 
#' par2 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 2, environment = 0.5, specRate = 1)
#' 
#' ## Merge the parameter sets. It is important to note, that the funktion
#' ## needs a list of parameter sets.
#' par <- list(par1,par2)
#' 
#' ## Run the model
#' simubatch <- runSimulationBatch(par, parallel=2) 
#' 
#' ## Compare results to null model
#' nullModel(simubatch[[1]],localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' nullModel(simubatch[[2]],localPlotSize = 100, numberOfPlots = 10, repetitions = 10)
#' 
#' @export
nullModel <- function(simu, which.simulation=NULL, abundance = FALSE, localPlotSize, numberOfPlots, repetitions, fun="mpd"){
  
  comMat <- localPlots(simu=simu, which.simulation=which.simulation, size = localPlotSize, n = numberOfPlots, community = T)$communityTable # create community matrix from local communities (requires PhyloSim)
  
  
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  simu <- simu$Output[[which.simulation]]
  phylogeny <- simu$phylogeny
  speciesMatrix <- simu$specMat
  
  extantPhyloCophen <- stats::cophenetic(phylogeny) # create distance matrix from phylogeny (requires picante)
  
  # calculate fun for "real matrix"
  ifelse (fun == "mpd", observedMPD <- picante::mpd(samp = comMat, dis = extantPhyloCophen, abundance.weighted = T),
          ifelse(fun == "pd", observedMPD <- picante::pd(samp = comMat, phylogeny)$PD, stop("wrong dist argument")))
  
  
  # calculate abundances of species in metacommunity
  
  ifelse(abundance==FALSE, metaabundance <- table(speciesMatrix),
         ifelse(abundance==TRUE, metaabundance <- colSums(comMat), stop("wrong abundance argument") ))
  
  
  if(abundance==FALSE) names(metaabundance) <- paste("s", names(metaabundance), sep="") # set names equal to those in the local communities and the distance matrix
  
  
  distribution <- rmultinom(n = repetitions, size=localPlotSize,  prob=metaabundance) # create multinomial distributed matrix
  
  if(fun == "mpd"){
  NullDistribution <- picante::mpd(samp = t(distribution), dis = extantPhyloCophen, abundance.weighted = T) # calculate mpd for null matrix
  }
  else{
   NullDistribution<- picante::pd(samp=t(distribution), tree= phylogeny)$PD
  }
   
  observedMPD[which(is.na(observedMPD))] <- 0
  
  CummulativeDensityFunction <- ecdf(NullDistribution) # create cumulative density function from null matrix
  
  
  pValues <- CummulativeDensityFunction(observedMPD) # use  cumulative density function on "real" mpd
  
  return(pValues)
}

