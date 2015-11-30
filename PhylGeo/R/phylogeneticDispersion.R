#' @title Null Model Batch Extension
#' @description Generates a null models for a list of Phylosim objects as created by \code{\link{runSimulationBatch}} and compares the observed results against. 
#' @param simu A list of Phylosim objects
#' @param plotlengths Integer or vectr of integers, size(s) of the sample plots used in the null models.
#' @param plots Integer, number of sample plots
#' @param replicates Integer, number of replicate
#' @param types String, determines which null model should be created. Possible inputs are "PhylMeta" (default), "PhylSample", "PhylPool", "SamplePool" (see details). 
#' @param which.simulation Integer or vector of integers, determines which result should be used. This argument is only usefull if interim steps are saved in the Phylosim object. You can choose a particular time step, a vector of time steps, or use all timesteps with which.simulation = NULL (default).
#' @param fun String, determines which function should be used to calculate the phylogenetic diversity in the sample plot. Possible inputs are "mpd" or "pd" (see details).
#' @details In the types argument "PhylMeta" is equivalent to \code{\link{nullModel}} with abundance = FALSE, "PhylSample" is equivalent to the same function with abundance = TRUE, wheras the two other option use \code{\link[picante]{ses.mpd}}. "PhylPool" uses the argument null.model = "phylogeny.pool", "SamplePool" is setting null.model to "sample.pool". \cr\cr The fun argument is only used if types="PhylSample". For more information see \code{\link{nullModel}}. \cr \cr If yu choose which.simulation = NULL (default), the length of the output corresponds to the length of the 'runs' argument in your parameter definition. Otherwise the length of the list corresponds to the length of your which.simulation argument.
#' @return A list with pValues for each plot in the observed metacommunity. If you have calculated multiple scenarios, the results can be visualized by \code{\link{plotPhylogeneticDisperion}}.
#' @examples 
#' ## Create a set of parameters
#' par1 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 0, environment = 0.5, specRate = 1) 
#' par2 <- createCompletePar(x = 50, y = 50, dispersal = 0 , runs = c(500,1000),
#'         density = 1, environment = 0.5, specRate = 1)
#' 
#' ## Merge the parameter sets. 
#' par <- list(par1,par2)
#' 
#' ## Run the model
#' simu <- runSimulationBatch(par, parallel = "auto") 
#' 
#' ## Calculate null model and compare the observed results against
#' pValues <- calculatePhylogeneticDispersion(simu, plotlength=20, plots=20, replicates=20, types="PhylMeta")
#' 
#' 
#' @export

# Version that accepts multiple types
calculatePhylogeneticDispersion <- function(simu, plotlengths = 10,  plots = 200, replicates = 500, type = "PhylMeta",  which.simulation = NULL, reduce= FALSE, fun="mpd"){
  
  if ("PhyloSim" %in% class(simu) ==T) simu = list(simu)
  
  ## TODO implement function for only 1 scenario
  
  nScenarios = length(simu) 
  nRuns = length(simu[[1]]$Output)
  parIndex = simu[[1]] 
  times <- which.simulation
  if(is.null(times)){
    times = c(1:nRuns)
  }
  # type / scenario / plotlength / repetition
  
  pValues = list()
  
  
  
  for(i in times){
    pValues[[i]] = list()
    timeindex = i
    
    for(k in 1:nScenarios){
      pValues[[i]][[k]] = list()
      
      
      phyl <- simu[[k]]$Output[[timeindex]]$phylogeny
      extantPhylo <- ape::drop.fossil(phy = phyl)
      extantPhyloDis <- cophenetic(extantPhylo)
      
      pval = list()
      for (j in 1:length(plotlengths)){
        
        plotsize = plotlengths[j]^2
        
        
        
        if (type == "PhylMeta"){
          pval[[j]] <- nullModel(simu[[k]], localPlotSize = plotsize, numberOfPlots = plots, which.simulation = timeindex, repetitions = replicates, fun = "mpd")
        }else if (type == "PhylSample"){
          pval[[j]] <- nullModel(simu[[k]], localPlotSize = plotsize, abundance = TRUE, numberOfPlots = plots, which.simulation = timeindex, repetitions = replicates)    
        }else if (type == "PhylPool"){
          comMat <- localPlots(simu[[k]],size = plotsize, n = plots, which.simulation = timeindex, community = T)$communityTable
          pval[[j]] <- picante::ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "phylogeny.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p      
        }else if (type == "SamplePool"){
          comMat <- localPlots(simu[[k]], size = plotsize, n = plots, which.simulation = timeindex, community = T)$communityTable
          pval[[j]] <- picante::ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "sample.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p
        }
        
      }
      names(pval)<-plotlengths
      
      pValues[[i]][[k]] = pval
      
      names(pValues[[i]][[k]]) = c(1: length(pValues[[i]][[k]]))
    }
    
  }
  
  return(pValues)
}




