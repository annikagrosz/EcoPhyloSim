#' @title Null Model Batch Extension
#' @description Generates a null models for a list of Phylosim objects as created by \code{\link{runSimulationBatch}} and compares the observed results against.
#' @param simu A list of Phylosim objects
#' @param plotlengths Integer, size of the sample plots
#' @param plots Integer, number of sample plots
#' @param replicates Integer, number of replicate
#' @param types String, determines which null model should be created. Possible inputs are "PhylMeta" (default), "PhylSample", "PhylPool", "SamplePool" (see details). 
#' @param which.simulation Integer, determines which result should be used. This argument is only usefull if interim steps are saved in the Phylosim object. By default (NULL), the end result is used.
#' @param fun String, determines which function should be used to calculate the phylogenetic diversity in the sample plot. Possible inputs are "mpd" or "pd" (see details).
#' @details In the types argument "PhylMeta" is equivalent to \code{\link{nullModel}} with abundance = FALSE, "PhylSample" is equivalent to the same function with abundance = TRUE, wheras the two other option use \code{\link[picante]{ses.mpd}}. "PhylPool" uses the argument null.model = "phylogeny.pool", "SamplePool" is setting null.model to "sample.pool". \cr\cr The fun argument is only used if types="PhylSample". For more information see \code{\link{nullModel}}. 
#' @return A numeric vector with pValues for each plot in the observed metacommunity
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
calculatePhylogeneticDispersion <- function(simu, plotlengths = 10,  plots = 200, replicates = 500, types = "PhylMeta",  which.simulation = NULL, reduce= FALSE, fun="mpd"){
  
  if ("Phylosim" %in% simu ==T) simu = list(simu)
  
  ## TODO implement function for only 1 scenario
  
  nScenarios = length(simu) 
  nRuns = length(simu[[1]]$Output)
  parIndex = simu[[1]] 
  times <- which.simulation
  if (is.null(times)) times = nRuns
  
  # type / scenario / plotlength / repetition
  
  pValues = list()
  
  for (i in 1:length(types)){
    pValues[[i]] = list()
    
    type = types[i]
    
    for(k in 1:nScenarios){
      pValues[[i]][[k]] = list()
      
      for (j in 1:length(plotlengths)){
        
        plotsize = plotlengths[j]^2
        pval = list()
        
        for (l in 1:length(times)){
          
          timeindex = times[i]
          
          s <- simu[[k]]$Output[[timeindex]]$specMat
          phyl <- simu[[k]]$Output[[timeindex]]$phylogeny
          extantPhylo <- drop.fossil(phy = phyl)
          extantPhyloDis <- cophenetic(extantPhylo)
          
          if (type == "PhylMeta"){
            pval[[l]] <- nullModel(simu[[k]], localPlotSize = plotsize, numberOfPlots = plots, repetitions = replicates, fun = "mpd")
          }else if (type == "PhylSample"){
            pval[[l]] <- nullModel(simu[[k]], localPlotSize = plotsize, abundance = TRUE, numberOfPlots = plots, repetitions = replicates)    
          }else if (type == "PhylPool"){
            comMat <- localPlots(simu[[k]],size = plotsize, n = plots, community = T)$communityTable
            pval[[l]] <- picante::ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "phylogeny.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p      
          }else if (type == "SamplePool"){
            comMat <- localPlots(simu[[k]], size = plotsize, n = plots, community = T)$communityTable
            pval[[l]] <- picante::ses.mpd(samp = comMat, dis = extantPhyloDis, null.model = "sample.pool", abundance.weighted = TRUE, runs = replicates)$mpd.obs.p
            
          }
        }
        names(pval) = times
        
        if(reduce == T & length(times) == 1) pval = pval[[1]]
        
        pValues[[i]][[k]][[j]] = pval
      }
      names(pValues[[i]][[k]]) = plotlengths
    }
    names(pValues[[i]]) = names(simu)
  }
  names(pValues) = types
  if (reduce == T){
    #     if (length(plotlengths) == 1){
    #       for (i in 1:length(types)){
    #         pValues[[i]]= pValues[[i]][[1]]
    #       }
    #     }
    if (length(types) == 1) pValues = pValues[[1]]       
  }
  return(pValues)
}



