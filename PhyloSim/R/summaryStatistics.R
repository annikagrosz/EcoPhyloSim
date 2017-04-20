
#' @title Summary Statistics
#' @description Calculates the summary satistics for a simulation.
#' @param simulation Simulation output of the class "Phylosim"
#' @details TODO: implement
#' @return A list containing the summary statistics.
#' @examples 
#' 
#' simu <- runSimulation(createCompletePar(x=50, y=50, runs=1000, specRate=2))
#' summStat <- calculateSummaryStatistics(simu)
#' 
#' 
#' @export
#' 

calculateSummaryStatistics <- function(simulation) {
  summaryStatistics <- list(rac=NA,
                            sac=NA,
                            alphaDiversity = NA,
                            betaDiversity = NA,
                            imbalance = NA,
                            dispersion = NA)
  
  # TODO: implement special case: only 1 species
  if (is.double(simulation$Output[[1]]$phylogeny)) {
    return(summaryStatistics)
  }
  
  # RAC
  summaryStatistics$rac <- rac(simulation, plot = F)
  
  # SAC
  summaryStatistics$sac <- sac(simulation, size = seq(1,15), plot = F)
  
  
  # alpha and beta diversity
  
  # Draw random plots
  # TODO: let the user chose the plot size and number of plots
  nSubPlots <- 20
  sizeSubPlots <- 10
  
  plots <- localPlots(simulation,
                      n = nSubPlots,
                      size = sizeSubPlots,
                      plot = F,
                      community = T)
  
  # alpha diversity
  # in this case alpha diversity = mean number of species per subplot
  summaryStatistics$alphaDiversity <- mean(rowSums(plots$communityTable>0))
  
  # beta diversity
  # uses vegan's beta diversity function
  # see betadiver(help=TRUE)
  # TODO: let the user chose the method
  method = 1
  summaryStatistics$betaDiversity <- mean(vegan::betadiver(plots$communityTable,
                                                           method=method))
  
  
  # phylogenetic imbalance
  # uses apTreeshape's colless function
  
  # phylo object must be converted to treeshape object
  tsPhylogeny <- apTreeshape::as.treeshape.phylo(ape::drop.fossil(simulation$Output[[1]]$phylogeny))
  
  # apTreeshape::colless only works on trees with more than 2 tips
  if(nrow(tsPhylogeny$merge) != 1) {
    # more than 2 tips
    # normalization of the colless index
    # TODO: let the user chose the normalization
    normalization = "yule"
    #normalization = "pda"
    #normalization = NULL
    
    summaryStatistics$imbalance <- apTreeshape::colless(tsPhylogeny,
                                                        norm = normalization)
  } else {
    # less than 2 tips
    # WARNING
    # This does not seem to make sense
    
    # copied from apTreeshape::colless
    # necessary, because apTreeshape does not implement colless for trees
    # with only two tips
    norm.yule <- function(ICN, tree) {
      leaf_nb <- nrow(tree$merge) + 1
      EICN <- leaf_nb * log(leaf_nb) + (0.57721566 - 1 - log(2)) * 
        leaf_nb
      IC <- (ICN - EICN)/(leaf_nb)
      IC
    }
    
    # TODO: implement pda and NULL normalization
    summaryStatistics$imbalance <- norm.yule(0, tsPhylogeny)
  }
  
  # phylogenetic dispersion
  # uses PhyloMeasures' implementation of the NRI
  
  # Causes error. Drop fossil seems to drop clades that are no real fossils?!
  #summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(ape::drop.fossil(simulation$Output[[1]]$phylogeny),
  #                                                              plots$communityTable,
  #                                                              standardize = TRUE))
  summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(simulation$Output[[1]]$phylogeny,
                                                                plots$communityTable,
                                                                standardize = TRUE))
  
  return(summaryStatistics)
}
