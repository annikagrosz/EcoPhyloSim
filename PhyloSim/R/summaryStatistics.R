
#' @title Summary Statistics
#' @description Calculates the summary satistics for a simulation of class PhyloSim.
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
  summaryStatistics <- list(racAuc=NA,
                            sacAuc=NA,
                            alphaDiversity = NA,
                            betaDiversity = NA,
                            gammaDiversity = NA,
                            imbalance = NA,
                            dispersion = NA,
                            gammaStatistics = NA,
                            meanNodeAge = NA)
  
  # TODO: implement special case: only 1 species
  if (is.double(simulation$Output[[1]]$phylogeny)) {
    return(summaryStatistics)
  }
  
  # RAC
  rac <- rac(simulation, plot = F)
  summaryStatistics$racAuc <- MESS::auc(x = rac$Rank,
                                        y = rac$Abundance)
  
  # SAC
  sac <- sac(simulation, size = seq(1,15), plot = F)
  summaryStatistics$sacAuc <- MESS::auc(x = sac$size,
                                        y = sac$sr.Mean)
  
  
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
  
  # gamma diversity
  summaryStatistics$gammaDiversity <- gammaDiversity(simulation)
  
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
  
  # Commented code below causes error. Drop fossil seems to drop clades that are no real fossils?!
  #summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(ape::drop.fossil(simulation$Output[[1]]$phylogeny),
  #                                                              plots$communityTable,
  #                                                              standardize = TRUE))
  
  summaryStatistics$dispersion <- mean(PhyloMeasures::mpd.query(simulation$Output[[1]]$phylogeny,
                                                                plots$communityTable,
                                                                standardize = TRUE))
  
  # gamma statistics
  # uses ape's implementation of the gammaStatistics
  # attention: only works on ultrametric trees
  summaryStatistics$gammaStatistics <- ape::gammaStat(ape::drop.fossils(simulation$Output[[1]]$phylogeny))
  
  # mean node age
  #summaryStatistics$meanNodeAge <- ape::chronoMPL(simulation$Output[[1]]$phylogeny)
  summaryStatistics$meanNodeAge <- mean(ape::drop.fossils(simulation$Output[[1]]$phylogeny)$edge.length)
  
  return(summaryStatistics)
}

gammaDiversity <- function(simulation, q=0){
  spec_vector <- c(simulation$Output[[1]]$specMat)
  specs <- unique(spec_vector)
  n_specs <- length(specs)
  
  if (q == 0) return(n_specs)
  
  n_individuals <- length(spec_vector)
  
  prop_abundances <- rep(NA, length(specs))
  
  for(i in 1:length(specs)){
    prop_abundances[i] <- sum(spec_vector == specs[i]) / n_individuals
  }
  
  gamma <- prop_abundances * prop_abundances^(q-1)
  gamma <- sum(gamma)^(1/(q-1))
  gamma <- 1/gamma
  return(gamma)
}