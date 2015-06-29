#' @title Null Model
#' @description Generates a null model to compare the observed results against.
#' @param speciesMatrix A square matrix containing the metacommunity (one individual per grid cell) 
#' @param phylogeny an object of class "phylo", representing the metacommunity
#' @param localPlotSize number of grid cells of the plots drawn within the metacommunity
#' @param numberOfPlots number of plots drawn within the metacommunity
#' @param repetitions number of generated null model plots
#' @return A numeric vector with pValues for each plot in the observed metacommunity
nullModel <- function(speciesMatrix, phylogeny, localPlotSize, numberOfPlots, repetitions){
  
  extantPhyloCophen <- stats::cophenetic(phylogeny) # create distance matrix from phylogeny (requires picante)
  comMat <- localPlots(size = localPlotSize, n = numberOfPlots, matrix = speciesMatrix, community = T)$communityTable # create community matrix from local communities (requires PhylGeo)
  
  metaabundance <- table(speciesMatrix) # calculate abundances of species in metacommunity
  names(metaabundance) <- paste("s", names(metaabundance), sep="") # set names equal to those in the local communities and the distance matrix
  
  
  distribution <- rmultinom(n = repetitions, size=localPlotSize,  prob=metaabundance) # create multinomial distributed matrix
  
  
  NullDistribution = mpd(samp = t(distribution), dis = extantPhyloCophen, abundance.weighted = T) # calculate mpd for null matrix
  observedMPD = mpd(samp = comMat, dis = extantPhyloCophen, abundance.weighted = T) # calculate mpd for "real matrix
  
  observedMPD[which(is.na(observedMPD))] <- 0
  
  CummulativeDensityFunction = ecdf(NullDistribution) # create cumulative density function from null matrix
  
  
  pValues = CummulativeDensityFunction(observedMPD) # use  cumulative density function on "real" mpd
  
  return(pValues)
}