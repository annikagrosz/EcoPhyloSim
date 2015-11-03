#' @title Phylogeny Reconstruction
#' @description reconstructs a phylogeny from a neutral trait matrix as returned by \code{\link{runSimulation}}
#' @param simu A object of class Phylosim
#' @param which.simulation Integer, determines which simulation should be used. Only useful if your Phylosim object contains more than one result. By default the last result is chosen.
#' @param ...  Additional arguments to be passed to function \code{\link{hclust}}
#' @return An object of class 'phylo'
#' @examples  
#' ## Run a simulation
#' par <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = 500,
#'         density = 1, environment = 0.5, specRate = 1)
#'
#' simu <- runSimulation(par)
#' 
#' ## Reconstruct phylogeny
#' rePhyl <- phyloReconstruct(simu)
#' 
#' ## Compare to the real phylogeny
#' par(mfrow=c(1,2))
#' plot(simu$Output[[1]]$phylogeny)
#' plot(rePhyl)
#' @export
#' 
phyloReconstruct <- function(simu, which.simulation = NULL, ...)
{
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  simu <- simu$Output[[which.simulation]]
  
  speciesTable <- table(simu$specMat)
  traitMatrix <- simu$neutMat
  
  meanNeutralTraits <-numeric()  
  for(i in 1:length(speciesTable))
  {
    meanNeutralTraits[i] <- mean(traitMatrix[which(simu$specMat == names(speciesTable)[i])])
  }
  names(meanNeutralTraits) = names(speciesTable)
 
  distNeutral <- data.frame(x = names(speciesTable))
  columnX <- numeric()
  for(i in 1:length(meanNeutralTraits))
  {
    for(j in 1: length(meanNeutralTraits))
    {
      columnX[j] <- abs(meanNeutralTraits[i] - meanNeutralTraits[j] )
    }
    distNeutral <- cbind(distNeutral, columnX)
  }
  distNeutral <- distNeutral[,-1]
  colnames(distNeutral) <- paste("s",names(meanNeutralTraits),  sep = "")
  rownames(distNeutral) <- paste("s",names(meanNeutralTraits),  sep = "")
  
  distMat <- dist(distNeutral, diag = TRUE)

  phyloclust <- ape::as.phylo(stats::hclust(distMat, method = ...))
  return(phyloclust)
}