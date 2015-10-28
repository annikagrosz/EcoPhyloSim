#' @title Phylogeny Reconstruction
#' @description reconstructs a phylogeny from a neutral trait matrix as returned by \code{\link{fullMod}}
#' @param speciesMatrix A square matrix containing a species community (one individual per grid cell)
#' @param traitMatrix A square matrix containing the neutral trait of a species community (one trait value per individual per grid cell
#' @param ...  Additional arguments to be passed to function \code{\link{hclust}}
#' @return An object of class 'phylo'
#' @export
phyloReconstruct <- function(speciesMatrix, traitMatrix, ...)
{
  
  speciesTable <- table(speciesMatrix)  
  meanNeutralTraits <-numeric()  
  for(i in 1:length(speciesTable))
  {
    meanNeutralTraits[i] <- mean(traitMatrix[which(speciesMatrix == names(speciesTable)[i])])
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