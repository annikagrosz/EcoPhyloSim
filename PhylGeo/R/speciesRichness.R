#' @title Species richness
#' @description Calculate the total species richness of a community
#' @param simu Object of class "Phylosim", usually consisting out of several lists. Needs at least the spatial distribution of the species stored in a matrix ($specMat)
#' @param which.simulation Integer, determines which simulation should be used. Only useful if your Phylosim object contains more than one result.
#' @return An integer value for the species richness
#' @export

specRich <-function(simu,which.simulation=NULL){
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  simu <- simu[[which.simulation]]
  matrix <- simu$specMat
  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}

