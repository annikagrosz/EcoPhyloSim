#' @title Species richness
#' @description Calculate the total species richness of a community
#' @param matrix A square matrix containing a species community (one individual per grid cell) 
#' @return An integer value for the species richness
specRich <- function(matrix){  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}