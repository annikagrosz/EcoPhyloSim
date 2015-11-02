#' @title Get Average Traits
#' @description calculates the arithmetic mean of the trait values for each species. 
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the spatial species matrix ($specMat) the environment matrix ($envMat) and the three different trait matrizes ($traitMat,$compMat, $neutMat)    
#' @param which.simulation defines which simulation run to choose in case you defined to save at multiple time steps. The default is the last one.
#' @param orderPhylo defines if the mean traits shall be calculated for extinct species as well. default is yes (TRUE) 
#' @import 
#' @export

getAverageTraits <- function(simu,which.simulation =NULL,orderPhylo = T){
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  simu <- simu[[which.simulation]]
  
  if(orderPhylo == T){
    extantPhylogeny <- drop.fossil(simu$phylogeny)
    species =  as.numeric(sapply(list(extantPhylogeny$tip.label), substring, 2))    
  }else{
    species = unique(simu$specMat)
  }
  
  traits <- matrix(nrow = length(species), ncol = 4)
  
  rownames(traits) = species
  colnames(traits) = c("environment", "env trait", "comp trait", "neutral trait")
  
  for (i in 1:length(species)){
    sel = simu$specMat == species[i]
    traits[i,1] = mean(simu$envMat[sel])
    traits[i,2] = mean(simu$traitMat[sel])
    traits[i,3] = mean(simu$compMat[sel])
    traits[i,4] = mean(simu$neutMat[sel])

  }
  return(traits)
}


