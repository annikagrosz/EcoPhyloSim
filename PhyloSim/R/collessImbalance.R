#' @title Colless' imbalance
#' @description Calculates the Colless' imbalance for a Phylogeny.
#' @param simu An object of type "PhyloSim"
#' @param which.simulation Integer, determines which simulation should be used. Only useful if your Phylosim object contains more than one result. By default the last result is chosen.
#' @return A numeric value for the Colless' Imbalance 
#' @export

collessImbalance <- function(simu, which.simulation = NULL){ 
  if (is.null(which.simulation)) which.simulation = length(simu$Output) 
  phylo <- simu$Output[[which.simulation]]$phylogeny
  
  # Check if Colless can be calculated
  len_tip <- length(phylo$tip.label)
  Nodes <- phylo$Nnode
  if (Nodes != (len_tip - 1)){
    c.imbal<-NA
  }else{ 
  # Calculate Colless  
  balance <- try(ape::balance(phylo))
    sum.diff <- 0
    for(i in 1:length(balance[,1])){
      diff <- abs(balance[i,1] - balance[i,2])
      sum.diff <- sum.diff + diff 
    }
    c.imbal <- 2*sum.diff/((sum(balance[1,])-1)*(sum(balance[1,])-2))
  }
  return(as.numeric(c.imbal))
}