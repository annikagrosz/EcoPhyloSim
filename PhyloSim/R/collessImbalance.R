#' @title Colless' imbalance
#' @description Calculates the Colless' imbalance for a Phylogeny.
#' @param simu An object of type "PhyloSim"
#' @param which.result Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.
#' @return A numeric value for the Colless' Imbalance 
#' @references Colless, D. H. "Review of phylogenetics: the theory and practice of phylogenetic systematics." Syst. Zool 31 (1982): 100-104.
#' @export

collessImbalance <- function(simu, which.result = NULL){ 
  if (is.null(which.result)) which.result = length(simu$Output) 
  phylo <- simu$Output[[which.result]]$phylogeny
  
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
  if(is.element(NA, c.imbal)) warning("NA values were produced where phylogeny is not rooted and fully dichotomous")
  return(as.numeric(c.imbal))
}