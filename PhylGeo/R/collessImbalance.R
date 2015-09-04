#' @title Colless' imbalance
#' @description Calculates the Colless' imbalance for a Phylogeny.
#' @param phylo An object of class 'phylo'
#' @return A numeric value for the Colless' Imbalance 
#' @export
collessImbalance <- function(phylo){ #colless imbalace function
  balance <- ape::balance(phylo)
  sum.diff <- 0
  for(i in 1:length(balance[,1])){
    diff <- abs(balance[i,1] - balance[i,2])
    sum.diff <- sum.diff + diff 
  }
  c.imbal <- 2*sum.diff/((sum(balance[1,])-1)*(sum(balance[1,])-2))
  return(as.numeric(c.imbal))
}