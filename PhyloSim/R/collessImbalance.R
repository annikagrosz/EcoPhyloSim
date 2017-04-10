#' @title Colless' imbalance
#' @description Calculates the Colless' imbalance for a Phylogeny.
#' @param simu An object of type "PhyloSim"
#' @param which.result Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.
#' @param useApTreeshape Boolean, if TRUE uses apTreeshape's colless function
#' @param norm Normalization: Either NULL, "yule" or "pda"
#' @param dropFossil Boolean, if TRUE applies ape's drop.fossil on the phylogeny
#' @return A numeric value for the Colless' Imbalance 
#' @details If useApTreeshape is set to TRUE the phylogeny of simu must be bifurcate.\cr\cr If dropFossils==TRUE only extant species are included in the phylogeny
#' @references Colless, D. H. "Review of phylogenetics: the theory and practice of phylogenetic systematics." Syst. Zool 31 (1982): 100-104.
#' @export

collessImbalance <- function(simu, which.result = NULL, useApTreeshape = TRUE, norm = NULL,dropFossils = FALSE){ 
  if (is.null(which.result)) which.result = length(simu$Output) 
  phylo <- simu$Output[[which.result]]$phylogeny
  
  if (dropFossils == TRUE) {
    phylo <- ape::drop.fossil(phylo)
  }
  
  if (useApTreeshape == TRUE) {
    # phylo must be converted to treeshape for apTreeshape
    treeShapePhylo <- apTreeshape::as.treeshape.phylo(phylo)
    return(apTreeshape::colless(treeShapePhylo, norm = norm))
  } else {
    # Calculate Colless  
    balance <- balancefun(phylo)
    sum.diff <- 0
    for(i in 1:length(balance[,1])){
      diff <- abs(balance[i,1] - balance[i,2])
      sum.diff <- sum.diff + diff 
    }
    c.imbal <- 2*sum.diff/((sum(balance[1,])-1)*(sum(balance[1,])-2))
    return(as.numeric(c.imbal))
  }
}


#### The following function is largely ####
## copied from the 'ape' package Version 3.4 ##
#  Paradis E., Claude J. & Strimmer K. 2004. 
#    APE: analyses of phylogenetics and evolution in R
#    language. Bioinformatics 20: 289-290
balancefun<-function(phy){
  if (!inherits(phy, "phylo")) 
    stop("object \"phy\" is not of class \"phylo\"")
  phy <- reorder(phy)
  N <- length(phy$tip.label)
  nb.node <- phy$Nnode
  
  ans <- matrix(NA, nb.node, 2)
  foo <- function(node, n) {
    s <- which(phy$edge[, 1] == node)
    desc <- phy$edge[s, 2]
    ans[node - N, 1] <<- n1 <- (s[2] - s[1] + 1)/2
    ans[node - N, 2] <<- n2 <- n - n1
    if (desc[1] > N) 
      foo(desc[1], n1)
    if (desc[2] > N) 
      foo(desc[2], n2)
  }
  foo(N + 1, N)
  rownames(ans) <- if (is.null(phy$node.label)) 
    N + 1:nb.node
  else phy$node.label
  ans
}