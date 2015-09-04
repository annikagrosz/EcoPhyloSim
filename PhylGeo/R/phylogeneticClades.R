#' @title Phylogenetic clades
#' @description Visualises the phylogenetic clades within a metacommunity from local community plots. 
#' @param phylo An object of class 'phylo'
#' @param community A community matrix
#' @param n The number of clades to visualise
#' @export
clades <- function(phylo, community, n){
  
  for( i in 1:n){
    x <- phylo$tip.label[is.element(phylo$tip.label,names(community[a,which(community[i,] >0)]))]
    y <- phylo$tip.label[!is.element(phylo$tip.label,names(community[a,which(community[i,] >0)]))]
    xy <- list(x,y)
    
    group <- ape::which.edge(phylo, x)
    
    cladecolor<-rep("darkgrey", dim(phylo$edge)[1])
    cladecolor[group]<-"black"
    
    cladewidth<-rep(1, dim(phylo$edge)[1])
    cladewidth[group]<-2
    
    title <- paste("Phylogenetic clade plots \n Clade", i)
    plot(phylo,  direction="downwards", show.tip.label=F, show.node.label=F, edge.color=cladecolor, edge.width=cladewidth, main = title)
    add.scale.bar()
  }
}