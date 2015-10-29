#' @title Phylogenetic clades
#' @description Visualises the phylogenetic clades within a metacommunity from local community plots. 
#' @param phylo An object of class 'phylo'
#' @param community A community matrix
#' @param n The number of clades to visualise
#' @export
clades <- function(simu,which.simulation=NULL, size, n){
  
  lp <- localPlots(simu = simu,size = size, n=n,community=T , which.simulation =which.simulation)
  
  if (is.null(which.simulation)) which.simulation = length(simu) - 1
  simu <- simu[[which.simulation]]
  
  phylo <- simu$phylogeny
  community <- lp$communityTable
  
  for( i in 1:n){
    x <- phylo$tip.label[is.element(phylo$tip.label,names(community[which(community[i,] > 0)]))]
    y <- phylo$tip.label[!is.element(phylo$tip.label,names(community[which(community[i,] > 0)]))]
    xy <- list(x,y)
    
    group <- ape::which.edge(phylo, x)
    
    cladecolor<-rep("darkgrey", dim(phylo$edge)[1])
    cladecolor[group]<-"black"
    
    cladewidth<-rep(1, dim(phylo$edge)[1])
    cladewidth[group]<-2
    
    title <- paste("Phylogenetic clade plots \n Clade", i)
    plot(phylo,  direction="downwards", show.tip.label=F, show.node.label=F, edge.color=cladecolor, edge.width=cladewidth, main = title)
    
    for(k in 1:length(x)){
      place[k] <- which(phylo$tip.label==x[k])
    }
    
    ape::tiplabels(text = x,tip = place, frame ="none", srt = 320, adj=0)
    ape::add.scale.bar()
  }
}