#' @title Phylogenetic clades
#' @description Visualises the phylogenetic clades of a local community within a metacommunity. 
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least the phylogeny ($phylogeny) and species matrix ($specMat). The species matrix is used as metacommunity
#' @param which.simulation defines which simulation run to choose in case you defined to save at multiple time steps. The default is the last one.
#' @param size, TODO
#' @param n The number of local communities. For each community a new plot is created. Needs to be greater than 1
#' @param subplot if no simu object is given, a communitytable of a local community can be given here
#' @param phylogeny if no simu object is ginve, an object of the class 'phylo' can be given here to describe the communites phylogeny
#' @details This function works with a single simu object (class "phylosim"). If no simu object is given, it is also possible to calculate the clades from a given phylogeny and localc plots. Here the phylogeny represents the metacommunity wihtin which the local plots create the clades
#' @examples  
#' 
#' #Load data
#' data(simu.neutral.global)
#' data(simu.neutral.local)
#' data(simu.envcom.local)
#' 
#'  
#' 
#' #Plot clades for neutral model with global dispersal with three subplots
#' par(mfrow=c(1,3))
#' plotclades(simu.neutral.global, size = 50, n = 3) 
#' 
#'  
#'
#' #Plot clades for neutral model with local dispersal with three subplots
#' par(mfrow=c(1,3))
#' plotclades(simu.neutral.local, size = 50, n = 3) 
#' 
#'  
#'
#'#Plot clades for environment and competition model with global dispersal with three subplots
#' par(mfrow=c(1,3))
#' plotclades(simu.envcom.local, size = 50, n = 3) 
#' 
#'  
#'
#'#increasing plot size
#'par(mfrow=c(2,2))
#'plotclades(simu.envcom.local, size = 50, n =2)
#'plotclades(simu.envcom.local, size = 1000, n =2)
#' @export
plotclades <- function(simu,which.simulation=NULL, size, n, subplot = NULL, phylogeny=NULL){
  
  
  if(is.null(subplot)){
  lp <- localPlots(simu = simu,size = size, n=n,community=T , which.simulation =which.simulation)
  community <- lp$communityTable} else community <- subplot
  
  if(is.null(phylogeny)){
    if (is.null(which.simulation)) which.simulation = length(simu$Output) 
    simu <- simu$Output[[which.simulation]]
  
  phylo <- simu$phylogeny} else phylo <- phylogeny
  
  
  for( i in 1:n){
    x <- phylo$tip.label[is.element(phylo$tip.label,names(community[which(community[i,] > 0)]))]
    y <- phylo$tip.label[!is.element(phylo$tip.label,names(community[which(community[i,] > 0)]))]
    xy <- list(x,y)
    
    group <- ape::which.edge(phylo, x)
    
    cladecolor<-rep("darkgrey", dim(phylo$edge)[1])
    cladecolor[group]<-"black"
    
    cladewidth<-rep(1, dim(phylo$edge)[1])
    cladewidth[group]<-2
    
    title <- paste("Phylogenetic Clade Plots \n Clade", i)
    plot(phylo,  direction="downwards", show.tip.label=F, show.node.label=F, edge.color=cladecolor, edge.width=cladewidth, main = title)
    
    place<- numeric(length(x))
    for(k in 1:length(x)){
      place[k] <- which(phylo$tip.label==x[k])
    }
    
    ape::tiplabels(text = x,tip = place, frame ="none", srt = 270, adj=0)
    ape::add.scale.bar()
  }
}