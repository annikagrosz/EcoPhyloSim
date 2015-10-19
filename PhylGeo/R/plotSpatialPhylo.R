#' @title Plot of phylogeny and species landscape
#' @description Plots the phylogeny and species landscape, colored by taxon 
#' @param simu simulation output, consisting of a list contain at least $specMat with the spatial species matrix, and $phylogeny with the phylogeny
#' @param phylogeny the corresponding (extant) phylogeny
#' @param plot defines what to plot. "both" plots the landscape and phylogeny side-by-side, other options are "landscape" and "phylogeny"
#' @param plotTraits defines if the traits should be plotted
#' @param time defines which simulation run to choose in case you defined to save at multiple time steps. The default is the last one.
#' @param col defines the color, options are "phylodist" (default) and "equidist" 
#' @param main defines the title of the plot
#' @import ape
#' @importFrom adephylo distTips
#' @export
plotSpatialPhylo <- function (simu, plot = "both", plotTraits = T, col = "phylodist", main = "", time = NULL){
  
  if (is.null(time)) time = length(simu) - 1
  
  
  phylogeny = simu[[time]]$phylogeny
  
  extantPhylogeny <- drop.fossil(phylogeny)
  
  nSpecies = length(extantPhylogeny$tip.label)
  
  landscape = simu[[time]]$specMat
  
  if (plotTraits == T){
    #library(shape)
    traits = getAverageTraits(simu[[time]])  
    #rootDist <- distRoot(extantPhylogeny, 1)
  }
  
  tipDistances <- cumsum(c(1, diag(as.matrix(distTips(extantPhylogeny))[,-1])))
  
  if (col == "phylodist") cols<-rainbow(max(tipDistances), start = 0.19)[tipDistances]
  if (col == "equidist")  cols<-rainbow(nSpecies, start = 0.19)
  
  
  tipNumbers <- as.numeric(sapply(list(extantPhylogeny$tip.label), substring, 2))
  
  
  landscape <- matrix(as.numeric(factor(landscape, levels = tipNumbers, labels = 1:nSpecies)), nrow = nrow(landscape))
  
  if(plot!="both"){
    if(plotTraits==T){
      layout(mat = matrix(1:4, nrow = 2, byrow = T), widths = c(0.5,1), heights = c(0.15,0.8))
      par(mar = c(0,0,0,0), oma = c(2,2,2,2))
      plot(NULL,frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7), ylim = c(0,3))
      text(1:4 - 0.5, 0, colnames(traits), srt = 45, pos = 4)
      plot.new()
      size = dev.size()[2]/3
      plot(rep(1:4, each = nSpecies), rep(1:nSpecies, 4), cex = size*traits, pch = 16, frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7))
    }
    
    if(plotTraits==F){
      par(mfrow=c(1,1),mar = c(1,1,1,1))
    }
    if(plot=="landscape"){
      image(landscape, col = cols, yaxt='n', xaxt='n')
    }
    else{
      if(plot=="phylogeny"){
        plot(extantPhylogeny, tip.color = cols, cex = 1.3)
      }
      else stop("wrong plot argument")
    }
  }
  else{
    if(plotTraits==T){
      layout(mat = matrix(1:6, nrow = 2, byrow = T), widths = c(1,0.5,1), heights = c(0.15,0.8))
      par(mar = c(0,0,0,0), oma = c(2,2,2,2))
      plot.new()
      plot(NULL,frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7), ylim = c(0,3))
      text(1:4 - 0.5, 0, colnames(traits), srt = 45, pos = 4)
      plot.new()
      plot(extantPhylogeny, tip.color = cols, cex = 1.3)
      size = dev.size()[2]/3
      plot(rep(1:4, each = nSpecies), rep(1:nSpecies, 4), cex = size*traits, pch = 16, frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7))
      par(mar = c(1,1,1,1))
      image(landscape, col = cols, yaxt='n', xaxt='n')
    }
    else{
      par(mfrow=c(1,2))
      plot(extantPhylogeny, tip.color = cols, cex = 1.3)
      image(landscape, col = cols, yaxt='n', xaxt='n')
    }
  }
  mtext(main, outer = T)
  
}

