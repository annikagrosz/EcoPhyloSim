#' @title Plot of Phylogeny and species Landscape
#' @description Plots the phylogeny and species landscape, colored by taxon 
#' @param simu simulation output, consisting of a list contain at least $specMat with the spatial species matrix, and $phylogeny with the phylogeny
#' @param phylogeny the corresponding (extant) phylogeny
#' @param plot defines what to plot. "both" plots the landscape and phylogeny side-by-side
plotSpatialPhylo <- function (simu, plot = "both", plotTraits = T, col = "phylodist"){
  
  phylogeny = simu$phylogeny
  
  extantPhylogeny <- drop.fossil(phylogeny)
  
  nSpecies = length(extantPhylogeny$tip.label)
  
  landscape = simu$specMat
  
  if (plotTraits == T){
    #library(adephylo)
    #library(shape)
    traits = getAverageTraits(simu)  
    #rootDist <- distRoot(extantPhylogeny, 1)
  }
  
  tipDistances <- cumsum(c(1, diag(as.matrix(distTips(extantPhylogeny))[,-1])))
  
  if (col == "phylodist") cols<-rainbow(max(tipDistances), start = 0.19)[tipDistances]
  if (col == "equidist")  cols<-rainbow(nSpecies, start = 0.19)

  
  tipNumbers <- as.numeric(sapply(list(extantPhylogeny$tip.label), substring, 2))


  landscape <- matrix(as.numeric(factor(landscape, levels = tipNumbers, labels = 1:nSpecies)), nrow = nrow(landscape))
  

  
  if (plotTraits == T){
    layout(mat = matrix(1:6, nrow = 2, byrow = T), widths = c(1,0.5,1), heights = c(0.15,0.8))
    par(mar = c(0,0,0,0), oma = c(2,2,2,2))
    
    plot.new()
    plot(NULL,frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7), ylim = c(0,3))
    text(1:4 - 0.5, 0, colnames(traits), srt = 45, pos = 4)

    plot.new()
    
    plot(extantPhylogeny, tip.color = cols)


    plot(rep(1:4, each = nSpecies), rep(1:nSpecies, 4), cex = size*traits, pch = 16, frame = F, yaxt='n', xaxt='n', ann=FALSE, xlim = c(0,7))


    
    }else{
    oldpar <- par(mfrow = c(1,3), mar = c(3,3,3,3))
    plot(extantPhylogeny, tip.color = cols) 
    par(oldpar)
  }
  par(mar = c(1,1,1,1))
  image(landscape, col = cols)

  
}


