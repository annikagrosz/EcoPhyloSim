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
    library(adephylo)
    library(shape)
    traits = getAverageTraits(simu)  
    rootDist <- distRoot(extantPhylogeny, 1)
  }
  
  tipDistances <- as.vector(as.matrix(distTips(extantPhylogeny, 1:2))[,1])+1
  
  if (col == "phylodist") cols<-rainbow(max(tipDistances), start = 0.19)[tipDistances]
  if (col == "equidist")  cols<-rainbow(nSpecies, start = 0.19)

  
  tipNumbers <- as.numeric(sapply(list(extantPhylogeny$tip.label), substring, 2))


  landscape <- matrix(as.numeric(factor(landscape, levels = tipNumbers, labels = 1:nSpecies)), nrow = nrow(landscape))
  

  
  if (plotTraits == T){
    layout(mat = matrix(1:3, nrow = 1), widths = c(1,0.3,1))
    xval = seq(1.25 * rootDist, 1.5*rootDist, len = 4)
    plot(extantPhylogeny, tip.color = cols, x.lim = 1.5*rootDist)
    size =  dev.size()[1] / 6
    
    plot(rep(xval, each = nSpecies), rep(1:nSpecies, 4), cex = size*traits, pch = 16, frame = F, yaxt='n', xaxt='n', ann=FALSE)

    }else{
    oldpar <- par(mfrow = c(1,3), mar = c(3,3,3,3))
    plot(extantPhylogeny, tip.color = cols) 
    par(oldpar)
  }
  
  image(landscape, col = cols)

  
}


