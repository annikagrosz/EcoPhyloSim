
#landscape = mat
#phylogeny = extantPhylo

# could maybe make nicer plots with http://cran.r-project.org/web/packages/phytools/index.html in the future, but increases dependencies


plotSpatialPhylo <- function (landscape, phylogeny, plot = "both"){
  
  distances <- cophenetic(phylogeny)[1,]
  
  cols<-heat.colors(length(distances))
  cols<-rainbow(length(distances), start = 0.2)

  
  tipNumbers <- as.numeric(sapply(list(phylogeny$tip.label), substring, 2))


  landscape <- matrix(as.numeric(factor(landscape, levels = tipNumbers, labels = 1:length(distances))), nrow = nrow(landscape))
  
  
  if (plot == "both") oldpar <- par(mfrow = c(1,2), mar = c(3,3,3,3))

  plot(phylogeny, tip.color = cols)

  image(landscape, col = cols)
  
  
  par(oldpar)
  
}