#' Calculate the rank-abundance pattern for a community matrix. The higest abundance is assigned rank "1" while the lowest is assigned the rank corresponding with species richness.
#'
#' @param matrix The species community
#' @return A dataframe containing the ranked abundances, sorted by ascending rank.
#' @examples
#' 
rac <- function(matrix, plot){
  Abundances <- as.data.frame(table(matrix))
  RAC <- data.frame(Rank = seq(1,length(Abundances$Freq),1), Abundance = sort(Abundances$Freq, decreasing=T))
  if(plot==T){
    plot(RAC, type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2)
  }
  return(RAC) 
}
#' Calculate the expected species richness in the equilibrium state as calculated by Hubbel
#'
#' @param specrate The speciation rate in speciations per generation
#' @param dimensions The length of the grid edge
#' @return A float value for the expected species richness
#' @examples
#' es(2, 100)
es <- function(specRate, dimensions){
  b <- specrate # Speciation rate
  theta <- 2 * b #eigentlich theta <- 2 * J * nue , nue ist die artenstehungsrate b pro etablierungsevent also bei uns b/J
  y <- c(0:(dimensions^2-1)) # vektor der laenge J
  y <- theta / (theta + y) # Berechnung von E(S) nach Hubbell
  ES <- sum(y) 
  return (ES)
}

localPlots <- function(size, n, matrix){
  edge <- round(sqrt(size))
  subPlots <- list()
  communityTable <- data.frame("species"= numeric())
  for(i in 1:n){                                                                          
    x = sample(x=1:length(matrix[1,]),size=1)
    y = sample(x=1:length(matrix[,1]),size=1)
    
    # Check for boundary issues and implemant warped bounaries if necessary
    if(x+edge > length(matrix[1,]) && y+edge <= length(matrix[,1]))
      subPlot <- matrix[c(seq(x,length(matrix[1,])),
                             seq(1,(edge-length(seq(x,length(matrix[1,])))))),seq(y,y+edge)-1]
    
    else if(x+edge <= length(matrix[1,]) && y+edge > length(matrix[,1]))
          subPlot <- matrix[seq(x,x+edge)-1, c(seq(y,length(matrix[,1])),seq(1,(edge-length(seq(y,length(matrix[,1]))))))] 
    
    else if(x+edge > length(matrix[1,]) && y+edge > length(matrix[,1]))
          subPlot <- matrix[c(seq(x,length(matrix[1,])), seq(1,(edge-length(seq(x,length(matrix[1,])))))), 
                            c(seq(y,length(matrix[,1])),seq(1,(edge-length(seq(y,length(matrix[,1]))))))]    
    else
          subPlot <- matrix[seq(x,x+edge),seq(y,y+edge)]
  
    subPlots[[i]] <- subPlot
    dataTable <- as.data.frame(table(subPlot))
    names(dataTable) <- c("species", paste("plot", i, collapse="", sep="")) 
    communityTable <- merge(communityTable, dataTable , all=T)
    communityTable[is.na(communityTable)] <- 0
    speciesNames <- character()
    for(b in 1:length(communityTable$species)){
      speciesNames[b] <-  paste("s" ,communityTable$species[b], collapse="", sep="")
    }
    communityTable$species <- speciesNames
    
    # Transponate the community matrix
    communityTable <- as.data.frame(t(communityTable), stringsAsFactors=F)
    communityTable <- communityTable[-1,]
    names(communityTable) <- speciesNames
    communityTable <- as.data.frame(sapply(communityTable, as.numeric), row.names=row.names(communityTable))
  }
  return(list(subPlots=subPlots, communityTable=communityTable))
}

specRich <- function(matrix){  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}

sac <- function(area, matrix, rep, plot){
  meanSpeciesRichness <- numeric()
  meanSpeciesRichnessSD <- numeric()
  #Loop over all plot sizes
  for(i in 1:length(area)){ 
   # Repetitions for each plot size
    subPlots <- localPlots(size=area[i], n = rep, matrix=matrix)$subPlots
    speciesRichness <- sapply(subPlots, specRich)
    meanSpeciesRichness[i] <- mean(speciesRichness)
    meanSpeciesRichnessSD[i] <- sd(speciesRichness)
  }
  if(plot == T){
    plot(area,meanSpeciesRichness,type="b", log="xy", xlab="Area (n cells)", ylab="Number of Species", main="SAC", lwd=2, pch=4)
    polygon(x=c(area,rev(area)), y=c(meanSpeciesRichness + 1.96*meanSpeciesRichnessSD,rev(meanSpeciesRichness - 1.96*meanSpeciesRichnessSD)), col="#00000030", border=NA)
    lines(area, (meanSpeciesRichness + 1.96*meanSpeciesRichnessSD), col="red", lty=2, lwd=2)
    lines(area, (meanSpeciesRichness - 1.96*meanSpeciesRichnessSD), col="red", lty=2, lwd=2)
  }
  return(list(sr.Mean = meanSpeciesRichness,sr.SD = meanSpeciesRichnessSD))
}

collessImbalance <- function(phylo){ #colless imbalace function
  balance <- bal(phylo)
  sum.diff <- 0
    for(i in 1:length(balance[,1])){
    diff <- abs(balance[i,1] - balance[i,2])
    sum.diff <- sum.diff + diff 
  }
  c.imbal <- 2*sum.diff/((sum(balance[1,])-1)*(sum(balance[1,])-2))
  return(as.numeric(collessImbalance = c.imbal))
}


clades <- function(phylo, community, n)

  for( i in 1:n){
  x <- phylo$tip.label[is.element(phylo$tip.label,names(community[a,which(community[i,] >0)]))]
  y <- phylo$tip.label[!is.element(phylo$tip.label,names(community[a,which(community[i,] >0)]))]
  xy <- list(x,y)
  
  group <- which.edge(phylo, x)
  
  cladecolor<-rep("darkgrey", dim(phylo$edge)[1])
  cladecolor[group]<-"black"
  
  cladewidth<-rep(1, dim(phylo$edge)[1])
  cladewidth[group]<-2
  
  title <- paste("Phylogenetic clade plots \n Clade", i)
  plot(phylo,  direction="downwards", show.tip.label=F, show.node.label=F, edge.color=cladecolor, edge.width=cladewidth, main = title)
  add.scale.bar()
  }
}

