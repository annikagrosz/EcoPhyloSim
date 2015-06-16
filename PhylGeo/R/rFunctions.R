#' @title Rank-abundance relation 
#' @description Calculates the rank-abundance pattern for a community matrix. 
#' @param matrix Integer matrix representing the species community
#' @param plot Logical determining whether to plot the RAC or not
#' @details The higest abundance is assigned rank "1" while the lowest is assigned the rank corresponding with species richness.
#' @return A dataframe containing the ranked abundances, sorted by ascending rank.
rac <- function(matrix, plot=T){
  Abundances <- as.data.frame(table(matrix))
  RAC <- data.frame(Rank = seq(1,length(Abundances$Freq),1), Abundance = sort(Abundances$Freq, decreasing=T))
  if(plot==T){
    plot(RAC, type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2)
  }
  return(RAC) 
}

#' @title Expected species richness
#' @description Calculate the expected species richness in the equilibrium state as calculated by Hubbel
#' @param specRate The speciation rate in speciations per generation
#' @param dimensions The length of the grid edge
#' @return A float value for the expected species richness
#' @examples
#' es(2, 100)
es <- function(specRate, dimensions){
  b <- specRate # Speciation rate
  theta <- 2 * b #eigentlich theta <- 2 * J * nue , nue ist die artenstehungsrate b pro etablierungsevent also bei uns b/J
  y <- c(0:(dimensions^2-1)) # vektor der laenge J
  y <- theta / (theta + y) # Berechnung von E(S) nach Hubbell
  ES <- sum(y) 
  return (ES)
}

#' @title Metacommunity subplots
#' @description Generates subplots from a metacommunity, simulating field conditions
#' @param size A single value or a vector determining the size(s) of the generated subplots
#' @param n The number of subplots to be generated
#' @param matrix A square matrix containing the metacommunity (one individual per grid cell) 
#' @param community Logical, determining whther to generate a community matrix or not default is FALSE
#' @return A list of subplots and (if coomunity = T) a community matrix with plots in rows and species in collumns
localPlots <- function(size, n, matrix, community=F){
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
    if(community == T)
    {
      dataTable <- as.data.frame(table(subPlot))
      names(dataTable) <- c("species", paste("plot", i, collapse="", sep="")) 
      communityTable <- merge(communityTable, dataTable , all=T)
      communityTable[is.na(communityTable)] <- 0
    } 
      
      # Transponate the community matrix
      
    }
    speciesNames <- character()
    for(b in 1:length(communityTable$species)){
      speciesNames[b] <-  paste("s" ,communityTable$species[b], collapse="", sep="")
    }
  if(community == T){
    communityTable$species <- speciesNames
    communityTable <- as.data.frame(t(communityTable), stringsAsFactors=F)
    communityTable <- communityTable[-1,]
    names(communityTable) <- speciesNames
    communityTable <- as.data.frame(sapply(communityTable, as.numeric), row.names=row.names(communityTable))
  }
  return(list(subPlots=subPlots, communityTable=communityTable))
}

#' @title Species richness
#' @description Calculate the total species richness of a community
#' @param matrix A square matrix containing a species community (one individual per grid cell) 
#' @return An integer value for the species richness
specRich <- function(matrix){  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}

#' @title Species-area relation
#' @description Calculates the species-area pattern for a community matrix. 
#' @param area A single value or a vector determining the size(s) of the generated subplots
#' @param matrix The species community
#' @param rep The number of repetitions per size to calculate the mean 
#' @param plot Logical determining whether to plot the SAC or not
#' @return A list containing the mean species richness for each size and the respective standard deviation
sac <- function(area, matrix, rep=5, plot=T){
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
  return(data.frame(area = area, sr.Mean = meanSpeciesRichness,sr.SD = meanSpeciesRichnessSD))
}

#' @title Colless' imbalance
#' @description Calculates the Colless' imbalance for a Phylogeny.
#' @param phylo An object of class 'phylo'
#' @return A numeric value for the Colless' Imbalance 
collessImbalance <- function(phylo){ #colless imbalace function
  balance <- balance(phylo)
  sum.diff <- 0
    for(i in 1:length(balance[,1])){
    diff <- abs(balance[i,1] - balance[i,2])
    sum.diff <- sum.diff + diff 
  }
  c.imbal <- 2*sum.diff/((sum(balance[1,])-1)*(sum(balance[1,])-2))
  return(as.numeric(c.imbal))
}

#' @title Phylogenetic clades
#' @description Visualises the phylogenetic clades within a metacommunity from local community plots. 
#' @param phylo An object of class 'phylo'
#' @param community A community matrix
#' @param n The number of clades to visualise
clades <- function(phylo, community, n){

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

#' @title Set Model parameters
#' @description Creates a .xml file of the desired model parameters to be loaded into the model.
#' @param scenarios Character vector containing the names of the scenarios to be run
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 2 = Nearest neighbor dispersal 
#' @param nSpec  Number of initial Species in the model landscape
#' @param specRate Numerical the number of species introduced into the community during each generation
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param densityStrength Numerical (0.0 - 1.0) determining the strength of the density dependence
#' @param location A character vector specifing the folder to save the .xml file in (the filename is always parameters.xml and does not need to be set.)
#' @return Creates a .xml file at the specified location
setLeipzigParametersXML <- function(scenarios, x, y, runs, dispersal, nSpec, specRate, density, densityStrength, location){
  parameters <- data.frame(scenarios = scenarios, x = x, y = y, runs = runs, dispersal = dispersal, nSpec = nSpec, specRate = specRate, density = as.integer(density), densityStrength = densityStrength)#
  file <- location
  write.xml(parameters, file)
}


#' @title Set Model parameters
#' @description Creates a .xml file of the desired model parameters to be loaded into the model.
#' @param scenarios Character vector containing the names of the scenarios to be run
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Gloabl dispersal, 3 = Local dispersal 
#' @param specRate Numerical the number of species introduced into the community during each generation
#' @param runs  Number of generations the model runs over
#' @param density Logical determining whether or not density dependence influences the model
#' @param environment Logical determining whether or not the environment influences the model
#' @param neutral Logical determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param location A character vector specifing the folder to save the .xml file in (the filename is always parameters.xml and does not need to be set.)
#' @param dispersalCut Integer defining the maximum dispersal distance of the kernel
#' @param densityCut Integer defining the range for the density dependence
#' @param seed Integer setting the random seed for the model
#' @return Creates a .xml file at the specified location
setModelParametersXML <- function(scenarios, x, y, runs, dispersal, specRate, density, environment, neutral, location, dispersalCut, densityCut, seed){
  parameters <- data.frame(scenarios = scenarios, x = x, y = y, runs = runs, dispersal = dispersal, specRate = specRate, density = as.integer(density), environment = as.integer(environment), neutral = as.integer(neutral), dispersalCut = dispersalCut, densityCut = densityCut, seed = seed)
  file <- location
  write.xml(parameters, file)
}


#' @title Get model parameters
#' @description Displays the model parameters from an .xml file as set by \code{\link{setParametersXML}}
#' @param file The .xml file to be inspected
#' @return A dataframe with the model parameters
#' @examples
#' # Set the model parameters
#' path <- "C:/test.xml"
#' setParametersXML(x = 50, y = 50, runs = 3000, dispersal = 1, nSpec = 1, specRate = 2, density = T, densityStrength = 0.4, location = path)
#'
#' # Retrive the model parameters
#' modelParameters <- getParametersXML(path)
#' modelParameters
getParametersXML <- function(file){
  parameters <- xmlToDataFrame(file)
  return (parameters)
}
