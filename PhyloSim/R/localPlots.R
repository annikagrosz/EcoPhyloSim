#' @title Create Subplots
#' @description Generates subplots from a metacommunity, simulating field conditions
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least a species matrix ($specMat)
#' @param which.simulation defines which simulation run to choose in case you defined to save at multiple time steps. The default is the last one.
#' @param size A single value or a vector determining the size(s) of the generated subplots
#' @param n The number of subplots to be generated
#' @param community Logical, determining whther to generate a communiy table or not. default is FALSE
#' @return A list of subplots and (if coomunity = T) a community table with plots in rows and species in collumns
#' @export
localPlots <- function(simu,which.simulation=NULL,size, n, community=F){
  
  if(n ==1){n <- 2 
            single = T}else{single=F}
  
  if("PhyloSim" %in% class(simu)==T){
    if (is.null(which.simulation)) which.simulation = length(simu$Output) 
    simu <- simu$Output[[which.simulation]]}
  
  matrix <- simu$specMat
  
  edge <- round(sqrt(size))-1
  subPlots <- list()
  communityTable <- data.frame("species"= numeric())
  for(i in 1:n)
  {                                                                          
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
   }
  
  speciesNames <- character()
  for(b in 1:length(communityTable$species))
  {
    speciesNames[b] <-  paste("s" ,communityTable$species[b], collapse="", sep="")
  }
  
  if(community == T)
  {
    communityTable$species <- speciesNames
    communityTable <- as.data.frame(t(communityTable), stringsAsFactors=F)
    communityTable <- communityTable[-1,]
    names(communityTable) <- speciesNames
    communityTable <- as.data.frame(sapply(communityTable, as.numeric), row.names=row.names(communityTable))
  }
  
  if(single ==T){communityTable <- communityTable[-2,]}
  return(list(subPlots=subPlots, communityTable=communityTable))
  
  }
