#' @title Create Subplots
#' @description Generates subplots from a metacommunity, simulating field conditions
#' @param simu Simulation output of the class "Phylosim", usually consisting out of several lists. Needs to contain at least a species matrix ($specMat)
#' @param which.result Integer, determines which result should be used. This argument is only usefull if your 'runs' argument in \code{\link{createCompletePar}} contains more than one element. By default (NULL), the last result is used.
#' @param size A single value or a vector determining the size(s) of the generated subplots
#' @param n The number of subplots to be generated
#' @param community Logical, determining whether to generate a communiy table or not. default is FALSE
#' @param plot Logical, determining whether the results should be shown graphically. Default is FALSE.
#' @return A list of subplots and (if coomunity = T) a community table with plots in rows and species in collumns
#' @export
localPlots <- function(simu,which.result=NULL,size, n, community = FALSE, plot = FALSE){
  
  if(n ==1){
    n <- 2 
    single = T
  }else{
    single=F
  }

  if (is.null(which.result)) which.result = length(simu$Output) 
  simu <- simu$Output[[which.result]]

  
  matrix <- simu$specMat
  env <- simu$envMat
  if(plot == T) plotmat <-matrix(0, ncol(matrix), nrow(matrix))
  
  edge <- round(sqrt(size))-1
  subPlots <- list()
  envPlots <- list()
  communityTable <- data.frame("species"= numeric())
  
  for(i in 1:n)
  {                                                                          
    x = sample(x=1:length(matrix[1,]),size=1)
    y = sample(x=1:length(matrix[,1]),size=1)
    
    # Check for boundary issues and implemant warped bounaries if necessary
    if(x+edge > length(matrix[1,]) && y+edge <= length(matrix[,1])){
      rsel = c(seq(x,length(matrix[1,])), seq(1,(edge-length(seq(x,length(matrix[1,]))))))
      csel = seq(y,y+edge)
    }
    else if(x+edge <= length(matrix[1,]) && y+edge > length(matrix[,1])){
      rsel = seq(x,x+edge)
      csel = c(seq(y,length(matrix[,1])),seq(1,(edge-length(seq(y,length(matrix[,1]))))))
    }
    else if(x+edge > length(matrix[1,]) && y+edge > length(matrix[,1])){
      rsel = c(seq(x,length(matrix[1,])), seq(1,(edge-length(seq(x,length(matrix[1,]))))))
      csel = c(seq(y,length(matrix[,1])), seq(1,(edge-length(seq(y,length(matrix[,1]))))))
    }
    else{
      rsel = seq(x,x+edge)
      csel = seq(y,y+edge)
    }
    
    
    subPlots[[i]] <- matrix[rsel, csel]
    envPlots[[i]] <- env[rsel, csel]    
    
    if(community == T)
    {
      dataTable <- as.data.frame(table(subPlot))
      names(dataTable) <- c("species", paste("plot", i, collapse="", sep="")) 
      communityTable <- merge(communityTable, dataTable , all=T)
      communityTable[is.na(communityTable)] <- 0
    } 
    if(plot == T){
      plotmat[rsel, csel] <- 1
  
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
  
  if(plot == T){
    oldpar <- par()$mar
    par(mar=c(1,1,1,1))
    image(matrix,  yaxt='n', xaxt='n', asp=1, bty="n")
    image(plotmat, col=c(rgb(0,0,0,0),rgb(0,0,1,0.7)), add=T)
    par(mar=oldpar)
  } 
  
  if(community == T){
  return(list(subPlots=subPlots, communityTable=communityTable, envPlots=envPlots))
  } else return(list(subPlots=subPlots, envPlots=envPlots))

  }
