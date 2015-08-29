#' @title Species richness
#' @description Calculate the total species richness of a community
#' @param matrix A square matrix containing a species community (one individual per grid cell) 
#' @return An integer value for the species richness
specRich <- function(matrix){  
  sr <- length(unique(c(matrix)))
  return(speciesRichness = sr)
}

#' @title Rank-abundance relation 
#' @description Calculates the rank-abundance pattern for a community matrix. 
#' @param matrix Integer matrix representing the species community
#' @param plot Logical determining whether to plot the RAC or not
#' @details The higest abundance is assigned rank "1" while the lowest is assigned the rank corresponding with species richness.
#' @return A dataframe containing the ranked abundances, sorted by ascending rank.
rac <- function(matrix, plot="line"){
  Abundances <- as.data.frame(table(matrix))
  sel <- order(Abundances$Freq, decreasing=T)
  RAC <- data.frame(Rank = seq(1,length(Abundances$Freq),1), Abundance = Abundances$Freq[sel], Species = Abundances$matrix[sel])
  if(plot=="bar"){
    barplot(RAC$Abundance, log="y",ylab="Log abundance", xlab="Rank", main="RAC", names.arg = RAC$Rank)
  }
  if(plot=="line"){
    plot(RAC$Rank, RAC$Abundance, type="l",log="y",ylab="Log abundance", xlab="Rank", main="RAC", lwd=2)
  }
  
  return(RAC) 
}

