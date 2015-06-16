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