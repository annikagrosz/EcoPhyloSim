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
