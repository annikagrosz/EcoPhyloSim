#' @title Expected species richness
#' @description Calculate the expected species richness in the equilibrium state as calculated by Hubbel
#' @param specRate The speciation rate in speciations per generation
#' @param dimensions The length of the grid edge
#' @return A float value for the expected species richness
#' @export
#' @examples
#' es(2, 100)
es <- function(specRate, dimensions){
  b <- specRate # Speciation rate
  theta <- 2 * b # Technically theta <- 2*J*nu ; nue is the speciation rate (b) per establishment. This means nue=b/J
  y <- c(0:(dimensions^2-1)) # vector of length J
  y <- theta / (theta + y) # Calculation of E(S) according to Hubbell
  ES <- sum(y) 
  return (ES)
}



