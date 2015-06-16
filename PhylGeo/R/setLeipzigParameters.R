#' @title Set Model parameters (Leipzig)
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