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
setModelParametersXML <- function(scenarios, x, y, runs, dispersal, specRate, density, environment, neutral, location, dispersalCut, densityCut, seed)
{
  parameters <- data.frame(scenarios = scenarios, x = x, y = y, runs = runs, dispersal = dispersal, specRate = specRate, density = as.integer(density), environment = as.integer(environment), neutral = as.integer(neutral), dispersalCut = dispersalCut, densityCut = densityCut, seed = seed)
  file <- location
  kulife::write.xml(parameters, file)
}