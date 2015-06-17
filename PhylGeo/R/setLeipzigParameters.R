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
#' @param location A character vector specifing the folder to save the .xml file in (eg. "C:/parametersR/"). The "/" at the end is vital.
#' @param fileName A character vector specifing the name of the xml file (eg. "myParameters.xml")
#' @return Creates a .xml file at the specified location
#' @examples
#' # Define parameter values
#' scenarios = c("globalDens", "globalNoDens", 
#'               "localDens", "localNoDens")
#'x = rep(50,4)
#'y = rep(50,4)
#'runs = rep(3000,4)
#'dispersal = c(1,1,2,2)
#'nSpec = rep(1,4)
#'specRate = rep(2,4)
#'density = c(T,F,T,F)
#'densityStrength =  rep(0.4,4)
#'path <- "C:/"
#'filename <- "test.xml"
#'
#'# Set parameters for the model 
#'setLeipzigParametersXML(scenarios = scenarios, x = x, y = y, runs = runs, 
#'                 dispersal = dispersal, nSpec = nSpec, 
#'                 specRate = specRate, 
#'                 density = density, densityStrength = densityStrength, 
#'                 location = path,
#'                 fileName = filename)
#'                 
#' # Display model parameters in data frame
#' 
#' dat <- paste(path, filename, collapse = "", sep = "")
#' parameters <- getParametersXML(dat)
#' parameters
setLeipzigParametersXML <- function(scenarios, x, y, runs, dispersal, nSpec, specRate, density, densityStrength, location, fileName)
{
  parameters <- data.frame(scenarios = scenarios, x = x, y = y, runs = runs, dispersal = dispersal, nSpec = nSpec, specRate = specRate, density = as.integer(density), densityStrength = densityStrength)
  file <- paste(location, fileName, collapse = "", sep = "")
  kulife::write.xml(parameters, file)
}