#' @title Get model parameters
#' @description Displays the model parameters from an .xml file as set by \code{\link{setModelParametersXML}} or \code{\link{setLeipzigParametersXML}}
#' @param file The .xml file to be inspected
#' @return A dataframe with the model parameters
#' @examples
#' # Set the model parameters
#' #path <- "C:/test.xml"
#' #setParametersXML(x = 50, y = 50, runs = 3000, dispersal = 1, nSpec = 1, specRate = 2, density = T, densityStrength = 0.4, location = path)
#'
#' # Retrive the model parameters
#' #modelParameters <- getParametersXML(path)
#' #modelParameters
getParametersXML <- function(file){
  load(file)
  #parameters <- XML::xmlToDataFrame(file)
  return (parameters)
}




#' @title Set Model parameters (Leipzig)
#' @description Creates a .xml file of the desired model parameters to be loaded into the model.
#' @param scenarios Character vector containing the names of the scenarios to be run
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Global dispersal, 2 = Nearest neighbor dispersal 
#' @param nSpec  Number of initial Species in the model landscape
#' @param specRate Numerical, the number of species introduced into the community during each generation
#' @param runs  Number of generations the model runs over
#' @param density Logical, determining whether or not density dependence influences the model
#' @param densityStrength Numerical, (0.0 - 1.0) determining the strength of the density dependence
#' @param location A character vector specifing the folder to save the .xml file in (eg. "C:/parametersR/"). The "/" at the end is vital.
#' @param fileName A character vector specifing the name of the xml file (eg. "myParameters.xml")
#' @return Creates a .xml file at the specified location
#' @examples
#' # Define parameter values
#' #scenarios = c("globalDens", "globalNoDens", 
#' #              "localDens", "localNoDens")
#' #x = rep(50,4)
#' #y = rep(50,4)
#' #runs = rep(3000,4)
#' #dispersal = c(1,1,2,2)
#' #nSpec = rep(1,4)
#' #specRate = rep(2,4)
#' #density = c(T,F,T,F)
#' #densityStrength =  rep(0.4,4)
#' #path <- "C:/"
#' #filename <- "test.xml"
#'
#' # Set parameters for the model 
#' #setLeipzigParametersXML(scenarios = scenarios, x = x, y = y, runs = runs, 
#' #                 dispersal = dispersal, nSpec = nSpec, 
#' #                 specRate = specRate, 
#' #                 density = density, densityStrength = densityStrength, 
#' #                 location = path,
#' #                 fileName = filename)
#'                 
#' # Display model parameters in data frame
#' 
#' #dat <- paste(path, filename, collapse = "", sep = "")
#' #parameters <- getParametersXML(dat)
#' #parameters
setLeipzigParametersXML <- function(scenarios, x, y, runs, dispersal, nSpec, specRate, density, densityStrength, location, fileName)
{
  parameters <- data.frame(scenarios = scenarios, x = x, y = y, runs = runs, dispersal = dispersal, nSpec = nSpec, specRate = specRate, density = as.integer(density), densityStrength = densityStrength)
  file <- paste(location, fileName, collapse = "", sep = "")
  kulife::write.xml(parameters, file)
}




#' @title Set Model parameters
#' @description Creates a .xml file of the desired model parameters to be loaded into the model.
#' @param scenarios Character vector containing the names of the scenarios to be run
#' @param x  Dimension of the model landscape in x-direction
#' @param y  Dimension of the model landscape in y-direction
#' @param dispersal 1 = Global dispersal, 3 = Local dispersal 
#' @param specRate Numerical, the number of species introduced into the community during each generation
#' @param runs  Number of generations the model runs over
#' @param density Logical, determining whether or not density dependence influences the model
#' @param environment Logical, determining whether or not the environment influences the model
#' @param mortalityFitness Logical, determining whether or not the individual fitness influences mortality
#' @param mortalityStrength Integer, determining the influence of fitness on mortality
#' @param reproductiveFitness Logical, determining whether or not the individual fitness influences reproducti
#' @param neutral Logical, determining whether or not the model is run under neutral conditions (overrides density and environment)
#' @param location A character vector specifing the folder to save the .xml file in (the filename is always parameters.xml and does not need to be set.)
#' @param dispersalCut Integer, defining the maximum dispersal distance of the kernel
#' @param densityCut Integer, defining the range for the density dependence
#' @param seed Integer, setting the random seed for the model
#' @param saveLocation Path of the folder to save the extracted files to
#' @return Creates a .xml file at the specified location
setModelParametersXML <- function(list, file)
{
  parameters <- as.data.frame(pars)
  save(parameters, file = file)
  #kulife::write.xml(parameters, location)
}

