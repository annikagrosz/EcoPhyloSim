#' @title Get model parameters
#' @description Displays the model parameters from an .xml file as set by \code{\link{setParametersXML}}
#' @param file The .xml file to be inspected
#' @return A dataframe with the model parameters
#' @examples
#' # Set the model parameters
#' path <- "C:/test.xml"
#' setParametersXML(x = 50, y = 50, runs = 3000, dispersal = 1, nSpec = 1, specRate = 2, density = T, densityStrength = 0.4, location = path)
#'
#' # Retrive the model parameters
#' modelParameters <- getParametersXML(path)
#' modelParameters
getParametersXML <- function(file){
  parameters <- XML::xmlToDataFrame(file)
  return (parameters)
}
