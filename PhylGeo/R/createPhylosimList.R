#' @title PhylosimList creator
#' @description Creates an object of class "PhylosimList" from a list of "Phylosim" objects.
#' @param simu Multiple objects of type Phylosim as created by \code{\link{runSimulation}}
#' @return An object of type PhylosimList
#' @details The function is used to make functions for objets of type "PhylosimList" usable for objects of class "Phylosim". The function needs at least two objets to work.  
#' @examples 
#' ## Create two objects of type Phylosim
#' par1 <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = 500,
#'         density = 1, environment = 0.5, specRate = 1)
#' par2 <- createCompletePar(x = 50, y = 50, dispersal = "global" , runs = 500,
#'         density = 1, environment = 0.5, specRate = 2)
#'         
#' simu1 <- runSimulation(par1)
#' simu2 <- runSimulation(par2)
#' 
#' ## Use the function
#' simuList <- createPhylosimList(list(simu1,simu2))
#' 
#' ## The object can now be handed to functions designed for objects of type "PhylosimList".
#'  
#' @export


createPhylosimList<-function(simu){
  simuBatch<-simu
  if(length(simuBatch) < 2) stop("Invalid argument. List of objects too short.")
  class(simuBatch)<-"PhylosimList"
  return(simuBatch)
}

