#' @title Model of neutral community assembly
#' @description Runs a simple model of species community assembly under neutral conditions as defined by Hubbel. 
#' @param dim The edge-length of the square metacommunity matrix
#' @param specRate The speciation rate in total speciation events per generation
#' @param seed The random seed (do not change this option if you previously set the seed!)
#' @param runs The number of generations the model runs through
#' @details Be careful with the dimensions you choose. Large grids and many generations may take very long to compute. You can assume to have reached the equilibrium state of the model after dim^2/2.
#' @examples
#' # Run the model
#' metaCom <- NeutralMod(dim=50, specRate=2, seed=1500, runs=1500)
#' # Visualize the grid
#' image(metaCom)
#' # Calculate the species area-relation
#' rac(metaCom, plot=T)
NeutralMod <- function(  dim = 100, specRate = 2,  seed = NULL, runs = 5000 ){
  
  ptm <- proc.time()
  if (! is.null(seed)) set.seed(seed)

  counter <- 1
  mat  <- matrix(counter,dim,dim)
  
  for(i in 1:runs){
  
    for (j in 1:(dim^2)){
      coord = sample.int(n=dim,4, T )
      mat[coord[1], coord[2]] = mat[coord[3], coord[4]]
    }
    for (j in 1:specRate){
      SpecCoord = sample.int(n=dim,2, T )
      mat[SpecCoord[1], SpecCoord[2]] <- counter +1
      counter <- counter +1  
    }
    if(i %% 100 == 0) print(i)
  }
   print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
   return (mat)
}