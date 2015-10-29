## Neutral Model. Function can be called by runSimulation.

NeutralMod <- function(  xdim = 100, ydim=100, specRate = 2,  seed = NULL, runs = 500 ){
  
  ptm <- proc.time()
  if (! is.null(seed)) set.seed(seed)
  
  counter <- 1
  mat  <- matrix(counter,xdim, ydim)
  
  for(i in 1:runs){
    
    for (j in 1:(xdim*ydim)){
      coordx = sample.int(n=xdim,2, T )
      coordy = sample.int(n=ydim,2, T )
      mat[coordx[1], coordy[1]] = mat[coordx[2], coordy[2]]
    }
    for (j in 1:specRate){
      SpecCoordx = sample.int(n=xdim,1, T )
      SpecCoordy = sample.int(n=ydim,1, T )
      mat[SpecCoordx, SpecCoordy] <- counter +1
      counter <- counter +1  
    }
    if(i %% 100 == 0) print(i)
  }
  print (paste("Finished after",floor(((proc.time() - ptm)[3])/60), "minute(s) and", ((proc.time() - ptm)[3])%%60, "second(s)."))
  return (mat)
}