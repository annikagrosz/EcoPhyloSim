# Neutrales Modell

library(compiler)

model <- function(  dim = 256, specRate <- 2,  seed = NULL){
  
  ptm <- proc.time()
  if (! is.null(seed)) set.seed(seed)

  counter <- 1
  mat  <- matrix(counter,dim,dim)
  
  for(i in 1:50000){
  
    for (j in 1:(dim^2)){
      coord = sample.int(n=dim,4, T )
      mat[coord[1], coord[2]] = mat[coord[3], coord[4]]
    }
    for (j in 1:specRate){
      SpecCoord = sample.int(n=dim,2, T )
      mat[SpecCoord[1], SpecCoord[2]] <- counter +1
      counter <- counter +1  
    }
    print(i)
  }
  
  print (paste("finished after", proc.time() - ptm))
  
  return (mat)
}

Cmodel = cmpfun(model)

out = Cmodel(seed = 1500)

par(mfrow = c(2,2))

image(out)


RAC <- as.data.frame(table(out))

b <- 2 # unsere artenstehungsrate
theta <- 2 * b #eigentlich theta <- 2 * J * nue , nue ist die artenstehungsrate b pro etablierungsevent also bei uns b/J
y_2 <- c(0:(100^2-1)) # vektor der laenge J
y_2 <- theta / (theta + y_2) # Berechnung von E(S) nach Hubbell
ES2 <- sum(y_2) # ~ 39

plot(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2, sub= " L=256, Speciation = 2 , runs = 100k", ylim=c(1,10000), xlim=c(0,50), col="blue")
points(x=ES2, y=1, col="red")

