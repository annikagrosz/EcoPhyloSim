for (i in seq(90000,99000,1000)){
  
  

  filename <- paste("C:\\Users\\Paul\\Desktop\\test1\\test_out_", i,".txt", collapse="", sep="")
  specMat <- as.matrix(read.csv(filename, header=F)[,-257]) # Species Matrix
  
  filename <- paste("C:/Users/Paul/Documents/model_out/timeline/", "global_", i,".txt", collapse="", sep="")
  specMat1 <- as.matrix(read.csv(filename, header=F)[,-257]) # Species Matrix
  
  if (sum(specMat != specMat1) > 0 || i %% 100 == 0){ 
    
    par(mfrow = c(2,2))
    image(specMat, col = rainbow(50))
    image(specMat1, col = rainbow(50))
    
    
    RAC <- as.data.frame(table(specMat))

    
    b <- 2 # unsere artenstehungsrate
    theta <- 2 * b #eigentlich theta <- 2 * J * nue , nue ist die artenstehungsrate b pro etablierungsevent also bei uns b/J
    y_2 <- c(0:(256^2-1)) # vektor der laenge J
    y_2 <- theta / (theta + y_2) # Berechnung von E(S) nach Hubbell
    ES2 <- sum(y_2) # ~ 39
    
    plot(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2, sub= " L=256, Speciation = 2 , runs = 100k", ylim=c(1,70000), xlim=c(0,50), col="blue")
    points(x=ES2, y=1, col="red")
    
    RAC <- as.data.frame(table(specMat1))

    
    plot(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2, sub= " L=256, Speciation = 2 , runs = 100k", ylim=c(1,70000), xlim=c(0,50), col="blue")
    points(x=ES2, y=1, col="red")
    
    
    if (sum(specMat != specMat1) > 0) {
      image(specMat != specMat1, col=c("red", "blue")) # red= True, blue = False
      
      print(paste("PROBLEM !!!!!", i))     
      
    }
    else print(paste("OK", i))
    

  }
  else print(paste("OK", i))
}
par(mfrow=c(1,1))
RAC <- as.data.frame(table(specMat))
RAC <- as.data.frame(table(specMat1))

b <- 2 # unsere artenstehungsrate
theta <- 2 * b #eigentlich theta <- 2 * J * nue , nue ist die artenstehungsrate b pro etablierungsevent also bei uns b/J
y_2 <- c(0:(256^2-1)) # vektor der laenge J
y_2 <- theta / (theta + y_2) # Berechnung von E(S) nach Hubbell
ES2 <- sum(y_2) # ~ 39

plot(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), type="l",log="y",ylab="Abundance", xlab="Rank", main="RAC", lwd=2, sub= " L=256, Speciation = 2 , runs = 100k", ylim=c(1,70000), xlim=c(0,50), col="blue")
points(x=ES2, y=1, col="red")

for(i in seq(10,1000,10)){
  filename <- paste("global_", i,".txt", collapse="", sep="")
  specMat <- as.matrix(read.csv(filename, header=F)[,-257]) # Species Matrix
  RAC <- as.data.frame(table(specMat))
  lines(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), col="#FF884450")
}
lines(seq(1,length(RAC$Freq),1),sort(RAC$Freq, decreasing=T), lwd=2, col="green")


i