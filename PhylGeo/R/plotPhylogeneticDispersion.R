

plotPhylogeneticDispersion <- function(pvalues, positions=NULL, title = "P-values", multiple = T){
  
  if (is.null(positions)){
    positions = list(x=1, y = 1, xname = "", yname = "")
  }  
  
  if(multiple == T){
    lengths = as.numeric(names(pvalues[[1]]))
    nlengths = length(lengths)
    lRange <- max(lengths) - min(lengths)
    
    z <- matrix(nrow = nlengths, ncol = length(pvalues), dimnames = list(names(pvalues[[1]]), names(pvalues)))
    zCILOW <- z
    zCIUP <- z
    for (i in 1:length(pvalues)){
      z[,i] <- sapply(pvalues[[i]], median, na.rm = T)
      zCILOW[,i] <- sapply(pvalues[[i]], function(x)quantile(x,0.25, na.rm = T))   
      zCIUP[,i] <- sapply(pvalues[[i]], function(x)quantile(x,0.75, na.rm = T))
    }
  }else{
    z <- sapply(pvalues, median, na.rm = T)
    zSD <- sapply(pvalues, sd, na.rm = T) 
  }
  
  
  
  ColPalet <- colorRampPalette(c("turquoise4", "white", "palevioletred"))
  Cols <- ColPalet(100)
  index <- seq(0,1,0.01)
  par(mar=c(0, 2, 0, 0), xpd=TRUE)
  shape::emptyplot(xlim=c(0, 5.5), ylim=c(0.5,2.5), frame.plot = FALSE)
  title(title, line=-2)
  
  
  
  for(i in 1:length(positions$y)){
    for(j in 1:length(positions$x)){
      
      x <- positions$x[j] / 2
      fitn = positions$xname[j]
      
      disp = positions$yname[i]
      y <- positions$y[i] / 2
      
      k <- 7*(i-1) + j
      
      if (multiple == T){
        
        mpv <- mean(z[,k])
        shape::filledrectangle(wx = 0.3, wy = 0.3, col = Cols[which.max(index[index <= mpv])], mid = c(x, y), angle = 0, lcol = "darkgrey")
        
        xval <- (lengths - min(lengths)) / lRange *0.3 -0.15
        yval <- z[,k] * 0.3 - 0.15
        yUP <- zCIUP[,k] * 0.3 - 0.15 
        yLOW <- zCILOW[,k] * 0.3 - 0.15
        
        polygon(c(x+xval, rev(x+xval)), c(y+yUP, rev(y+yLOW)), col = "#99999940", border = NA)
        lines(x+xval, y+yval)
        lines(c(x-0.15,x-0.13) , c(y , y ))
        lines(c(x+0.13,x+0.15) , c(y , y ))
        #lines(x+xval, y+ 0.5, lty=2)
        #lines(x+xval, y+yUP, lty = 2)
        #lines(x+xval, y+yLOW, lty = 2)
        
      }else{    
        shape::filledrectangle(wx = 0.3, wy = 0.3, col = Cols[which.max(index[index <= z[k]])], mid = c(x, y), angle = 0)
        text(x = x, y = y+0.05, labels = round(z[k],digits = 3))
        text(x = x, y = y-0.05, labels = round(zSD[k],digits = 3))        
      }
    }
  }
  
  text(x = 0, y = positions$y/2, labels = positions$yname , pos = 4) 
  text(x = positions$x/2, y = 2.3, labels =  positions$xname ) 
  
  barx <- 4.5
  bary <- 1.2
  barl <- 0.3
  fields::colorbar.plot(x = barx, y = bary, strip=seq(0,1,0.001), col=Cols, strip.length = barl, horizontal = F, strip.width = 0.04)
  text(x = barx + 0.2, y = bary + 1.8* barl, labels = "overdispersed" , pos = 4) 
  text(x = barx + 0.2, y = bary, labels = "neutral" , pos = 4) 
  text(x = barx + 0.2, y = bary - 1.8* barl, labels = "underdispersed \n(clustered)", pos = 4) 
}

