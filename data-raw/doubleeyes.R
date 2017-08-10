library(png)
a <- readPNG("/home/thom/Desktop/AA040.png")

y <- a
y.out <- y
for(dim in 1:3){
  
  offset <- sample(50:100)[1]
  for(i in 1:ncol(y)){
    
    #sortr <- y[,i,1] <.8 & y[,i,1] > .4
    sortr <- y[,i,3] > .5
    
    
    # move up
    #z <- c(tail(sortr, offset), head(sortr, length(sortr) - offset))
    # idk
    z <- c(tail(sortr, length(sortr) - offset), head(sortr, offset))
    #
    
    
    y.out[sortr, i, dim] <- y[z, i, dim]
  }
}

library(jpeg)
writeJPEG(y.out^1.1, target = "/home/thom/Desktop/AA040-glitch.jpg")
