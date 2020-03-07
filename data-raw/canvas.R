
a <- glitch$new("AA032A.jpg")
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, rainbow = FALSE,
       ymin = 500, ymax = 2500, xmin = 500, xmax = 1000,
       sparse = 50)

a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, rainbow = FALSE,
       overwrite = TRUE,
       ymin = 500, ymax = 2500, xmin = 500, xmax = 1000,
       sparse = 80)

a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, rainbow = FALSE,
       overwrite = FALSE, channel = 1,
       sparse = 20)

a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, off = c(0, 0), rainbow = FALSE,
       overwrite = TRUE, channel = 1:3,
       sparse = 20)

a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, off = c(0, 1000), rainbow = FALSE,
       overwrite = TRUE, channel = 2,
       sparse = 20)


writeJPEG(a$out, "/home/thom/Desktop/AA.jpg")

# important pixel sorter ideas:
# add random bandwidth sizes...
# get edge detection and use edges to build the rule
# sortintervals --- hmmmm
# sort rule
# sort shift
# sort.rev

#' @param sortatall A boolean. Toggles whether to sort the image.
#' @param sortmore A boolean. Toggles whether to sort both the indexed layer
#'  (via rule) and the target layer (via off). If FALSE, this sorts only the
#'  target layer (which is the indexed layer if \code{off = c(0, 0)}).
#' @param sortrev A boolean. Toggles whether to sort in reverse.


# Optionally sort target
# if(sort){
#   if(sortmore){ # sort target and [xxx]
#     sort(temp[replace | index, i], decreased = sort.rev)
#   }else{ # sort target and rule (if melt = TRUE)
#     sort(temp[replace, i], decreased = sort.rev)
#   }
# }

a <- glitch$new("/home/thom/Desktop/AA003A.jpg")
a$.jit(row = FALSE, pos = TRUE, rule = a$img[,,1] < .2, rainbow = FALSE,
       melt = TRUE, off = c(80, 320), sparse = 0, bw = 8, shiftall = FALSE,
       channel = 1)
library(jpeg)
writeJPEG(a$out, "/home/thom/Desktop/AA.jpg")
a$img <- a$out
a$.jit(row = FALSE, pos = TRUE, rule = a$img[,,1] > .2, rainbow = FALSE,
       melt = TRUE, off = c(100, 110), sparse = 0, bw = 16, shiftall = TRUE,
       ymin = 737, ymax = 907)
writeJPEG(a$out, "/home/thom/Desktop/AA2.jpg")
a$img <- a$out
a$.jit(row = TRUE, pos = TRUE, rule = a$img[,,1] > .2, rainbow = FALSE,
       melt = FALSE, off = c(800, 1000), sparse = 80, bw = 8, shiftall = TRUE,
       ymin = 737, channel = 2)
writeJPEG(a$out, "/home/thom/Desktop/AA3.jpg")
a$img <- a$out
a$.jit(row = FALSE, pos = TRUE, rule = a$img[,,1] < .2, rainbow = FALSE,
       melt = FALSE, off = c(800, 1000), sparse = 80, bw = 8, shiftall = TRUE)
writeJPEG(a$out, "/home/thom/Desktop/AA4.jpg")

# a$img <- a$out
# a$.jit(row = FALSE, pos = FALSE, rule = a$img[,,1] > .5, rainbow = FALSE,
#        xmin = 1000, sparse = 50, off = c(50, 200))
# writeJPEG(a$out, "/home/thom/Desktop/AA.jpg")

plot(1)
rasterImage(a$out, 0, 100, 0, 100)

a$
  a$update <- function(){
    version <<- version + 1
    writeJPEG(a$out, "xxxx")
    a$img <<- a$out
  }



setwd("/home/thom/Dropbox/R/glitch/")
source("glitch.R")
a <- glitch$new("AA037.jpg")
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] < .5, rainbow = FALSE,
       xmin = 1200, xmax = 1400, off = c(50, 100), bw = 12, sparse = 50)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")
a$img <- a$out
a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .5, rainbow = FALSE,
       xmin = 800, xmax = 1200, melt = TRUE, off = c(200, 200), bw = 36, sparse = 60)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")
a$img <- a$out
a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .9, rainbow = FALSE,
       xmax = 800, melt = TRUE, off = c(400, 400), bw = 36, sparse = 60)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")
a$img <- a$out
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] < .9, rainbow = FALSE, shiftall = FALSE,
       ymax = 300, melt = FALSE, off = c(400, 400), bw = 8, sparse = 40)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")

a$img <- a$out
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .8, rainbow = FALSE, shiftall = FALSE,
       xmin = 300, xmax = 800, melt = TRUE, off = c(5, 100), bw = 8, sparse = 80)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")


a$img <- a$out
a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE,
       xmin = 1400, melt = TRUE, off = c(5, 100), bw = 64, sparse = 80)
jpeg::writeJPEG(a$out, "AA037-glitch.jpg")


setwd("/home/thom/Dropbox/R/glitch/")
source("glitch.R")
a <- glitch$new("AA037-glitch (copy).jpg")
z <- a$img

for(i in 1:3){
  z[,,i] <- sin(a$img[,,i])
}

jpeg::writeJPEG(z, "AA037-sine.jpg")


for(i in 1:3){
  z[,,i] <- cos(a$img[,,i])
}

jpeg::writeJPEG(z, "AA037-cosine.jpg")


z <- a$img
for(i in 1:3){
  for(j in 1:nrow(a$img)){
    z[j,,i] <- (a$img[j,,i] + c(tail(a$img[j,,i], 2),
                                head(a$img[j,,i], length(a$img[j,,i]) - 2))) / 2
  }
}

jpeg::writeJPEG(z, "AA037-shift.jpg")





z <- a$img
for(i in 1:3){
  
  for(j in 1:nrow(a$img)){
    # z[j,,i] <- (a$img[j,,i]) + 
    # compositions::rDirichlet.acomp(length(a$img[j,,i]),alpha=c(A=2,B=0.2,C=0.5))[,2]
    # z[j,,i] <- tanh(a$img[j,,i])
    # z[j,,1] <- (a$img[j,,i] +
    #               (sin(seq(1, 180, length.out = 1999)) + 1)/2) / 2
    
    # z[j,,1] <- (a$img[j,,i] +
    #               (sin(seq(1, 180, length.out = 1999)) + 1)/2) / 2
    
    #plot((a$img[,,1] +
    #(sin(seq(1, 3, length.out = length(a$img[,,1]))) + 1)/2) / 2, type = "l")
    
    z[j,,i] <- 
      
      plot(signal::conv(a$img[1,,1], signal::gausswin(length(a$img[1,,1]), w = 2)))
    
  }
}

jpeg::writeJPEG(z, "AA037-etc.jpg")

setwd("/home/thom/Dropbox/R/glitch/")
source("glitch.R")
a <- glitch$new("AA037-glitch (copy).jpg")
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] > .4, rainbow = FALSE, shiftall = TRUE, preserve = TRUE,
       melt = TRUE, off = c(0, 0), bw = 40, sparse = 80,
       sort = TRUE, sortmore = FALSE)
a$update()

a$.jit(row = TRUE, pos = TRUE, a$img[,,1] > .4, rainbow = FALSE, shiftall = TRUE,
       xmin = 1400, melt = TRUE, off = c(400, 400), bw = 80, sparse = 90, preserve = TRUE,
       sort = TRUE, sortmore = TRUE)
a$update()

z <- a$img


tmp <- compositions::rDirichlet.acomp(10,alpha=c(A=2,B=0.2,C=0.2))
plot(tmp)




as.raw(a$img[1,,1])

y <- colorspace::RGB(R = a$img[1,,1], G = a$img[1,,2], B = a$img[1,,3])
z <- colorspace::hex(y)
as.raw(z)





source("glitch.R")
a <- glitch$new("AA016-glitch.jpg")
a$.jit(row = FALSE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = FALSE, preserve = FALSE,
       melt = TRUE, off = c(40, 80), bw = 12, sparse = 10)

a$update()

a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE, preserve = FALSE,
       melt = TRUE, off = c(10, 10), bw = 2, sparse = 80, xmax = 1000)
a$update()


a$.jit(row = FALSE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE, preserve = FALSE,
       melt = TRUE, off = c(100, 200), bw = 18, sparse = 80, ymin = 2000)
a$update()


a$.jit(row = FALSE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE, preserve = FALSE,
       melt = TRUE, off = c(100, 200), bw = 18, sparse = 80, ymax = 800)
a$update()

a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE, preserve = FALSE,
       melt = TRUE, off = c(20, 40), bw = 2, sparse = 80, xmax = 1000)
a$update()


a$.jit(row = TRUE, pos = TRUE, a$img[,,1] < .4, rainbow = FALSE, shiftall = TRUE, preserve = FALSE,
       melt = TRUE, off = c(40, 80), bw = 4, sparse = 90, xmax = 1000)
a$update()


# gaussian smooth image
# laplace edge detection

# 
# In image processing
# 
# See also: digital signal processing
# 
# In digital image processing convolutional filtering plays an important role in many important algorithms in edge detection and related processes.
# In optics, an out-of-focus photograph is a convolution of the sharp image with a lens function. The photographic term for this is bokeh.
# In image processing applications such as adding blurring.
# 
