
library(R6)

glitchr <- R6Class(

  "glitchr",

  public = list(

    file = NULL,
    basename = NULL,
    version = NULL,
    original = NULL,
    current = NULL,
    active = NULL,
    log = NULL,

    initialize = function(file){

      self$file <- file

      self$basename <- paste0(dirname(file), "/",
                              strsplit(basename(file), split = "\\.")[[1]][1])

      self$version <- 1

      self$original <- tryCatch(jpeg::readJPEG(file),
                                error = function(e){
                                  tryCatch(png::readPNG(file))
                                }, error = function(e){
                                  stop("File not recognized.")
                                })

      self$current <- self$original

      self$active <- self$original

      self$log <- paste("Load", self$file)

      invisible(self)
    },

    print = function(){

      cat("Progress (if you believe in it):\n")
      cat(paste("*", self$log), sep = "\n")

      par(mfrow=c(1,2))

      o <- square(self$original)
      res <- dim(o)[1:2]
      plot(1,1, main = "Original",
           xlim=c(1,res[1]),ylim=c(1,res[2]),
           asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',
           xlab='',ylab='',bty='n')
      rasterImage(o,1,1,res[1],res[2])

      c <- square(self$current)
      res <- dim(c)[1:2]
      plot(1,1, main = "Current",
           xlim=c(1,res[1]),ylim=c(1,res[2]),
           asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',
           xlab='',ylab='',bty='n')
      rasterImage(c,1,1,res[1],res[2])

      par(mfrow=c(1,1))

      cat("Save changes with $ok()\n",
          "  (or try again)")

      invisible(self)
    },

    ok = function(){

      self$active <- self$current
      invisible(self)
    },

    .glitch = function(rule,
                       row = FALSE, # shift by row

                       xmin = 1, xmax = nrow(self$active),
                       ymin = 1, ymax = ncol(self$active),
                       channel = 1:dim(self$active)[3],

                       pos = TRUE, # shift in positive direction?
                       bw = 8, # bandwidth of shift
                       off = c(5, 50), # extent of shift
                       sparse = 0, # sparsity of shift (0-100)

                       shiftall = TRUE, # shift everything
                       rainbow = FALSE, # shift channels independently
                       melt = TRUE, # shift the target too

                       preserve = FALSE, # preserve the rule
                       domore = TRUE, # apply to shift and rule
                       do = NA, # additional function
                       ...){

      img <- self$active

      args <- as.list(substitute(list(...)))[-1]
      if(length(args) == 0) args <- NULL

      # Account for single channel or B&W input
      if(length(dim(img) == 3)){
        dims <- dim(img)[3]
      }else{
        dims <- 1
      }

      # For each channel...
      for(dim in 1:dims){

        # Transpose if performing by-row calculations
        if(row){
          temp <- t(img[,,dim])
          y <- xmin; Y <- xmax; x <- ymin; X <- ymax
        }else{
          temp <- img[,,dim]
          x <- xmin; X <- xmax; y <- ymin; Y <- ymax
        }

        # Build master offset guide before skipping channels
        if(dim == 1 & !rainbow){
          master <- sample(off[1]:off[2], size = ncol(temp), replace = TRUE)
        }

        # Randomly pick columns (or rows) to skip before skipping channels
        if(dim == 1 & sparse > 0 & sparse < 100){
          sparset <- sample(1:ncol(temp), round(sparse/100 * ncol(temp)))
        }

        # Only glitch selected channels
        if(!dim %in% channel) next

        for(i in y:Y){

          # Optionally jitter channels independently
          if(rainbow){
            if((i - y) %% bw == 0) offset <- sample(off[1]:off[2])[1]
          }else{
            if((i - y) %% bw == 0) offset <- master[i]
          }

          # If off[1] == off[2], use a fixed offset
          if(off[1] == off[2]){
            offset <- off[1]
          }

          # Skip columns (or rows) found in sparse set -- Do not move above offset!
          if((i - y) %% bw == 0) iBand <- i
          if(sparse > 0 & sparse < 100){
            if(iBand %in% sparset) next
          }

          # Allow 2D or 3D rules
          if(length(dim(rule)) == 3) rule <- rule[,,dim]
          if(row){
            index <- rule[i, ]
          }else{
            index <- rule[, i]
          }

          # Confine rule to within xmin and xmax
          within <- rep(FALSE, length(index))
          within[x:X] <- TRUE
          index <- index & within

          # Determine replacement values
          if(pos){
            if(shiftall){ # shift entire vector
              shift <- c(tail(temp[, i], offset), head(temp[, i], length(index) - offset))
              shift <- shift[index]
            }else{ # shift only the rule
              if(offset > sum(index)) offset <- sum(index)
              shift <- c(tail(temp[index, i], offset), head(temp[index, i], sum(index) - offset))
            }
          }else{
            if(shiftall){ # shift entire vector
              shift <- c(tail(temp[, i], length(index) - offset), head(temp[, i], offset))
              shift <- shift[index]
            }else{ # shift only the rule
              if(offset > sum(index)) offset <- sum(index)
              shift <- c(tail(temp[index, i], sum(index) - offset), head(temp[index, i], offset))
            }
          }

          # Determine replacement targets
          if(melt){ # melted jitter -- replace a shifted target
            if(pos){
              replace <- c(tail(index, offset), head(index, length(index) - offset))
            }else{
              replace <- c(tail(index, length(index) - offset), head(index, offset))
            }
          }else{ # electric jitter -- replace the rule target
            replace <- index
          }

          # Replace target with values
          if(preserve & melt) archive <- temp[index, i]
          temp[replace, i] <- shift

          # Optionally sort target (and rule)
          if(is.function(do)){
            if(domore) replace <- replace | index
            temp[replace, i] <- do.call(do, c(list(temp[replace, i]), args))
          }

          # Preserve rule
          if(preserve & melt) temp[index, i] <- archive
        }

        if(row) temp <- t(temp)
        self$current[,,dim] <- temp
      }
    }
  )
)

#
# glitchr <- function(dir){
#
#   glitchr$new(dir)
#   return(glitchr)
# }

a <- glitchr$new("/home/thom/Dropbox/R/projects/_lib-glitchr/AA001.jpg")
#a

a$.glitch(rule = a$original[,,1] >= .8, off = c(0, 0), bw = 16, sparse = 80, do = cos)
#a
a

#https://cran.r-project.org/web/packages/magick/vignettes/intro.html#drawing_and_graphics
library(magick)
frink <- image_read("/home/thom/Dropbox/R/projects/_lib-glitchr/AA001.jpg")
print(frink)
plot(frink)

image_read(as.raster(frink))
