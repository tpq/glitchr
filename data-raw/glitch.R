glitch <- setRefClass(
  "glitch",
  fields =
    list(
      "base" = "character",
      "version" = "numeric",
      "img" = "array",
      "out" = "array"
    ),
  methods =
    list(
      initialize = function(file){
        base <<- paste0(dirname(file), "/",
                        strsplit(basename(file), split = "\\.")[[1]][1])
        version <<- 1
        img <<- tryCatch(jpeg::readJPEG(file),
                         error = function(e){
                           tryCatch(png::readPNG(file))
                         }, error = function(e){
                           stop("File not recognized.")
                         })
        out <<- img
      },
      update = function(){
        jpeg::writeJPEG(out, paste0(base, "-", version, ".jpg"))
        version <<- version + 1
        img <<- out
      },
      #' @param rule A 2D or 3D boolean array. Typically built from $img, but
      #'  maybe built from another image altogether?
      #' @param row A boolean. Toggles whether to jitter rows.
      #' @param pos A boolean. Toggles whether to jitter right (rows) or down (columns).
      #' @param bw A numeric. The bandwidth of the jitter.
      #' @param off Two numeric values. The range of pixels by which to offset the rule.
      #'  Repeat the same value twice for a fixed offset.
      #' @param rainbow A boolean. Toggles whether to jitter channels independently.
      #' @param melt A boolean. Toggles whether to overwrite rule layer. If TRUE, the shifted
      #'  values replace a shifted target. If FALSE, the shifted values replace the rule
      #'  target. If TRUE, image looks melted. If FALSE, image looks electric.
      #' @param shiftall A boolean. Toggles whether to shift the entire vector. If TRUE,
      #'  all values get shifted. If FALSE, only the rule values get shifted.
      #' @parm preserve A boolean. Toggles whether to preserve the rule. Note, this argument
      #'  does not work if \code{melt = FALSE}. Note, this argument applies to shifting
      #'  and pixel sorting.
      #' @param xmin,xmax,ymin,ymax A numeric. The bounds in which to apply the filter.
      #'  Note that in the moving dimension, these coordinates apply to defining the rule,
      #'  not to applying the rule.
      #' @param channel A numeric. Applies changes to only one color channel.
      #' @param sparse A numeric. The sparseness rating of filter as a percent.
      #'  Takes values from (0, 100).
      #' @param sort A boolean. Toggles whether to sort the target. Note, pixel sorting
      #'  occurs after shifting. Set \code{off = c(0, 0)} to skip offset.
      #' @param sortmore A boolean. Toggles whether to sort the rule in addition to
      #'  the target. Note, this argument does not work if \code{melt = FALSE}.
      #' @param sortrev A boolean. Toggles whether to reverse sort.
      .jit = function(rule, row = FALSE, pos = TRUE, bw = 8, off = c(5, 50),
                      rainbow = FALSE, melt = TRUE, shiftall = TRUE, preserve = FALSE,
                      xmin = 1, xmax = nrow(img), ymin = 1, ymax = ncol(img), channel = 1:3, sparse = 0,
                      sort = FALSE, sortmore = TRUE, sortrev = FALSE){
        
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
          
          # Only jitter selected channels
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
            if(sort){
              if(sortmore) replace <- replace | index
              temp[replace, i] <- sort(temp[replace, i], decreasing = sortrev)
            }
            
            # Preserve rule
            if(preserve & melt) temp[index, i] <- archive
          }
          
          if(row) temp <- t(temp)
          out[,,dim] <<- temp
        }
      }
    )
)
