#' Square Image
#'
#' If an image is not square, add white space to make it square.
#'
#' @param img An image with any number of color channels.
#'
#' @export
square <- function(img){

  if(dim(img)[2] < dim(img)[1]){ # If image is a portrait

    # Create white space for sides
    filler.dim <- round((nrow(img) - ncol(img))/2, 0)
    filler <- matrix(1, nrow(img), filler.dim)

    # Create container for output
    newimg <- array(0, dim = c(nrow(img), ncol(img) + 2 * filler.dim, dim(img)[3]))
    for(i in 1:dim(img)[3]){ # For each color channel

      # Add white space to image
      newimg[,,i] <- do.call(cbind, list(filler, img[,,i], filler))
    }

  }else if(dim(img)[2] > dim(img)[1]){ # If image is a landscape

    # Create white space for sides
    filler.dim <- round((ncol(img) - nrow(img))/2, 0)
    filler <- matrix(1, filler.dim, ncol(img))

    # Create container for output
    newimg <- array(0, dim = c(nrow(img) + 2 * filler.dim, ncol(img), dim(img)[3]))
    for(i in 1:dim(img)[3]){ # For each color channel

      # Add white space to image
      newimg[,,i] <- do.call(rbind, list(filler, img[,,i], filler))
    }
  }

  return(newimg)
}
