#' Convert TIFF to 6 MP JPEG
#'
#' If an image is a TIFF, make it a reasonably sized JPEG.
#'
#' @param dir A directory of *[tT][iI][fF][fF]? files.
#'
#' @export
tiff2jpeg <- function(dir = getwd()){

  oldwd <- getwd()
  setwd(dir)
  files <- list.files(pattern = "*[tT][iI][fF][fF]?")
  dir.create("jpeg", showWarnings = FALSE)

  for(file in files){ # For each file in directory

    numTicks <- progress(which(file == files), length(files), numTicks)
    img <- magick::image_read(file)
    img.info <- magick::image_info(img)

    if(img.info$width > img.info$height){
      img.new <- magick::image_scale(img, "3000x1999")
    }else{
      img.new <- magick::image_scale(img, "1999x3000")
    }

    img.out <- magick::image_convert(img.new, "jpeg")
    magick::image_write(
      img.out, path = paste0(
        "jpeg/", gsub("[tT][iI][fF][fF]?", "jpeg", file)))
  }

  setwd(oldwd)
}

#' Convert JPEG to 6 MP JPEG
#'
#' If an image is a JPEG, make it a reasonably sized JPEG.
#'
#' @param dir A directory of *[jJ][pP][eE]?[gG] files.
#'
#' @export
jpeg2jpeg <- function(dir = getwd()){

  oldwd <- getwd()
  setwd(dir)
  files <- list.files(pattern = "*[jJ][pP][eE]?[gG]")
  dir.create("jpeg", showWarnings = FALSE)

  for(file in files){ # For each file in directory

    numTicks <- progress(which(file == files), length(files), numTicks)
    img <- magick::image_read(file)
    img.info <- magick::image_info(img)

    if(img.info$width > img.info$height){
      img.new <- magick::image_scale(img, "3000x1999")
    }else{
      img.new <- magick::image_scale(img, "1999x3000")
    }

    magick::image_write(img.new, path = paste0("jpeg/", file))
  }

  setwd(oldwd)
}
