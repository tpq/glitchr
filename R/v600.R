#' Crop Scans from V600
#'
#' @param dir Directory
#' @param negate A logical. Toggles whether to negate images.
#' @export
v600_4x5 <- function(dir = getwd(), negate = TRUE){

  oldwd <- getwd()
  setwd(dir)
  files <- list.files(pattern = "*[tT][iI][fF][fF]?")
  dir.create("out", showWarnings = FALSE)
  dir.create("out/raw", showWarnings = FALSE)
  for(file in files){

    numTicks <- glitchr::progress(which(file == files), length(files), numTicks)

    img <- magick::image_read(file)
    img.info <- magick::image_info(img)

    # Decide where to crop the image
    w <- img.info$width
    h <- img.info$height

    `%+%` <- function(a,b) paste0(a,b)
    img.new <- img
    if(negate) img.new <- magick::image_negate(img.new)

    new.info <- magick::image_info(img.new)
    w <- new.info$width
    h <- new.info$height

    s <- 1
    for(LR in 1:2){
      for(TB in 1:2){

        j <- round(h/2)
        h_offset <- j * LR - j
        k <- round(w/2)
        w_offset <- k * TB - k

        img.s <- magick::image_crop(img.new, geometry = k %+%
                                      "x" %+% j %+% "+" %+% w_offset %+% "+" %+% h_offset)
        img.s <- magick::image_normalize(img.s)
        name <- gsub("\\.[tT][iI][fF][fF]?", "-" %+% s %+% ".tiff", file)
        magick::image_write(img.s, path = paste0("out/", name))
        s <- s+1
      }
    }

    file.copy(from = file, to = paste0("out/raw/", file))
    file.remove(file)
  }

  setwd(oldwd)
}

#' Crop Scans from V600
#'
#' @param dir Directory
#' @param negate A logical. Toggles whether to negate images.
#' @param split An integer. Number of frames per scan.
#' @export
v600_120 <- function(dir = getwd(), negate = TRUE, split = 1){

  oldwd <- getwd()
  setwd(dir)
  files <- list.files(pattern = "*[tT][iI][fF][fF]?")
  dir.create("out", showWarnings = FALSE)
  dir.create("out/raw", showWarnings = FALSE)
  for(file in files){

    numTicks <- glitchr::progress(which(file == files), length(files), numTicks)

    img <- magick::image_read(file)
    img.info <- magick::image_info(img)

    # Decide where to crop the image
    w <- img.info$width
    h <- img.info$height
    adj <- 3828/4312 * img.info$width
    trim.r <- round((w-adj))
    trim.l <- round(255/4312 * w)
    trim.t <- round(2612/15263 * h)
    #trim.b <- round((img.info$height-14900)/img.info$height * img.info$height)
    adj <- 14340/15263 * h
    trim.b <- round((h-adj))

    `%+%` <- function(a,b) paste0(a,b)
    crop <- (w-trim.r-trim.l) %+% "x" %+% (h-trim.t-trim.b) %+% "+" %+% trim.l %+% "+" %+% trim.t
    img.new <- magick::image_crop(img, geometry = crop)
    if(negate) img.new <- magick::image_negate(img.new)

    if(split > 1){

      new.info <- magick::image_info(img.new)
      w <- new.info$width
      h <- new.info$height
      j <- round(h/split)
      for(s in 1:split){
        offset <- j * s - j
        img.s <- magick::image_crop(img.new, geometry = w %+% "x" %+% j %+% "+0+" %+% offset)
        img.s <- magick::image_normalize(img.s)
        name <- gsub("\\.[tT][iI][fF][fF]?", "-" %+% s %+% ".tiff", file)
        magick::image_write(img.s, path = paste0("out/", name))
      }

    }else{

      magick::image_write(img.new, path = paste0("out/", file))
    }

    file.copy(from = file, to = paste0("out/raw/", file))
    file.remove(file)
  }

  setwd(oldwd)
}
