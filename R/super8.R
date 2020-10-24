#' Adjust Frame Rate of Super8 Scans
#'
#' @param dir Directory
#' @param negate A logical. Toggles whether to negate images.
#' @export
super8_18fps <- function(dir = getwd(), negate = TRUE){

  files <- list.files(dir, pattern = "(.mp4)|(.MP4)", full.names = TRUE)
  for(file in files){

    outfile <- paste0(gsub("(.mp4)|(.MP4)", "", basename(file)), "-18fps.mp4")
    system(paste0("MP4Box -add ", file, "#video -raw 1 -new temp"))
    system(paste0("MP4Box -add temp_track1.h264:fps=18 -new ", outfile))
    if(negate){
      negfile <- paste0(gsub("(.mp4)|(.MP4)", "", basename(outfile)), "-18fps-neg.mp4")
      system(paste0("ffmpeg -i ", outfile, " -vf negate ", negfile))
    }
    system("rm temp")
    system("rm temp_track1.h264")
  }
}
