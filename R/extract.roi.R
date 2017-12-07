
#' Extract an ROI from an image timeseries using a mask. Expects an image in the format of NIFTI file. If it's not, we get trouble.
#' @param img.ts a 4D image timeseries from which to extract the mask
#' @param roi.mask the roi mask to use to extract the file.
#' @export
#' @examples
#' image.filename <- system.file("extdata", "haxby2001subj1bold.nii.gz", package = "mvpa")
#' mask.filename <- system.file("extdata", "haxby2001subj1mask.nii.gz", package = "mvpa")
#' fmri.image <- read.image(image.filename)
#' mask.image <- read.image(mask.filename)
#' roi.data <- extract.roi(fmri.image,mask.image)
#' summary(roi.data)
extract.roi <- function(img.ts,roi.mask){
  masked.image <- matrix(img.ts[roi.mask>0],c(sum(roi.mask>0),dim(img.ts)[4]))
  return(masked.image)
}