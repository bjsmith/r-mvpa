
#' Display a 2d image on screen.
#' @export
#' @examples
#' display.2D.img(array(rnorm(100^2),dim=c(100, 100)))

display.2D.img <- function(ds){
  minval <- min(ds,na.rm=T)
  range <- max(ds,na.rm=T) - min(ds,na.rm=T)
  ds.scaled <- (ds-minval)/range
  max.img.side<-max(dim(ds)[1:2])
  plot(c(1, max.img.side+1), c(1, max.img.side+1), type = "n", xlab = "col (voxel)", ylab = "row (timepoint)")
  rasterImage((ds.scaled), 1, 1, dim(ds.scaled)[2]+1, dim(ds.scaled)[1]+1, interpolate = FALSE)
}