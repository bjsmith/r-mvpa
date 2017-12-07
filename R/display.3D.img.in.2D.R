
#' Display a 3d image on screen in 3D by tiling it.
#' @export
#' @examples
#' data <- array(sample(1:100,10^3,replace=TRUE),c(10,10,10))
#' display.3D.img.in.2D(data,1)
display.3D.img.in.2D <- function(img,dimension=3){
  display.2D.img(align.3D.img.in.2D(img,dimension))
}