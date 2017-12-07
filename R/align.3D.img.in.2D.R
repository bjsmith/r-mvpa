#' Display a 3d image on screen in 3D by tiling it.
#' @param img the image to put into 3D
#' @param dimension the dimension to reduce across
#' @export
#' 
align.3D.img.in.2D <- function(img = NULL ,dimension=1){
  dtm=dimension
  #do the first dimension
  dim.1 <- c((dtm+3) %% 3+1,(dtm+4) %% 3+1,dtm)
  
  #display.2D.img(array(img.average,dim=c(40,64*8,64/8))[, , 4])
  
  
  a2 = aperm(img,dim.1)
  #OK now we've rearranged the matrix we know what order we want to manipulate the values.
  #get the sqrt of the last dimension, and the
  
  cx=ceiling(sqrt(dim(a2)[3]))
  cy=ceiling(dim(a2)[3]/cx)
  #now we might need to add some empty rows onto this to make it work.
  #install.packages("abind")
  #require(abind)
  #add blank images so we have an array which can be shaped evently
  a3 <- abind(a2,array(0,dim=c(dim(a2)[1:2],cx*cy-dim(a2)[3])))
  dim(a3)
  
  #now compress along the first dimension
  a4 <- array(a3,dim=c(dim(a3)[1],dim(a3)[2]*cy,dim(a3)[3]/cy))
  #display.2D.img(a4[, , 3])
  a5 <- aperm(a4,c(2,1,3))
  dim(a5)
  a6 <- array(a5,dim=c(dim(a5)[1],dim(a5)[2]*cx))
  #display.2D.img(a6)
  return(a6)  
}