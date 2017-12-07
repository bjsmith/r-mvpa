library(mvpa)
my.array<-array(0,dim=c(10,11,12))
#paint borders
my.array[c(1,10), , ] <- 1
my.array[,c(1,11) , ] <- 1
my.array[, , c(1,12)] <- 1
my.array[2,4,8]=2
my.array[3,6,9]=3
my.array[4,7,10]=4

#now, isn't there an easier way to get the surrounding voxels to say,
#[7,6,5]?

getAdjacentVoxels <- function(x,y,z){
  x=7;y=6;z=5
  #first, all the voxels sharing 4 edges (a plane) with the key voxel
  c(c(x+1,x-1),y,z)
} #nah, this would end up being just as complicated as Etzel's function I think.
display.3D.img.in.2D(my.array)