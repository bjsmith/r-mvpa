library(mvpa)
#install.packages("oro.nifti")
#install.packages("FactoMineR")
#install.packages("fmri")
library(FactoMineR)

#load mask
mask.img <- read.image(file = "/../mnt/data/haxby-2001-set/mask_cat_select_vt.nii") 

summary(mask.img)


#go through all the files and read them into a list of files
files = list()
for (n in 1:10){  
  test.path <- paste0("/../mnt/data/haxby-2001-set/haxby8_r",as.character(n),".nii")
  if(file.exists(test.path)){
    files <- c(files, test.path)
  }
}
getwd()
#now we know which files we want! Let's see if they can be loaded
images <- as.list(rep(NA,length(files)))
for (i in 1:length(files)){
  print(paste0("Loading ", i, " into memory"))
  images[[i]] <- read.image(file = files[[i]]) 
}

#we could multiply
product <- images[[1]][, , , 1]*mask.img
#then convert back into a nifti format.
product.img.nifti <- as(array(product,c(64,64,40)),"nifti")

#look! It looks close to the mask
image(product.img.nifti)
image(mask.img)
product.img.nifti[product.img.nifti>0]
images[[1]][, , , 1][mask.img>0]

#or, for more efficiency, we could just use the mask to select the appropriate voxels and train on that.
mask.voxels.only <- as.matrix(images[[1]],nrow = prod(dim(mask.img)), ncol = dim(images[[1]])[4])
mask.voxels.only <- as.array(images[[1]],c((dim(mask.img)),dim(images[[1]])[4]))
mask.voxels.only <- as.array(images[[1]],c(prod(dim(mask.img)),dim(images[[1]])[4]))
mask.voxels.only <- as.array(images[[1]],c(163840,121))
length(mask.voxels.only)

#this is the easiest way
myval <- images[[1]][mask.img>0]
myval[1:577]

images[[1]][, , , 2][mask.img>0]
myval[577+1:577]

#convert into a matrix of vectors, one for each volume
masked.series <- matrix(images[[1]][mask.img>0],c(577,dim(images[[1]])[4]))

#these can be placed back into the overall image using the mask we used to create it.

#now to do this systematically for all the images on hand...

images.masked <- lapply(images,function(img){
  masked.image <- matrix(img[mask.img>0],c(sum(mask.img>0),dim(images[[1]])[4]))
  return(masked.image)
})