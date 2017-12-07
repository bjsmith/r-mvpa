library(mvpa)
#install.packages("oro.nifti")
#install.packages("FactoMineR")
#install.packages("fmri")
#install.packages("R.matlab")
library(R.matlab)
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

product.img.nifti <- as(array(product,c(64,64,40)),"nifti")


#take each image series, mask it, and then put it into a matrix where rows represent the masked voxels and columns represent time.
#to put these back into 4D, we'd need to use the mask image to select the sum(mask.img>0) values to put back in.
#cool!
images.masked <- lapply(images,function(img){
  masked.image <- matrix(img[mask.img>0],c(sum(mask.img>0),dim(images[[1]])[4]))
  return(masked.image)
})

#OK, now for each of the 10 samples we need to classify each of these images as:
#train or test
#one of 8 classes or rest data.

#how do we know which images in the sereis are of each class, so that we can train?
#let's read the .mat files in the dir with the haxby data and see what's in there.
tutorial.regs <- read.table(file = "/../mnt/data/haxby-2001-set/tutorial_regs.mat")
tutorial.runs <- read.csv(file = "/../mnt/data/haxby-2001-set/tutorial_runs.mat")

#this is from a slightly different version of the file.
#http://www.pymvpa.org/datadb/haxby2001.html
#haxby had 12 runs...why is there only 10 here??

#This is where the haxby-2001-set came from:
#http://code.google.com/p/princeton-mvpa-toolbox/wiki/Downloads?tm=2
#see: http://code.google.com/p/princeton-mvpa-toolbox/wiki/TutorialIntro
#there are 10 runs of a single subject here.
#each blog contains a 9-run TR with a (1?) rest in between and in each end.

#tutorial including info tutorial_runs can be found here:
#http://code.google.com/p/princeton-mvpa-toolbox/wiki/TutorialIntro

tutorial.runs <- readMat("/../mnt/data/haxby-2001-set/tutorial_runs.mat")
tutorial.regs <- readMat("/../mnt/data/haxby-2001-set/tutorial_regs.mat")
#OK now we have the codes for each!
dim(tutorial.regs$regs)
#tutorial.regs has the codes
plot(tutorial.regs$regs[1, ])
plot(tutorial.regs$regs[2, ])
plot(tutorial.regs$regs[3, ])
length(which(tutorial.regs$regs[3, ]==1))#OK Great this is what we want!

#we can create a vector which stores which category each image is.
category <- apply(tutorial.regs$regs,2, function(x){
  x.cat <- which(x==1)
  if(length(x.cat)==0){
    return(NA)
  }else{
    return(x.cat)
  }
  })

#so this might be better to work with, it's all the subjects concatenated together.
#actually no, it's only 2 subjects' worth of data..
concat.hax <- read.image(file = "/../mnt/data/haxby-2001-set/concat_hax.nii")
summary(concat.hax)

concat.hax <- read.image(file = "/../mnt/data/haxby-2001-set/concat_hax.nii")
summary(concat.hax)

#should apply mask to htis like we applied to the others
img.ts <- concat.hax
roi.ts <- extract.roi(img.ts=concat.hax,mask.img)
dim(img.ts)
dim(images[[1]])

#so to recap, did we apply the mask before?
dim(images.masked[[1]])
lapply(images.masked,summary)
#since our category vector is 1210-item long, concatenated list of (we assume consecutive) runs,
#we should try to get it into that format :-D
length(category)
class(images.masked[[1]])

#check all matrices have the same number of rows (observations/voxels)
img.series.rows = unlist(lapply(images.masked,function(x){return(dim(x)[1])}))
img.series.col.sum = sum(unlist(lapply(images.masked,function(x){return(dim(x)[2])})))

if (unique(img.series.rows)>1) stop("SOME RUNS APPEAR TO HAVE A DIFFERING NUMBER OF VOXELS MASKED.")

images.masked.concat = matrix(nrow=img.series.rows[1],ncol=img.series.col.sum)
#now populate images.masked.concat
#we won't assume that every image has the same number of timepoints.
col.index <-1
for (i in 1:length(images.masked)){
  image.i.endpos <- col.index + dim(images.masked[[i]])[2]-1
  images.masked.concat[,col.index:image.i.endpos] <- images.masked[[i]]
  col.index <- image.i.endpos + 1  
}

#display the dataset
display.ds <- function(ds){
  max.img.side<-max(dim(ds))
  plot(c(1, max.img.side), c(1, max.img.side), type = "n", xlab = "voxel", ylab = "obs")
  rasterImage(ds, 1, 1, dim(ds)[1], dim(ds)[2], interpolate = FALSE)
}
display.ds(images.masked.concat)


#now let's practice being able to match image rows with their exemplars.
#category 1
display.ds(images.masked.concat[,which(category==1)])

#category 1, and then category 4
display.ds(images.masked.concat[,c(which(category==1),which(category==4))])


