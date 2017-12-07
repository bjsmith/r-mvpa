library(mvpa)
#install.packages("oro.nifti")
#install.packages("FactoMineR")
#install.packages("fmri")
library(FactoMineR)
#go through all the files and read them into a list of files
files = list()
for (n in 1:500){
  test.path <- paste0("/../mnt/data/GoNoGo/sub",as.character(n),".nii.gz")
  if(file.exists(test.path)){
    files <- c(files, test.path)
  }
}
#seems to be able to load about 85 datasets simultaneously. Dimensionality reduction is probably still necessary!
#243 components would be great (it seems to be the most we could get on only 243 observations)
#It would also be overfitting...so not sure how to apply across datasets!
#Just aligning various brain regions via a classification system would be a project in itself
#question is whether the information is actually in this dataset.
getwd()
#now we know which files we want! Let's see if they can be loaded
img <- read.image(
  file = files[[1]])

images <- as.list(rep(NA,length(files)))
for (i in 1:length(files)){
  print(paste0("Loading ", i, " into memory"))
  images[[i]] <- read.image(file = files[[i]]) 
}
#so we can load a selection of images but maybe no more than 5 or so at a time into memory; at least, not this way of storing the data.
#we can pick out selections in the images!
img[5,7,8,14]

#There are some options:
#--Use PCA to transform the first timeseries into its principal components (get components of space, not components of time);
#then apply the same transformation to all the other images.
#--find a way to load more stuff into memory
#What if we created one really big 4D array and just added everything into that 4-D array? Would we have more success in loading data that way?
#would have to be SUPER big.
#Some data reduction seems inevitable given the size of the data.
#So I think we should probably try that!

# FactoMineR
#
#let's try first turning image into a vector so we can precisely specify what we're trying to do.
print(img)
dim(img)
img.matrix <- matrix(img,nrow=dim(img)[4],ncol = prod(dim(img)[1:3]))

#rearrange the image into a format we might be able to input into FactoMineR
img.rearranged <- aperm(img,c(4,1,2,3))
#flatten in to 2D
img.matrix <- matrix(img.rearranged,nrow=dim(img)[4],ncol = prod(dim(img)[1:3]))

#try put it into a data frame
img.df <- data.frame(img.matrix)

#just to test we did the transformation correctly, let's grab the 32nd pixel from each space dimension for timeseries 100:120
img[32,1,1,100:120]
img.df[100:120,32]

c.x <- 33
c.y <- 13
c.z <- 26
img[c.x,c.y,c.z,100:120]
img.df[100:120,c.x+64*(c.y-1)+64*64*(c.z-1)]
img.df[100:120,c.x+64*(c.y-1+64*(c.z-1))]

img.dft <- t(img.df)
#put it through PCA
res <- PCA(img.df[,1:500],ncp=5,graph = F)
summary(res)

rest <- PCA(img.dft[1:500,],ncp=5,graph = F)
summary(rest)
#the eigenvector is going to be described in terms of its direction through the original space.
#so if we have 1000 dimensions the eigenvector needs to have 1000 items describing it.

#5 components, each described in 500 dimensions
summary(res$var)
summary(res$var$coord)
dim(res$var$coord)
res$eig
summary(res$ind)

#5 components, each described in 243 dimensions (this is the wrong interpretation.)
summary(rest$var)
dim(rest$var$coord)
summary(rest$ind)

#try princomp
res2 <- princomp(img.dft[1:500,])
loadings(res2)
summary(res2)