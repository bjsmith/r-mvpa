library(mvpa)
#install.packages("oro.nifti")
#install.packages("FactoMineR")
#install.packages("fmri")
#install.packages("R.matlab")
library(R.matlab)
library(FactoMineR)
library(MASS)
library(class)
#install.packages("cvTools")
library(cvTools)

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

#take each image series, mask it, and then put it into a matrix where rows represent the masked voxels and columns represent time.
#to put these back into 4D, we'd need to use the mask image to select the sum(mask.img>0) values to put back in.
#cool!
#images.masked <- lapply(images,function(img){
#  masked.image <- matrix(img[mask.img>0],c(sum(mask.img>0),dim(images[[1]])[4]))
#  return(masked.image)
#})

#take each image series, mask it, and then put it into a matrix where rows represent time and the columns represent the masked voxels.
#this will require some transposition
#to put these back into 4D, we'd need to use the mask image to select the sum(mask.img>0) values to put back in.
#and will need to transpose again
#cool!
images.masked <- lapply(images,function(img){
  masked.image <- t(matrix(img[mask.img>0],c(sum(mask.img>0),dim(images[[1]])[4])))
  #we transpose because the general R convention, set e.g., by data frames,
  #is that each row is an observation
  #and each column is a variable
  return(masked.image)
})

#OK, now for each of the 10 samples we need to classify each of these images as:
#train or test
#one of 8 classes or rest data.

#how do we know which images in the sereis are of each class, so that we can train?
#let's read the .mat files in the dir with the haxby data and see what's in there.

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

#get the run codes and categories of each ite
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
category.ts <- apply(tutorial.regs$regs,2, function(x){
  x.cat <- which(x==1)
  if(length(x.cat)==0){
    return(NA)
  }else{
    return(x.cat)
  }
})

run.ts <- array(unlist(tutorial.runs$runs))

#so to recap, did we apply the mask before?
dim(images.masked[[1]])
#since our category vector is 1210-item long, concatenated list of (we assume consecutive) runs,
#we should try to get it into that format :-D
length(category.ts)
class(images.masked[[1]])

#check all matrices have the same number of cols (i.e., variables, voxels)
img.series.cols = unlist(lapply(images.masked,function(x){return(dim(x)[2])}))
#and check the total length of all observation, datapoints.
img.series.row.sum = sum(unlist(lapply(images.masked,function(x){return(dim(x)[1])})))

if (length(unique(img.series.cols))>1) stop("SOME RUNS APPEAR TO HAVE A DIFFERING NUMBER OF VOXELS MASKED.")

#create a matrix to populate with a concatenated timeseries of masked images 
images.masked.concat = matrix(nrow=img.series.row.sum,ncol=img.series.cols[1])
#now populate images.masked.concat
#we won't assume that every image has the same number of timepoints.
row.index <-1
for (i in 1:length(images.masked)){
  image.i.endpos <- row.index + dim(images.masked[[i]])[1]-1
  images.masked.concat[row.index:image.i.endpos,] <- images.masked[[i]]
  row.index <- image.i.endpos + 1  
}

#display the dataset
display.ds <- function(ds){
  minval <- min(ds,na.rm=T)
  range <- max(ds,na.rm=T) - min(ds,na.rm=T)
  ds.scaled <- (ds-minval)/range
  max.img.side<-max(dim(ds))
  plot(c(1, max.img.side+1), c(1, max.img.side+1), type = "n", xlab = "col (voxel)", ylab = "row (timepoint)")
  rasterImage((ds.scaled), 1, 1, dim(ds.scaled)[2]+1, dim(ds.scaled)[1]+1, interpolate = FALSE)
}

#PREPROCESSING
#now for this dataset, we should:
#-subtract the mean *image* of each run from all of the images in that run,
# because we assume that runs are counterbalanced
#image.timeseries<-images.masked.concat
#run.timeseries <- run.ts
subtract.mean.image.by.run <- function(image.timeseries,run.timeseries){
  image.timeseries.mc <- matrix(nrow=dim(image.timeseries)[1],ncol=dim(image.timeseries)[2])
  #r=1
  for (r in unique(run.timeseries)){
    r.timepoints <- which(run.timeseries==r)
    dim(image.timeseries[r.timepoints,])
    run.mean.image <- colMeans(image.timeseries[r.timepoints,])
    #print(length(run.mean.image))
    image.timeseries.mc[r.timepoints,] <- t(t(image.timeseries[r.timepoints,]) - run.mean.image)
    #display.ds(image.timeseries.mc)
    #sweep(image.timeseries,r.timepoints,#seems like it could a very good function to use, but we don't need it..
    warning("run.mean.image is a double matrix. For slight loss in precision but less memory usage, it could be converted into an integer matrix.")
  }
  return(image.timeseries.mc)
}
images.masked.concat.mc <- subtract.mean.image.by.run(images.masked.concat,run.ts)
display.ds(images.masked.concat.mc)

#first - what are the distributional properties of the values in each category?
#to classify we should probably know how much the variances vary by each variable.
hist(apply(images.masked.concat.mc,2,sd),breaks=40)
length(apply(images.masked.concat.mc,2,sd))
#this is a fairly wide range of variances.
#they should probably be normalized.


# (same number of samples in each run, so that's reasonable)
#-then subtract the mean voxel from all voxels in each image, 
# because we assume that what's most important is the *relative* activity of each voxel.
#though I don't know if this is all that necessary now; looking at the dataset there appears to be as much variability across voxels as within them.
#image.timeseries<-images.masked.concat.mc
#http://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
mean.center.each.image <- function(image.timeseries){
  image.timeseries.mc <- matrix(nrow=dim(image.timeseries)[1],ncol=dim(image.timeseries)[2])
  #get the mean voxel from all voxels in each image
  mean.voxel.by.image <- rowMeans(image.timeseries)
  length(mean.voxel.by.image)
  #and subtract
  image.timeseries.mc <- image.timeseries - mean.voxel.by.image
  warning("you may want to normalize each voxel, but here, we're only demeaning them.")  
  #now check that we did that right...
  vox=500:510
  tp=54
  image.timeseries.mc[tp,vox]+mean.voxel.by.image[tp]
  image.timeseries[tp,vox]
  return(image.timeseries.mc)
}
#this might be superfluous - do we need to mean center each image?

images.masked.concat.mc2 <- mean.center.each.image(images.masked.concat.mc)

#what we WILL do is now normalize each voxel
normalize.each.voxel <- function(image.timeseries){
  image.timeseries.normed <- t((t(image.timeseries) - colMeans(image.timeseries)) / apply(image.timeseries,2,sd))
  return(image.timeseries.normed)
}
images.masked.concat.normed <- normalize.each.voxel(images.masked.concat.mc2)

display.ds(images.masked.concat.normed)
images.preprocessed <- images.masked.concat.normed

#NOW...finally...we can start some training!!!!
#let's just grab the stuff that's in a category.
#set up the data in a format we can use to train. We might have to put it into a single data frame but let's try to avoid a data frame for now.
image.rc <- images.preprocessed[!is.na(category.ts),]
category.rc <- category.ts[!is.na(category.ts)]
runs.rc <- run.ts[!is.na(category.ts)]
set.seed(42) #select a randomly determined 50% of the dataset to test on. 
train.set.indices <- sample(1:5,5)
test.set.indices <- which(!(1:10 %in% train.set.indices))

training.obs <- runs.rc%in% train.set.indices
testing.obs <-runs.rc%in% test.set.indices 

#wraps all the testing in a function that is called from the cross validator to get overall results.
evaluate.model <- function(training.obs, testing.obs){
  res <-list()
  train.vars<-image.rc[training.obs,]
  test.vars<-image.rc[testing.obs,]
  train.targets <- category.rc[training.obs]
  test.targets <- category.rc[testing.obs]
  
  print("LDA Result:")
  test.res <- lapply(
    c('moment','mle'),function(da.method){
    print(paste0("Trying LDA with",da.method))
    lda.fit <- lda(train.vars, train.targets, method = da.method)
    lda.predict <- predict(lda.fit, train.vars)
    train.train.success <- data.frame("predictions" = lda.predict$class, "ground.truth" = train.targets, "success" = lda.predict$class==train.targets)
    lda.test.predict <- predict(lda.fit, test.vars)
    train.test.success <- data.frame("predictions" = lda.test.predict$class, "ground.truth" = test.targets, "success" = lda.test.predict$class==test.targets)
    lda.success = mean(train.test.success$success)
    return(lda.success)
  })
  res$LDA.moment <- test.res[[1]]
  res$LDA.mle <- test.res[[2]]
  
  #OK so not resounding success with LDA.
  
  #We could try a few different methods.
  
  #QDA
  tryCatch({
  qda.fit <- qda(train.vars, train.targets, method = 'mle')
  qda.predict <- predict(qda.fit, train.vars)
  train.train.success <- data.frame("predictions" = qda.predict$class, "ground.truth" = train.targets, "success" = qda.predict$class==train.targets)
  lda.test.predict <- predict(lda.fit, test.vars)
  train.test.success <- data.frame("predictions" = qda.test.predict$class, "ground.truth" = test.targets, "success" = qda.test.predict$class==test.targets)
  qda.success = mean(train.test.success$success)
  res$QDA <<- qda.success
  }, warning = function(war){
    print(war)
  }, error = function(err){
    print(err)
  })
  #could also use...what's the package for trying out different values?
  #can use:
  #apply
  #tune (e1071 package)
  #caret http://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/
  
  
  #for now, apply will do :-)
  
  #KNN
  
  knn.success <- lapply(c(1:8),function(k.val){
    set.seed(1)
    knn.pred <- knn(train.vars,test.vars, train.targets,k=k.val)
    table(knn.pred,test.targets)
    return(sum(diag(table(knn.pred,test.targets)))/sum(table(knn.pred,test.targets)))
  })
  res$KNN <- knn.success
  
  return(res)
}
evaluate.model(training.obs,testing.obs)

#and...what data manipulations could we try?
#could probably legitimately try taking the average from each category/block.
#and could also try putting them together into consecutive training bits.

#OK, we should be doing k-fold cross validation here if we're comparing models...
#vals <- cvFolds(dim(image.rc)[1],K=8,type="consecutive") #ONLY works because the data is stored consecutively.

#let's try this.
#method is a function which we call to do the kfold cross validation.
#folds is vector that describes which fold each data point belongs to.
do.kfold <- function(method,folds){
  #method=evaluate.model,folds=runs.rc
  res.list <- list()
  res.values <- list()
  res.byfold=list()
  #f=1
  for (f in unique(folds)){
    print(paste0("KFold Excluding fold ",f))
    #for each fold, exclude it and use all the others as the training data, and then use f as the test data.
    training.obs <- folds!=f
    testing.obs <-folds==f
    res<-method(training.obs,testing.obs)
    res.byfold[[f]]=res
    res.values=unique(c(res.values,names(res)))
  }
  #take the average of each value in res
  #cycle through all the values found.
#   for (res.value in res.values){
#     res.list[[res.value]] <- mean(
#       unlist(lapply(res,function(x){
#       return(res[[res.value]])
#     })))
#     }
#   return(list("res.list" = res.list,"res.byfold" = res.byfold))
  return(res.byfold)
}
res.kfold  = do.kfold(evaluate.model,runs.rc)


#So none of these method perofmr marvelously, and to add suspicion, why did we got from 66 to 55 % accuracy?

