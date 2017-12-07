#' Apply preprocessing stuff.
#' 
#' @param x fmri timeseries
#' @param run list of the run objects
#' @export
prepropress.fmri.run.set <- function(x,run){
  display.2D.img(x)
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
      #dim(image.timeseries[r.timepoints,])
      run.mean.image <- colMeans(image.timeseries[r.timepoints,])
      #print(length(run.mean.image))
      #print(r.timepoints)
      image.timeseries.mc[r.timepoints,] <- t(t(image.timeseries[r.timepoints,]) - run.mean.image)
      #display.2D.img(image.timeseries.mc)
      #sweep(image.timeseries,r.timepoints,#seems like it could a very good function to use, but we don't need it..
      
    }
    warning("run.mean.image is a double matrix. For slight loss in precision but less memory usage, it could be converted into an integer matrix.")
    return(image.timeseries.mc)
  }
  x2 <- subtract.mean.image.by.run(x,run)
  #display.2D.img(x2)
  
  #first - what are the distributional properties of the values in each category?
  #to classify we should probably know how much the variances vary by each variable.
  #hist(apply(x2,2,sd),breaks=40)
  #length(apply(x2,2,sd))
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
    #now check that we did that right...
    #vox=500:510
    #tp=54
    #print(image.timeseries.mc[tp,vox]+mean.voxel.by.image[tp]
    #image.timeseries[tp,vox]
    return(image.timeseries.mc)
  }
  #this might be superfluous - do we need to mean center each image?
  
  x3 <- mean.center.each.image(x2)
  
  #what we WILL do is now normalize each voxel
  normalize.each.voxel <- function(image.timeseries){
    image.timeseries.normed <- t((t(image.timeseries) - colMeans(image.timeseries)) / apply(image.timeseries,2,sd))
    return(image.timeseries.normed)
  }
  x4 <- normalize.each.voxel(x3)
  
 # display.2D.img(x4)
  return(x4)
  
}