###################################################################################################################################################################
# 29 January 2014.
# R code written by Joset A. Etzel (jetzel@artsci.wustl.edu, mvpa.blogspot.com) to perform an example single-subject searchlight analysis.
# This code may be adapted for personal use, provided the source is cited.
# The sample image (dataset.nii.gz) is at https://dl.dropboxusercontent.com/u/13098670/dataset.nii.gz
###################################################################################################################################################################
# make 3d.rds: object with a 3d array (matching the size of the input image) of numbered voxels.
# the input image is 4d, with brain voxels nonzero. They have already been preprocessed.
library(mvpa)

library(oro.nifti); 
rm(list=ls());   # clear R's memory

searchlight.demo.path <- "testcode/searchlight_demo/"
img.path <- searchlight.demo.path;   # read in the image to get its size
out.path <- searchlight.demo.path;  # will write 3d.rds into this directory

img <- readNIfTI(paste0(img.path, "dataset.nii.gz"));   # read in one of the nifti images - they are all preprocessed and the same size.
dim(img);   # get the dimensions of the input images.
# [1] 41 48 35 20

inds <- which(img[,,,1] != 0, arr.ind=TRUE);  # find all the brain voxel coordinates
if (ncol(inds) != 3) { stop('wrong dims on inds'); }

out <- array(0, dim(img[,,,1]));  # make a blank 3d matrix 'brain', all zeros
out[inds] <- 1:nrow(inds);  # and put integers into the voxel places
saveRDS(out, file=paste0(out.path, "3d.rds"));   # write out as an R object.

###################################################################################################################################################################
# make the look-up table; one row for each voxel in the brain. just find each voxel, then look at the ones above/below, etc. and get the voxel numbers.
# this code makes Jo-style iterative-shell searchlights (http://mvpa.blogspot.com/2012/09/my-searchlight-and-some-final-thoughts.html). The code is not particularly
# optimized, and can take some hours to run for small voxels and/or large radii. I haven't bothered improving it, since it only needs to be run once.
# Rewrite the getAdjacentVoxels function for different shapes.

rm(list=ls());

getAdjacentVoxels <- function(x,y,z) {
  # return the indicies of the 18 voxels in a one-voxel-radius (including edges) of the voxel at x,y,z
  # tests (x>1, etc) are in case the surround overlaps the edge of the volume.
  surrounds <- rep(0, 18);
  
  # voxels at same level as center voxel
  if (y > 1) { surrounds[1] <- newVol[x, (y-1), z]; }
  if (y < maxY) { surrounds[2] <- newVol[x, (y+1), z]; }
  if (x < maxX & y > 1) { surrounds[3] <- newVol[(x+1), (y-1), z]; }
  if (x < maxX & y < maxY) { surrounds[4] <- newVol[(x+1), (y+1), z]; }
  if (x < maxX) { surrounds[5] <- newVol[(x+1), y, z]; }
  if (x > 1 & y < maxY) { surrounds[6] <- newVol[(x-1), (y+1), z]; }
  if (x > 1 & y > 1) { surrounds[7] <- newVol[(x-1), (y-1), z]; }
  if (x > 1) { surrounds[8] <- newVol[(x-1), y, z]; }

  # voxels on the layer above the center
  if (z < maxZ) { surrounds[9] <- newVol[x, y, (z+1)]; }
  if (y > 1 & z < maxZ) { surrounds[10] <- newVol[x, (y-1), (z+1)]; }
  if (y < maxY & z < maxZ) { surrounds[11] <- newVol[x, (y+1), (z+1)]; }
  if (x > 1 & z < maxZ) { surrounds[12] <- newVol[(x-1), y, (z+1)]; }
  if (x < maxX & z < maxZ) { surrounds[13] <- newVol[(x+1), y, (z+1)]; }
  
  # and below the center
  if (y < maxY & z > 1) { surrounds[14] <- newVol[x, (y+1), (z-1)]; }
  if (z > 1) { surrounds[15] <- newVol[x, y, (z-1)]; }
  if (y > 1 & z > 1) { surrounds[16] <- newVol[x, (y-1), (z-1)]; }
  if (x > 1 & z > 1) { surrounds[17] <- newVol[(x-1), y, (z-1)]; }
  if (x < maxX & z > 1) { surrounds[18] <- newVol[(x+1), y, (z-1)]; }

  return(surrounds);
}

#test: getAdjacentVoxels(50,30,20)

path <- searchlight.demo.path;  # where 3d.rds is, and where we'll write the 'lookup' file.
need.radius <- 2;  # the voxel radius we want in the searchlights, as an integer.

# get the number of voxels and dimensionality from 3d.rds. This code requires zero for non-brain voxels and integers for brain voxels.
newVol <- readRDS(paste0(path, "3d.rds"));
num.roi.vox <- max(newVol);
maxX <- dim(newVol)[1];
maxY <- dim(newVol)[2];
maxZ <- dim(newVol)[3];
inds <- which(newVol != 0, arr.ind=TRUE); 
if (dim(inds)[1] != num.roi.vox) { stop("dim(inds)[1] != num.roi.vox"); }

# start filling up the lookup list: the adjacent voxels for each non-zero voxel in 3d.rds
lookup <- vector(mode="list", length=num.roi.vox);  # list that will hold the surrounds for each center voxel
prevVoxel <- 0;   # so can check the ordering
#add some time-managed monitoring
timer.last.message <- proc.time()
for (i in 1:num.roi.vox) {    # i <- 1;
  if ((proc.time()- timer.last.message)[["elapsed"]]>10){
    timer.last.message <<- proc.time()
    print(paste0("running iteration ", i))
  }
  
  doneRadius <- 1;  # control making the searchlights bigger
  X <- inds[i, 1];
  Y <- inds[i, 2];
  Z <- inds[i, 3];
  if (newVol[X, Y, Z] == 0) { stop("got a zero!"); }  # this should only iterate through the brain voxels
  thisVoxel <- newVol[X, Y, Z];
  if (thisVoxel != (prevVoxel + 1)) { stop("voxels aren't in order"); }
  while (doneRadius <= need.radius) {
    svox <- lookup[[thisVoxel]];  # surrounding voxels for this one, if some already calculated (if radius > 1)
    if (length(which(svox != 0)) > 0) {  
      sinds <- which(svox != 0)
      for (si in sinds) {   # si <- 1;
        searchvox <- which(newVol == svox[si], arr.ind=TRUE);
        if (dim(searchvox)[1] != 1) { stop("dim(searchvox)[1] != 1"); }
        # add the surrounds for this surrounding voxel to the original center; use union so don't get lots of duplicates
        lookup[[thisVoxel]] <- union(lookup[[thisVoxel]], getAdjacentVoxels(searchvox[1,1], searchvox[1,2], searchvox[1,3]));
      }
    } else { 
      lookup[[thisVoxel]] <- getAdjacentVoxels(X,Y,Z);  # get the 18 voxels in a one-voxel radius of X,Y,Z (radius 1 searchlight)
    }
    doneRadius <- doneRadius + 1;
  }
  # lookup[[thisVoxel]] # shows voxels in the 'surround' for this center voxel. 0s mean the searchlight is on the edge, containing non-brain voxels.
  prevVoxel <- thisVoxel;
}
# turn the list into an array, then save as a RDS object
biggest <- 1;  # find the most surrounding voxels of any searchlight, so can set the number of columns correctly.
for (i in 1:num.roi.vox) {    # i <- 1;
  lookup[[i]] <- lookup[[i]][which(lookup[[i]] > 0)];
  if (length(lookup[[i]]) > biggest) { biggest <- length(lookup[[i]]); }
}
outtbl <- array(NA, c(num.roi.vox, biggest));
for (i in 1:num.roi.vox) { 
  if (length(lookup[[i]]) > 0) { outtbl[i,1:length(lookup[[i]])] <- lookup[[i]]; }
}
saveRDS(outtbl, paste0(path, "lookup_radius", need.radius, ".rds"));

###############################################################################################################################################################
# do the searchlight analysis. Since this is a demo, it runs a very simple cross-validation scheme - not recommended for actual analyses!
# this code runs the searchlight analysis in four "chunks": each chunk is a separate part of the brain. I run each chunk as a separate job on a supercomputing
# cluster, then collect and combine the individual output files. Obviously, how the paths and arguments are set will vary with cluster computer.
# each chunk takes around an hour to run on my local machine; code speed-ups are probably possible.

library(oro.nifti); 
library(e1071);   # for the linear svm


#rm(list=ls()); on.cluster <- TRUE; 
rm(list=ls()); on.cluster <- FALSE; 
searchlight.demo.path <- "testcode/searchlight_demo/"

if (on.cluster == TRUE) {   # get the chunk from the argument sent to R when the job was started.
  cA <- commandArgs();
  do.chunk  <- as.numeric(cA[5]);   # chunk to do in this job, sent as an argument when R was started

  img.path <- "searchlight.demo.pathscratch/jetzel/searchlightDemo/";   # the nii.gz
  spot.path <- "searchlight.demo.pathscratch/jetzel/searchlightDemo/";  # 3d.rds and lookup_radius2.rds directory
  out.path <- "searchlight.demo.pathscratch/jetzel/searchlightDemo/";   # write into here
} else {   # not on the cluster, so set the chunk here.
  img.path <- searchlight.demo.path;   # the nii.gz
  spot.path <- searchlight.demo.path;  # 3d.rds and lookup_radius2.rds directory
  out.path <- paste0(searchlight.demo.path, "output/");   # write into here
  
  do.chunk <- 1;  # four total: which part of the brain to run
}

radius <- 2;   # searchlight radius

lookup <- readRDS(paste0(spot.path, "lookup_radius", radius, ".rds"));  # searchlight surrounds
vol3d <- readRDS(paste0(spot.path, "3d.rds"));  # for going from voxel numbers to 3d coordinates.
all.img <- readNIfTI(paste0(img.path, "dataset.nii.gz"));  # first 10 class 'a', second 10 class 'b'
class.key <- c(rep("a", 10), rep("b", 10));  # vector of class labels

# figure out which voxels to run in this chunk
num.vox <- nrow(lookup);
if (max(vol3d) != num.vox) { stop("max(vol3d) != num.vox"); }
all.chunks <- seq(from=1, to=num.vox, by=4500); # 4 chunks
if (do.chunk > length(all.chunks)) { stop("do.chunk > length(all.chunks)"); }
if (do.chunk == length(all.chunks)) { do.centers <- all.chunks[do.chunk]:num.vox; }
if (do.chunk < length(all.chunks)) { do.centers <- all.chunks[do.chunk]:(all.chunks[do.chunk+1] - 1); }
if (length(class.key) != dim(all.img)[4]) { stop("number of labels doesn't match number of images"); }

# now do the classification for all the searchlights in this chunk.
out.img <- array(NA, dim(vol3d));
timer.last.message <- proc.time()
for (v in do.centers) {  # v <- do.centers[1];
  #progress based on a timer rather than having completed a set number.
  if ((proc.time()- timer.last.message)[["elapsed"]]>10){
    timer.last.message <<- proc.time()
    print(paste("at", v, "of", length(do.centers)))
  }
    
  #if (v%%500 == 0) { print(paste("at", v, "of", length(do.centers))); }   # print a message to show progress
  # find which voxels belong in this center voxel's searchlight
  voxs <- union(v, unlist(lookup[v,], use.names=FALSE));   # surrounding voxels for this center; union to put center in the searchlight
  voxs <- voxs[which(!is.na(voxs))];  # get rid of NAs. will be NA entries if some surrounding voxels not in the brain.
  if (length(voxs) > 2) {    # how many surrounding voxels must be in the searchlight? Smaller ones (edge of brain) will be skipped.
    # put the data into a matrix so can classify
    thisData <- array(NA, c(length(class.key), length(voxs)));  # images in the rows (voxels in this searchlight only), voxels in the columns
    for (i in 1:length(voxs)) {   # i <- 1;
      coords <- which(vol3d == voxs[i], arr.ind=TRUE);
      if (ncol(coords) != 3 | nrow(coords) != 1) { stop("wrong sized coords"); }
      thisOne <- all.img[coords[1], coords[2], coords[3],]
      if (sd(thisOne) > 0) { thisData[,i] <- thisOne; } else { stop("zero variance voxel"); } 
    }
    #thisData <- t(scale(t(thisData)));  # slow row-scaling ...
    
    # do the cross-validation and classification.
    # This is a stupidly simple cross-validation: leave out first of each class, then second, etc. Not recommended for real analyses!
    a.inds <- which(class.key == "a");
    b.inds <- which(class.key == "b");
    if (length(a.inds) != length(b.inds)) { stop("imbalance in a and b"); }
    accs <- rep(NA, length(a.inds));  # leave-one-pair-out cross-validation
    for (i in 1:length(a.inds)) {   # i <- 1;
      test.inds <- c(a.inds[i], b.inds[i]);
      train.inds <- (1:length(class.key))[-test.inds]
      
      if (length(union(train.inds, test.inds)) != nrow(thisData)) { stop("length(union(train.inds, test.inds)) != nrow(thisData)"); }
      if (length(train.inds) < length(test.inds)) { stop("length(train.inds) > length(test.inds)"); }
      
      # do the linear svm
      fit <- svm(class.key[train.inds]~., data=thisData[train.inds,], type="C-classification", kernel="linear", cost=1, scale=FALSE);  # train
      tree <- table(class.key[test.inds], predict(fit, thisData[test.inds,]));   # test
      if (nrow(tree) == 1 | ncol(tree) == 1) { accs[i] <- 0.5; } else { accs[i] <- sum(diag(tree))/sum(tree); }  # calculate proportion correct
    }
    coords <- which(vol3d == v, arr.ind=TRUE);   # find the coordinates of this searchlight center
    if (ncol(coords) != 3 | nrow(coords) != 1) { stop("wrong sized coords"); }
    out.img[coords[1], coords[2], coords[3]] <- mean(accs);   # store the mean accuracy in the correct place
  }
}
writeNIfTI(nifti(out.img, datatype=64, pixdim=c(1,4,4,4,1,1,1,1)), paste0(out.path, "chunk", do.chunk, "_rad", radius, "_slOut"));  # save the accuracies


###############################################################################################################################################################
# combine the chunk files into a single whole-brain output file

library(oro.nifti); 
rm(list=ls()); 
searchlight.demo.path <- "testcode/searchlight_demo/"

in.path <- paste0(searchlight.demo.path,"output/");
out.path <- paste0(searchlight.demo.path,"output/");

radius <- 2;   # searchlight radius
num.chunks <- 1;   # lazy hard-coding
img.dim <- c(41,48,35);

out.fname <- paste0(out.path, "searchlightAccuracies_rad", radius);
if (!file.exists(paste0(out.fname, ".nii.gz"))) {   # check so don't overwrite existing files
  ctr <- 0;
  allImg <- array(0, img.dim); 
  for (i in 1:num.chunks) {    # i <- 1;   # first, check to see if all chunk files are present
    fname <- paste0(in.path, "chunk", i, "_rad", radius, "_slOut.nii.gz")
    if (!file.exists(fname)) { 
      print(paste("missing:", fname)); 
    } else {
      inimg <- readNIfTI(fname, reorient=FALSE);
      if (dim(inimg)[1] != img.dim[1] | dim(inimg)[2] != img.dim[2] | dim(inimg)[3] != img.dim[3]) { stop("not expected dims"); }
      inds <- which(inimg != 0, arr.ind=TRUE);  # get the nonzero voxel locations: make sure don't have overlaps before adding!
      if (max(allImg[inds]) != 0 | min(allImg[inds])!= 0) { stop("overlapping chunks"); }  # stop if overlaps.
      allImg[inds] <- allImg[inds] + inimg[inds]; 
      ctr <- ctr + 1;
    }
  }
  if (ctr == num.chunks) {
    writeNIfTI(nifti(allImg, datatype=64, pixdim=c(1,4,4,4,1,1,1,1)), out.fname);
    # this code will delete the chunk files if made the output file
    # if (file.exists(paste0(out.fname, ".nii.gz"))) { 
    #   for (i in 1:num.chunks) {    # i <- 1;
    #     fname <- paste0(in.path, "chunk", i, "_rad", radius, "_slOut.nii.gz")
    #     msg <- file.remove(fname)
    #     if (msg == FALSE) { stop(paste("didn't remove", fname)); }
    #   }
    # }
  } 
}

###############################################################################################################################################################
# now, we have a single 3d nifti image in which each voxel value is the accuracy of the voxel's searchlight.
# this sample dataset is for a single participant; if there were multiple participants we'd do group-level statistics with these images.
###############################################################################################################################################################

accuracies<-read.image(out.fname)
display.3D.img.in.2D(accuracies,3)
#

