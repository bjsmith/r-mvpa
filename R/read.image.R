
#' Read a brain image
#' http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#' This function allows you to read a brain image very nicely
#' @param file
#' @param file.format file format the mri file is stored in. Currently only NIFTI
#' @export
#' 
read.image <- function(file,dim, file.format="NIFTI"){
  if (file.format=="NIFTI"){
    
    tryCatch({
      printv(getwd())
      image <- readNIfTI(file, reorient=FALSE)
    },error = function(err){
      catnl(paste0("While processing file ", file))
      stop(paste("Error while attempting to read the image:  ",err))
    })
    
    #package oro.nifti
#     URL <- "http://imaging.mrc-cbu.cam.ac.uk/downloads/Colin/colin_1mm.tgz"
#     urlfile <- "colin_1mm.tgz"
#     if (!file.exists(urlfile)){
#       download.file(URL, dest=urlfile, quiet=TRUE)
#       untar(urlfile)
#     }
#     img <- readNIfTI("colin_1mm")
#     img = cal_img(img)
    
    #test.file <- "../testdata/sub126-CUPSTask.nii.gz"
    #img <- readNIfTI(test.file)#Error in performPermutation(trans, real.dimensions, data, verbose) : 
                                #Transformation is not simple, cannot reorient!
    
    #img <- readNIfTI(test.file.uncompressed)#Error in performPermutation(trans, real.dimensions, data, verbose) : 
                                            #Transformation is not simple, cannot reorient!
    
    #img <- readNIfTI(test.file,reorient=FALSE)
    #works, but it's really slow working with this file.
    
    #package fmri
    #img <- read.NIFTI(file) #using package fmri
    #img2 <- read.NIFTI(filename=test.file) #"Hmm! This does not seem to be a NIFTI header (hdr/img-pair)! Wrong size or does not exist!"
    
    #package mritc
    #img <- readMRI(test.file,format= "nifti") #using package mritc
    #Error in performPermutation(trans, real.dimensions, data, verbose) : 
    #  Transformation is not simple, cannot reorient!
      #actually looks to be using the same base package as readNIfTI.
    
  }else{
    stop(paste0("read.image doesn't recognize the file format ", file.format))
  }
  return(image)
}