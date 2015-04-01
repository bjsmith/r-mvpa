
#' Read a brain image
#' 
#' This function allows you to read a brain image
#' @param file
#' @param file.format file format the mri file is stored in. Currently only NIFTI
#' @export
#' @examples
#' read.image()
#http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#setwd(nwd)

#require("fmri")
read.image <- function(file,dim, file.format="NIFTI"){
  if (file.format=="NIFTI"){
    #package fmri
    #img <- read.NIFTI(file) #using package fmri
    
    #package mritc
    #img <- readMRI(file, dim, "nifti") #using package mritc
    
    #package oro.nifti
    URL <- "http://imaging.mrc-cbu.cam.ac.uk/downloads/Colin/colin_1mm.tgz"
    urlfile <- "colin_1mm.tgz"
    if (!file.exists(urlfile)){
      download.file(URL, dest=urlfile, quiet=TRUE)
      untar(urlfile)
    }
    img <- readNIfTI("colin_1mm")
    img = cal_img(img)
    
    test.file <- "/home/ben/uscdocs/PSYC599-bigdata/project/fmri-r-package/test-data/sub126-CUPSTask.nii.gz"
    test.file.uncompressed <- "/home/ben/uscdocs/PSYC599-bigdata/project/fmri-r-package/test-data/sub126-CUPSTask.nii"
    img <- readNIfTI(test.file)#Error in performPermutation(trans, real.dimensions, data, verbose) : 
                                #Transformation is not simple, cannot reorient!
    img <- readNIfTI(test.file.uncompressed)#Error in performPermutation(trans, real.dimensions, data, verbose) : 
                                            #Transformation is not simple, cannot reorient!
  }else{
    stop(paste0("read.image doesn't recognize the file format ", file.format))
  }
  return(image)
}