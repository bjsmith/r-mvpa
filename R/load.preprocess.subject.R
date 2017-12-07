#' load and pre-process a subject
#' 
#' @param x fmri timeseries
#' @param run.path list of the run objects as a file to look up
#' @param run.factor.list should be a data frame, the first column of which is called "labels"
#' and describes the labels applied to each image; the second column called "chunks" and describes any applicable chunks (e.g., runs)
#' @export
load.preprocess.subject <- function(img.path,mask.path,run.path=NULL,run.factor.list=NULL){
  #loads subject data, masks, and preprocesses it.
  #the image should be a 4D image the 4th dimension of which is time.
  subj.data <- NA
  result = tryCatch({
    #subj.dir <- paste0("/../mnt/data/haxby2001/subj", sub.n, "/")
    print(paste0("getting data from ",img.path," and ",mask.path))
    subj.n.data <- load.subject.data(img.path,mask.path,run.path,run.factor.list)
    #now get the masked version of this subject's data.
    print("Size of mask:")
    print(sum(subj.n.data$mask))
    display.2D.img(subj.n.data$mask[11,,])
    prod(dim(subj.n.data$raw.data)[1:3]) 
    masked.data <- t(matrix(subj.n.data$raw.data[subj.n.data$mask>0],sum(subj.n.data$mask>0),dim(subj.n.data$raw.data)[4]))
    print("size of masked data:")
    print(dim(masked.data))
    
    #OK; now we want to take out all rest values.
    include.ts <- subj.n.data$labels$labels!="rest"
    masked.active.data <- masked.data[include.ts, ]
    active.labels <- factor(subj.n.data$labels$labels[include.ts])
    active.runs <-subj.n.data$labels$chunks[include.ts]
    #preprocess it
    print("...preprocessing...")
    masked.active.data.pp <- prepropress.fmri.run.set(masked.active.data, active.runs)
    
    print("...taking average value...")
    #one more thing: so that we can show the masked data in context,
    #we'll take a 3D snapshot of the 4D dataset and save that
    #so that the masked data can be projected onto it.
    img.average = apply(subj.n.data$raw.data,c(1,2,3),mean)

    
    
    display.2D.img(masked.active.data.pp)
    subj.data <- list(x = masked.active.data.pp, 
                      y = active.labels,
                      run=active.runs,
                      img.average = img.average,
                      mask = subj.n.data$mask)
  }, error = function(e) {
    print(paste0("There was an error while trying to get the data for subject with path",img.path,":\n",e))
    warning(paste0("There was an error while trying to get the data for subject with path",img.path,":\n",e))
  })
  
  return(subj.data)
}
#for data downloaded from http://data.pymvpa.org/datasets/haxby2001/
load.subject.data <- function(img.path,mask.path,labels.filename=NULL,labels.factor.list=NULL){

  #subj.dir <- "/../mnt/data/haxby2001/subj1/"
  #load mask
  mask.vt.img <- read.image(file = mask.path)
  summary(mask.vt.img)
  table(mask.vt.img)
  
  #nice.
  
  #get the data
  print("loading BOLD data, this may take some time...")
  bold.raw.data <- read.image(file = img.path)
  dim(bold.raw.data)
  print("loaded.")
  
  #are we reading filename or factors?
  label.mode<-'filename' #by default
  if(!is.null(labels.filename)&&is.null(labels.factor.list)){
    #now we need the design file and hopefully subject info.
    #labels.filename <-  run.path
    labels <- readChar(labels.filename, file.info(labels.filename)$size)
    labels <- read.csv(labels.filename,sep=" ")
    table(labels)
    #this appears to have both. great!
  }else if (is.null(labels.filename)&& !is.null(labels.factor.list)) {
    labels <- labels.factor.list
  }else{
    stop("load.subject.data should be passed exactly one of labels.filename and labels.factor.list, but neither or both of these was passed. Please check the call and try again.")
  }
  
  return(list(mask = mask.vt.img, raw.data = bold.raw.data, labels = labels))
}
