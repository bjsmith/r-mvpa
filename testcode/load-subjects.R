load.subjects(sapply(1:6,function(n){(paste0("/../mnt/data/haxby2001/subj", n, "/"))})
              ,'bold.nii.gz'
              ,'mask4_vt.nii.gz')

load.subjects <- function(dir.list,img.rel.path,mask.rel.path){
  #loads subject data, masks, and preprocesses it.
  ds=list()
  for (subj.dir in dir.list){
    #sub.n=2
    subj.data <- NA
    result = tryCatch({
      #subj.dir <- paste0("/../mnt/data/haxby2001/subj", sub.n, "/")
      print(paste0("getting data from ",subj.dir))
      subj.n.data <- load.subject.data(subj.dir,img.rel.path,mask.rel.path)
      #now get the masked version of this subject's data.
      sum(subj.n.data$mask)
      display.2D.img(subj.n.data$mask[11,,])
      prod(dim(subj.n.data$raw.data)[1:3]) 
      masked.data <- t(matrix(subj.n.data$raw.data[subj.n.data$mask==1],sum(subj.n.data$mask==1),dim(subj.n.data$raw.data)[4]))
      #OK; now we want to take out all rest values.
      include.ts <- subj.n.data$labels$labels!="rest"
      masked.active.data <- masked.data[include.ts, ]
      active.labels <- factor(subj.n.data$labels$labels[include.ts])
      active.runs <-subj.n.data$labels$chunks[include.ts]
      #preprocess it
      masked.active.data.pp <- prepropress.haxby.run.set(masked.active.data, active.runs)
      
      display.2D.img(masked.active.data.pp)
      subj.data <- list(x = masked.active.data.pp, 
                        y = active.labels,
                        run=active.runs)
    }, error = function(e) {
      warning(paste0("There was an error while trying to get the data for subject ",sub.n,":\n",e))
    })
    
    ds=append(ds,list(subj.data))
    
    print('data collected.')
    
  }
  return(ds)
}
#for data downloaded from http://data.pymvpa.org/datasets/haxby2001/
load.subject.data <- function(subj.dir,img.rel.path,mask.rel.path){
  #subj.dir <- "/../mnt/data/haxby2001/subj1/"
  #load mask
  mask.vt.img <- read.image(file = paste0(subj.dir,mask.rel.path))
  summary(mask.vt.img)
  table(mask.vt.img)
  
  #nice.
  
  #get the data
  print("loading BOLD data, this may take some time...")
  bold.raw.data <- read.image(file = paste0(subj.dir,img.rel.path))
  dim(bold.raw.data)
  print("loaded.")
  
  #now we need the design file and hopefully subject info.
  labels.filename <-  paste0(subj.dir,'labels.txt')
  labels <- readChar(labels.filename, file.info(labels.filename)$size)
  labels <- read.csv(labels.filename,sep=" ")
  table(labels)
  #this appears to have both. great!
  
  return(list(mask = mask.vt.img, raw.data = bold.raw.data, labels = labels))
}
