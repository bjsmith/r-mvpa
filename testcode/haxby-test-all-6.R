library(mvpa)
#load the haxby data
#need: sudo mount /dev/sdc /mnt/data/ at command line.
register.machine.cores()
#or just load the data I prepared earlier.
load(file = "haxby.6subjs.rformat.Rdata")
for (ds in ds.results){
  display.mask.over.data <- function(subj.data){
    #need to put the mask in the  space of the data
    #though I think we have that...
    img.in.2D=align.3D.img.in.2D(subj.data$img.average,3)
    mask.in.2D=align.3D.img.in.2D(subj.data$mask,3)*max(img.in.2D)
    additive.d <- img.in.2D+mask.in.2D
    additive.d[additive.d>max(img.in.2D)] <- max(img.in.2D)
    proj <- abind(additive.d,img.in.2D,img.in.2D,along=3)
    display.2D.img(proj)
  }
  display.mask.over.data(ds)
  
  #Get R to look up the number of machine cores to take advantage of multicore processing
  
  
  #OK, now let's try with PCA
  svm.spec <- get.caret.model.spec("svmLinear", preProcess="pca")
  lda.spec <- get.caret.model.spec("lda", preProcess = "pca")
  knn.spec <- get.caret.model.spec("knn",tuning = 10, preProcess = "pca")
  
  #Now let's try nnet.
  #with the nnet package we need to specify a
  nnet.spec <- get.caret.model.spec(
    "nnet"
    , tuning = expand.grid(decay = c(0.3, 0.7), size = c(3,5,7))
    ,preProcess="pca")
  
  method.list <- list(svm.spec, lda.spec, knn.spec, nnet.spec)
  
  res.nnet <- caret.train.model.list(
    x=ds$x
    ,y=ds$y
    ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
    ,training.list = method.list
  )
  
  results <- resamples(res.nnet)
  dotplot(results)
  for (res in results){
    print(res)
  }
}