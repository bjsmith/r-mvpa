install.packages('caret', dependencies = TRUE)
library(mvpa)
#load the haxby data
#need: sudo mount /dev/sdc /mnt/data/ at command line.
# ds.results =list()
# subj.dir <- "/Users/benjaminjsmith/msmserver/sub"
# for (n in 1:6){
#   
#   img.path <- paste0(subj.dir, n, "/data/Gonogo.nii.gz")
#   mask.path <- paste0(subj.dir, n, "/mask4_vt.nii.gz")
#   run.path <- paste0(subj.dir, n, '/labels.txt')
#   subj.n <- 
#     load.preprocess.subject(
#       img.path
#       ,mask.path
#       ,run.path)
#   ds.results = append(ds.results,list(subj.n))
# }
# save(ds.results, file = "haxby.6subjs.rformat.Rdata")

#or just load the data I prepared earlier.
setwd("/Users/benjaminsmith/Documents/r-mvpa-project/mvpa")
load(file = "haxby.6subjs.rformat.Rdata")

subj1.data <-ds.results[[1]]
load(file = "haxby.sub1.rformat.Rdata")
#save(subj1.data, file = "haxby.sub1.rformat.Rdata")

#view it

display.mask.over.data <- function(subj.data){
  #need to put the mask in the space of the data
  #though I think we have that...
  img.in.2D=t(align.3D.img.in.2D(subj.data$img.average,3))
  mask.in.2D=t(align.3D.img.in.2D(subj.data$mask,3)*max(img.in.2D))
  additive.d <- img.in.2D+mask.in.2D
  additive.d[additive.d>max(img.in.2D)] <- max(img.in.2D)
  proj <- abind(additive.d,img.in.2D,img.in.2D,along=3)
  display.2D.img(proj)
}
display.mask.over.data(subj1.data)

#Get R to look up the number of machine cores to take advantage of multicore processing
register.machine.cores()

#now run through the nnet classifier - perhaps try several different methods.


#we'll start with SVM for comparison purposes.
svm.spec <- get.caret.model.spec("svmLinear")
print(svm.spec)


#add another couple of methods
lda.spec <- get.caret.model.spec("lda")
knn.spec <- get.caret.model.spec("knn",tuning = 10)

#without reduction to 10% of items
no.reduction <- list(svm.spec,lda.spec,knn.spec)
full.features <- caret.train.model.list(
  x=subj1.data$x
  ,y=subj1.data$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3)
  ,training.list = no.reduction
)
names(full.features)
full.features[["1svmLinear"]]
full.features[["3knn"]]
full.features[["3knn"]]$bestTune

results <- resamples(full.features)
dotplot(results)

#OK, now let's try with PCA
svm.spec <- get.caret.model.spec("svmLinear", preProcess="pca")
lda.spec <- get.caret.model.spec("lda", preProcess = "pca")
knn.spec <- get.caret.model.spec("knn",tuning = 10, preProcess = "pca")

non.nnet.methods <- list(svm.spec, lda.spec, knn.spec)

subj1.data <- ds.results[[1]]

#cool. Now we can pass into caret.

res <- caret.train.model.list(
  x=subj1.data$x
  ,y=subj1.data$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = non.nnet.methods
  )

results <- resamples(res)
dotplot(results)

#Now let's try nnet.
#with the nnet package we need to specify a
nnet.spec <- get.caret.model.spec(
  "nnet"
  , tuning = expand.grid(decay = c(0.3, 0.7), size = c(3,5,7))
  ,preProcess="pca")

method.list <- list(svm.spec, lda.spec, knn.spec, nnet.spec)

res.nnet <- caret.train.model.list(
  x=subj1.data$x
  ,y=subj1.data$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = method.list
)

#check out the results
print(res.nnet[["3knn"]])
print(res.nnet[["4nnet"]])


results <- resamples(res.nnet)
dotplot(results)
names(res.nnet)
res.nnet[["4nnet"]]

# these results look almost too good, especially on such a simple network.
# What if we randomized the outcome variable y
#- would they be be the same?
y.random = sample(subj1.data$y,length(subj1.data$y),replace = FALSE)
res.randomized <- caret.train.model.list(
  x=subj1.data$x
  ,y=y.random
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = method.list
)
results <- resamples(res.randomized)
dotplot(results)
#Chance level of 1/8 is within the confidence interval of each model generated here.

#-get comparable results on other subjects
#-more challenging data
#-try with a deep learning network

#need to look for another network!!!