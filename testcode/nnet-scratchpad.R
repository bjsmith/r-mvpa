#http://stackoverflow.com/questions/19862478/classification-in-r-neuralnet
#http://webcache.googleusercontent.com/search?q=cache:JrBY-Ihnor8J:www.jeffheaton.com/2013/06/basic-classification-in-r-neural-networks-and-support-vector-machines/+&cd=1&hl=en&ct=clnk&gl=us&lr=lang_en%7Clang_zh-CN&client=ubuntu-browser
#http://www.r-bloggers.com/classification-using-neural-net-in-r/

source("testcode/haxby-load-6subjects.R")
ds.results <- haxby.load.6subjects()

getwd()
#save(ds.results,file = "haxby.6subjects.rformat.Rdata")
#load(file = "haxby.6subjects.rformat.Rdata")
control <- trainControl(method="repeatedcv", number=10, repeats=3)
register.machine.cores()
res <- ds.results[[1]]
#try again with some new functions I wrote.
modelLookup("nnet")
train.list = list(
  get.caret.model.spec("svmLinear"), #base comparison
  get.caret.model.spec("svmLinear",preProcess="pca"), #test reaction of base method when using PCA
  #get.caret.model.spec("knn"),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7, 0.3), size = c(1, 2, 3))
                       ,preProcess="pca"),#try out a PCA neural network with PCA Preprocessing,
  get.caret.model.spec("pcaNNet"
                       , expand.grid(decay = c(0.7, 0.5, 0.3), size = c(1, 2, 3))
                       ,preProcess="pca"),#try out a PCA neural network with PCA Preprocessing
#  get.caret.model.spec("pcr",5),
  get.caret.model.spec("avNNet"
                       ,5
                       ,preProcess="pca")#try out an avNNet with PCA Preprocessing
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  res$x,
  res$y,
  control,
  train.list)
results <- resamples(caret.model.list)
dotplot(results)
names(caret.model.list[[1]])
caret.model.list[[1]]$bestTune
#better. What are the specifications on this?
#that's funny, I'd have though you would need a neural net hidden layers number equal to at least the power of two
#which equals the number of outputs, in this case, 2^n=8, n=3.
#perhaps we should include 3 in the input.

caret.model.list[[1]]

caret.model.list[[2]]

#seems to be the same problem as for nnet - more than a couple of units in the hidden layer and we're getting NaNs.
#Wonder why?
#wonder if it's worth applying PCA pre-processing?



summary(caret.model.list[[1]])

#here's the results:
# 
# decay  size  Accuracy   Kappa      Accuracy SD  Kappa SD  
# 0.3    1     0.2754176  0.1724805  0.03376913   0.03832694
# 0.3    2           NaN        NaN          NA           NA
# 0.3    3           NaN        NaN          NA           NA
# 0.3    4           NaN        NaN          NA           NA
# 0.5    1     0.2831488  0.1805843  0.02586171   0.02916264
# 0.5    2           NaN        NaN          NA           NA
# 0.5    3           NaN        NaN          NA           NA
# 0.5    4           NaN        NaN          NA           NA
# 0.7    1     0.2889457  0.1878253  0.02702265   0.03039081
# 0.7    2           NaN        NaN          NA           NA
# 0.7    3           NaN        NaN          NA           NA
# 0.7    4           NaN        NaN          NA           NA

#It's clear that ther'es something wrong with specifying a network with size>1, and we need to sort this out because,
#of course, it's not much of a neural network with only one unit in the hidden layer.

#so what could we change to get a better result?
train.res <- train(x=res$x,y=res$y,
      method = "nnet",
      trControl = control,
      preProcess = "pca")

#OK. adding the preProcess PCA doesn't help. What about reducing the number of input values? perhaps that would help
#the training.
reduced.voxels.ds <- res
reduced.voxel.sample <- sample(1:dim(res$x)[2],floor(dim(res$x)[2]/10))
reduced.voxels.ds$x <- res$x[, reduced.voxel.sample]

train.list.svm.nnet = list(
  get.caret.model.spec("svmLinear"), #base comparison
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7, 0.5, 0.3), size = c(1, 2, 3, 4)))
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
#OK - eliminating a bunch of the input voxels did seem to allow for better performance.
#Perhaps if we want to have such a large number of neural networks, we need to implement a multi-layered approach, somehow.
#Let's explore the network size more.
#I would have thought that the PCA simplification would achieve the same thing.
#we might need to look more carefully at exactly what that's doing.
train.list.svm.nnet = list(
  get.caret.model.spec("svmLinear"), #base comparison
  get.caret.model.spec("nnet", expand.grid(decay = c(0.9, 0.7, 0.5, 0.3), size = c(1:10)))#try out a PCA neural network with PCA Preprocessing,
)
#res <- ds.results[[1]]
dim(reduced.voxels.ds$x)
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
#OK...so the network with 10 hidden layers now performs best; better than svmLinear on an equivalent dataset.
#so what if we extend the size even more?

train.list.svm.nnet = list(
  get.caret.model.spec("svmLinear"), #base comparison
  get.caret.model.spec("nnet", expand.grid(decay = c(0.9, 0.7, 0.5, 0.3), size = floor(1.5^c(3:10))))#try out a PCA neural network with PCA Preprocessing,
)
#res <- ds.results[[1]]
dim(reduced.voxels.ds$x)
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune

#we've hit the "NaN" ceiling again. Time to run nnet directly to try dig down to what's going wrong.
#to be consistent we should stick to 10-fold cross validation.
library(nnet)
nnet.cv <- function(size,decay){
  set.seed(183443)
  folds <- createFolds(reduced.voxels.ds$y,k=10)
  #y.int <- as.integer(reduced.voxels.ds$y)
  nnet.results <- list()
  nnet.performance.list <- list()
  nnet.matches <- list()
  for (holdout in folds){
    y.var <- class.ind(reduced.voxels.ds$y) #http://petewerner.blogspot.com/2013/01/tracking-down-errors-in-r.html
    train.folds <- which(!(1:length(reduced.voxels.ds$y) %in% holdout))  
    dim(reduced.voxels.ds$x[train.folds,])
    
    #options(warn = 2)
    nnet.m1 <- nnet(
      x=reduced.voxels.ds$x[train.folds, ]
      ,y=y.var[train.folds, ]
      , size=size,decay=decay
      ,MaxNWts=10000)
    #which folds are not in the training set
  
    #cool. test on the holdout.
    predict.res <- predict(nnet.m1,
            reduced.voxels.ds$x[holdout, ])
    nnet.results = append(nnet.results,list(predict.res))
    nnet.predicted.cat <- apply(predict.res,1,
                                function(row){
                                  return(names(row)[which(row==max(row))])})
    targets <- as.character(reduced.voxels.ds$y[holdout])
    nnet.performance = 
      sum(targets==nnet.predicted.cat)/ length(holdout)
    nnet.matches= append(nnet.matches, list(table(nnet.predicted.cat,targets)))
    nnet.performance.list <- append(nnet.performance.list, nnet.performance)
  }
  boxplot(unlist(nnet.performance.list))
  print(summary(unlist(nnet.performance.list)))
  return(nnet.performance.list)
}
perform11 <- nnet.cv(11,0.9)
perform15 <- nnet.cv(15,0.9)
perform20 <- nnet.cv(20,0.9)

#so we get an error "too many weights" at this level. This might be why the network was failing earlier for large numbers
#of values. It's important we reach asymptotic performane for the number of units, so we need to explore higher.
#looks like we can pass this stuff to caret, too
perform30 <- nnet.cv(30,0.5)

summary(perform20)
summary(unlist(perform15))
summary(unlist(perform20))
summary(unlist(perform11))

#so we don't seem to be getting better at this level of weights. good to know.
#can we go back to 

#below: just trying nnet with 

#amazing tutorial:
#http://petewerner.blogspot.com/2013/01/tracking-down-errors-in-r.html

#try as data frame


nnet.results <- list()
nnet.performance.list <- list()
nnet.matches <- list()
reduced.voxels.df <- data.frame(y=reduced.voxels.ds$y,reduced.voxels.ds$x)
for (holdout in folds){
  train.folds <- which(!(1:length(reduced.voxels.ds$y) %in% holdout))  
  dim(reduced.voxels.ds$x[train.folds,])
  nndf <- 
  #options(warn = 2)
  nnet.m1 <- nnet(y~.,
                  reduced.voxels.df[train.folds, ],
    , size=11,decay=0.9)
  #which folds are not in the training set
  
  #cool. test on the holdout.
  predict.res <- predict(nnet.m1, reduced.voxels.df[holdout, ])
  nnet.results = append(nnet.results,list(predict.res))
  nnet.predicted.cat <- apply(predict.res,1,
                              function(row){
                                return(names(row)[which(row==max(row))])})
  targets <- as.character(reduced.voxels.ds$y[holdout])
  nnet.performance = 
    sum(targets==nnet.predicted.cat)/ length(holdout)
  nnet.matches= append(nnet.matches, list(table(nnet.predicted.cat,targets)))
  nnet.performance.list <- append(nnet.performance.list, nnet.performance)
}
print(summary(unlist(nnet.performance.list)))
#OK great - this is about what we were getting before when we wre runnint nnet through caret


#Try again, this time just test we are able to pass MaxNWts as a param
train.list.svm.nnet = list(
#  get.caret.model.spec("svmLinear") #base comparison
#  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(10, 20,25)))
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(25)),MaxNWts=10000)
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune

#then - try again using PCA again.
train.list.svm.nnet = list(
  #  get.caret.model.spec("svmLinear") #base comparison
  #  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(10, 20,25)))
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,8,16)),preProcess="pca",MaxNWts=1000),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,8,16)),MaxNWts=1000)
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
summary(caret.model.list)
caret.model.list[[1]]$results
caret.model.list[[2]]$results
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune

#great! Before we try on the larger network - how large could we push this network?
train.list.svm.nnet = list(
  #  get.caret.model.spec("svmLinear") #base comparison
  #  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(10, 20,25)))
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,8,16,24)),preProcess="pca",MaxNWts=1000),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,8,16)),MaxNWts=1000)
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  reduced.voxels.ds$x,
  reduced.voxels.ds$y,
  control,
  train.list.svm.nnet)
summary(caret.model.list)
caret.model.list[[1]]$results
caret.model.list[[2]]$results
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
#doesn't quite get to 24 - we might still need to work on the PCA pre-processing!

#OK. Let's go back to the original full-size dataset, but this time, let's do it with PCA - now that we know the PCA is working.
ds <- ds.results[[1]]
train.list.svm.nnet = list(
    get.caret.model.spec("svmLinear"), #base comparison
  #  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(10, 20,25)))
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(1,2,4)),preProcess="pca"),
    get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(1,2,4)))
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  ds$x,
  ds$y,
  control,
  train.list.svm.nnet)
summary(caret.model.list)
caret.model.list[[1]]$results
caret.model.list[[2]]$results
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune

#we can check to see the preprocess class and what it actually does with the data.
#works with the predictor data only...
preprocess.res <- preProcess(ds$x,method="pca")
length(preprocess.res)
preprocess.res$dim
dim(preprocess.res$rotation)
preprocess.res$rotation
preprocess.res$numComp #OK so it reduced down to 263 values to get 95% of the variance. 
#the result WAS better precisely because it allowed us to get to 4 hidden units.

#looks like even with this PCA preprocessing, we can't get to 4 hidden layer units. We will have to push the maximum layers up, which will get us
#at least 4, I think.


train.list.svm.nnet = list(
  get.caret.model.spec("svmLinear"), #base comparison
  #  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(10, 20,25)))
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,7)),
                       preProcess="pca",
                       MaxNWts=10000)
)
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  ds$x,
  ds$y,
  control,
  train.list.svm.nnet)
summary(caret.model.list)
caret.model.list[[1]]$results
caret.model.list[[2]]$results
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
#great! We get 95% accuracy with 7 hidden units - -so for this really simple task, we don't even need hidden layers.
#seem to need just under 2000 weights, so let's set MaxNWts to 3000 next time.
#debugging: http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/debug.shtml
#What if we went for 57 components - that's the number of voxels we randomly selected before.
train.list.svm.nnet = list(
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,4,7,10))
                       ,preProcess="pca"
                       ,trainControl = trainControl(preProcOptions=list(pcaComp=57))
                       ,MaxNWts=10000),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(4,7,10))
                         ,preProcess="pca"
                         ,MaxNWts=10000)
)
#http://www.inside-r.org/packages/cran/caret/docs/trainControl
caret.model.list <- caret.train.model.list(
  ds$x,
  ds$y,
  control,
  train.list.svm.nnet)

caret.model.list[[2]]$preProcess$numComp
caret.model.list[[2]]$preProcess$dim

summary(caret.model.list)
caret.model.list[[1]]$results
caret.model.list[[2]]$results
caret.model.list[[2]]$preProcess$pcaComp

caret.model.list[[2]]$preProcess$pcaComp

caret.model.list[[3]]$results
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
save(caret.model.list,file="comparison-full-net-and-pca.RData")
#do we want to move on to a multi-layer neural network next, or try something else?
#neuralnet would do multi-layers
#caret seems to support neuralnet, so that's helpful.
#http://stackoverflow.com/questions/21662180/using-neuralnet-with-caret-train-and-adjusting-the-parameters
#there's also rsnns
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&uact=8&ved=0CCcQFjAB&url=http%3A%2F%2Fcran.r-project.org%2Fweb%2Fpackages%2FRSNNS%2FRSNNS.pdf&ei=l_Q2Vbj_JoexyQT_9YDwBw&usg=AFQjCNEsVDwgZzcMbTvD0jGm01zQ3tSd-w&sig2=3dkZBMfyBFOqP47UrpVi9A&bvm=bv.91427555,d.aWw
#http://dicits.ugr.es/software/RSNNS/
#but RSNNS isn't integrated with caret
#I think neuralnet will be the one to try.
#otherwise, we could have another quick go with deepnet, but that's probably a waste of time.

#just trying to get the pre-processing working properly
preprocess.res <- preProcess(ds$x,method="pca",pcaComp=57)
preprocess.res$pcaComp
#FOUND the problem:
#We're passing "preProcess" as an argumetn to train, but it needs to be an item in the list passed to trainControl to generate the object passed to trControl
#right now--for good reason--I've set it up to accept a single trControl argument for all values.
#The best way forward is probably to reprogram get.caret.model.spec to take a trControl value that will override the "default" value
#that is passed to caret.train.model.list. This is probably a good general model going forward
#because ther is likely to be a lot of values that sometimes are useful to set all at once and sometimes useful to specfiy in particular.

train.lite = list(
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,3))
                       ,preProcess="pca"
                       ,trControl = trainControl(preProcOptions=list(pcaComp=57))
                       ,MaxNWts=10000),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(2,3))
                       ,preProcess="pca"
                       ,MaxNWts=10000)
)

caret.model.list <- caret.train.model.list(
  ds$x,
  ds$y,
  control,
  train.lite)

caret.model.list[[1]]$preProcess$pcaComp
caret.model.list[[2]]$preProcess$pcaComp
summary(caret.model.list)
results <- resamples(caret.model.list)
caret.model.list[[1]]
caret.model.list[[2]]

#repeat now it's working!

train.pca.comparison = list(
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(5,7,10))
                       ,preProcess="pca"
                       ,trControl = trainControl(preProcOptions=list(pcaComp=57))
                       ,MaxNWts=10000),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(5,7,10))
                       ,preProcess="pca"
                       ,MaxNWts=10000)
)
caret.model.list <- caret.train.model.list(
  ds$x,
  ds$y,
  control,
  train.pca.comparison)

caret.model.list[[1]]$preProcess$pcaComp
caret.model.list[[2]]$preProcess$pcaComp
summary(caret.model.list)
results <- resamples(caret.model.list)
caret.model.list[[1]]
caret.model.list[[2]]
#accuracy of our 10-unit hidden layer network went down from 97% to 93% when training on 57 principal components (10% of the dataset)
#now, it's really time to go on to looking at neuralnet