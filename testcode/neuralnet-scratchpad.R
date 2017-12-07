#we can actually use neuralnet with caret too!
#This makes it an easy tool to try out to do multi-level modeling
#(I only wonder...is there a point in doing this if the linear stuff already works damn well?)
#uses the functions
# neuralnet and
# prediction
#looks like it's typically used with a formula
#so let's try a real basic one and just try to classify better than chance.

#source("testcode/haxby-load-6subjects.R")
#ds.results <- haxby.load.6subjects()
load(file = "haxby.6subjects.rformat.Rdata")

#to get a network that's exactly comparable with the nnet, we need to manipulate "size" and decay
#as well as other defaults, but let's not worry about them for the moment.
#we need the formula format of the data...



control.10pcpca <- trainControl(method="repeatedcv", number=10, repeats=3,
                        ,preProcOptions=list(pcaComp=57))
register.machine.cores()
s1.ds <- ds.results[[1]]
s1.df <- data.frame(y=s1.ds$y,s1.ds$x)


modelLookup("neuralnet")
# list(
#   get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(5,7,10))
#                        ,preProcess="pca"
#                        ,MaxNWts=10000),
#   get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(5,7,10))
#                        ,preProcess="pca"
#                        ,MaxNWts=10000)
# )
neuralnet.base.comparison = list(
  get.caret.model.spec("svmLinear"
                       ,preProcess="pca"), #base comparison
#  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(7))
#                       ,preProcess="pca"
#                       ,trControl = trainControl(preProcOptions=list(pcaComp=57))
#                       ,MaxNWts=10000),
  get.caret.model.spec("neuralnet",preProcess="pca",expand.grid(layer1=c(2,3))))
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  y~.,
  s1.df,
  control.10pcpca,
  neuralnet.base.comparison)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune
#we might need a data frame approach.

#still doesn't work'. Turns out we need to:
#1) expand the output factor to a set of binary variables;
#2) avoid "." notation; perhaps use http://www.inside-r.org/packages/cran/AlgDesign/docs/expand.formula ?
library(nnet)#needed for the class.ind function.
s1.ds <- ds.results[[1]]
s1.df <- data.frame(class.ind(s1.ds$y),s1.ds$x)
s1.df2 <- data.frame(y=s1.ds$y,s1.ds$x)

#now we need to create a formula
y.val.names = c("bottle","cat","chair","face","house","scissors","scrambledpix","shoe")
#prepare the data frame.
all.names = colnames(s1.df)
ds.formula <- as.formula(paste(
  paste(y.val.names, collapse = " + ")
  #"y"
  ,"~"
  ,paste(all.names[!(all.names %in% y.val.names)], collapse = " + ")))

#try again
neuralnet.base.comparison = list(
  get.caret.model.spec("neuralnet",preProcess="pca",expand.grid(layer1=c(2,3))))
#res <- ds.results[[1]]
caret.model.list <- caret.train.model.list(
  ds.formula,
  s1.df,
  control.10pcpca,
  neuralnet.base.comparison)
results <- resamples(caret.model.list)
dotplot(results)
caret.model.list[[2]]$bestTune

#OK we are getting problems...we're gonna have to try running neuralnet by itself
source("neuralnets-seeds-example.R")

#OK so we need to:

#clean out any NA values in predictors
View(s1.df)
#make sure there's not too many categories
#there's not; we have 8.

#columns should be named
colnames(s1.df)
#names look fine.
#need to write out formula.
y.val.names = c("bottle","cat","chair","face","house","scissors","scrambledpix","shoe")
all.names = colnames(s1.df)
ds.formula <- as.formula(paste(
  paste(y.val.names, collapse = " + ")
  #"y"
  ,"~"
  ,paste(all.names[!(all.names %in% y.val.names)], collapse = " + ")))

#run
nn = neuralnet(ds.formula,
               data = s1.df,
               hidden=2)
#let's not plot
names(nn)
print(nn)
res <- compute(nn,s1.df[, -(1:8)])
summary(res)
dim(res$net.result)
predictions <- apply(res$net.result,1,function(row){return(which(row==max(row)))})
ground.truth <- apply(s1.df[, 1:8],1,function(row){return(which(row==max(row)))})
table(predictions,ground.truth)

#not too bad, though remember we're not using CV. Try again with 3?

nn = neuralnet(ds.formula,
               data = s1.df,
               hidden=3)
#let's not plot
control <- trainControl(method="repeatedcv", number=10, repeats=3)
names(nn)
print(nn)
res <- compute(nn,s1.df[, -(1:8)])
summary(res)
dim(res$net.result)
predictions <- apply(res$net.result,1,function(row){return(which(row==max(row)))})
ground.truth <- apply(s1.df[, 1:8],1,function(row){return(which(row==max(row)))})
table(predictions,ground.truth)

#OK. Let's stop, and try to implement again using caret.

res <- train(ds.formula,
      s1.df,
      method = "neuralnet",
      trControl = control,
      tuneGrid = expand.grid(layer1=c(2,3)))

#OK so we just got the same error when we missed out "neuralnet"! which means there's
#something wrong in the way caret is processing this.
debug(train)

res <- train(ds.formula,
             s1.df,
             method = "neuralnet",
             trControl = control,
             tuneGrid = expand.grid(layer1=c(2,3)))
undebug(train)
debug(createFolds)
#so we've pinpointed the line the error occurs on but it's hard to see why
createFolds
#it doesn't like input from y.
res <- train(ds.formula,
             s1.df,
             method = "svmLinear",
             trControl = control)
#OK, there's an error here, too. something wrong with control?
control <- trainControl(method="cv", number=10)
res <- train(ds.formula,
             s1.df,
             method = "svmLinear",
             trControl = control)

#well...whatabout when it was working...what was going on in cut then?
caret.model.list <- train(
  y~.,
  s1.df2,
  method = "svmLinear",
  trControl = control)

#something...but not the same. Let's take al ook at the inputs to createFolds in this case, particularly y
#seems to be measuring the factors now.
#so...there should be a better way to run createFolds
apply(s1.df,2,function(col){return(length(unique(col)))})
apply(s1.df2,2,function(col){return(length(unique(col)))})
#questions here, but no appropriate answer:
#http://stackoverflow.com/questions/18510492/train-in-caret-package-returns-an-error-about-names-gsub

control <- trainControl(method="cv", number=10)
ds.formula.trunc <- as.formula(paste(
  paste(y.val.names, collapse = " + ")
  #"y"
  ,"~"
  ,paste(all.names[which(!(all.names[1:20] %in% y.val.names))], collapse = " + ")))

res <- train(ds.formula.trunc,
             s1.df[, 1:20],
             method = "svmLinear",
             trControl = control)


#example for stackoverflow


ds.formula.trunc <- as.formula(paste(
  paste(y.val.names, collapse = " + ")
  #"y"
  ,"~"
  ,paste(all.names[which(!(all.names[1:20] %in% y.val.names))], collapse = " + ")))
train.formula <- ds.formula.trunc

eg.data <- s1.df[, 1:20]

#begins.
require(caret)
print(train.formula)
head(eg.data)
#all columns have different values specified
apply(eg.data,2,function(col){return(length(unique(col)))})
apply(eg.data[,1:8],2,table)
res <- train(train.formula,
             eg.data,
             method = "svmLinear",
             trControl = trainControl(method="cv", number=10))

traceback()

debug(cut.default)

res <- train(train.formula,
             eg.data,
             method = "svmLinear",
             trControl = trainControl(method="cv", number=10))

#could we maybe succeed if we don't do any cv?
res <- train(train.formula,
             eg.data,
             method = "svmLinear",
             trControl = trainControl(method="boot", number=10))

res <- train(train.formula,
             eg.data,
             method = "neuralnet",
             trControl = trainControl(method="boot", number=10),
             tuneGrid =  expand.grid(layer1=c(2,4,6,8),layer2=c(0,2,4,6),layer3=c(0,2,4,6)))

res$bestTune
#I'm not sure this is wroking properly but not sure what's wrong. Let's try with the full dataset.
res <- train(ds.formula,
             s1.df,
             method = "neuralnet",
             trControl = trainControl(method="boot", number=10),
             tuneGrid =  expand.grid(layer1=c(2,4,6),layer2=c(0,2,3),layer3=c(0,2,3)))

#OK so that gave us the same problem...unrealistically low RMSE values and NaN R^2. Let's try setting this up in my caret batch functionn.
control.boot <- trainControl(method="boot", number=10, 
                                ,preProcOptions=list(pcaComp=57))
#we might get problems passing in the data as it's currently formatted to svmLinear, though.

register.machine.cores()
s1.ds <- ds.results[[1]]
library(nnet)#needed for the class.ind function.
s1.df <- data.frame(class.ind(s1.ds$y),s1.ds$x)
y.val.names = c("bottle","cat","chair","face","house","scissors","scrambledpix","shoe")
all.names = colnames(s1.df)
ds.formula <- as.formula(paste(
  paste(y.val.names, collapse = " + ")
  #"y"
  ,"~"
  ,paste(all.names[!(all.names %in% y.val.names)], collapse = " + ")))

neuralnet.comparison = list(
  get.caret.model.spec("svmLinear"
                       ,preProcess="pca"),
  get.caret.model.spec("nnet", expand.grid(decay = c(0.7), size = c(7))
                       ,preProcess="pca"
                       ,MaxNWts=10000),
  get.caret.model.spec("neuralnet",preProcess="pca",expand.grid(layer1=c(2,4),layer2=c(0,2),layer3=c(0,2))))


caret.model.list <- caret.train.model.list(ds.formula,
                                           s1.df,
                                           control.boot,
                                           neuralnet.comparison)

#is the problem here with the way we're now formatting the data; or perhaps with the bootstrap method?
control.boot <- trainControl(method="boot", number=10, 
                             ,preProcOptions=list(pcaComp=57))
control.rcv <- trainControl(method="repeatedcv", number=10,repeats=3,preProcOptions=list(pcaComp=57))
control.none <- trainControl(method="none", preProcOptions=list(pcaComp=57))
svm.simple = list(
  get.caret.model.spec("svmLinear"
                       ,preProcess="pca"
                       ,trControl=control.none),
  get.caret.model.spec("svmLinear"
                       ,preProcess="pca"
                       ,trControl=control.boot))

caret.model.list <- caret.train.model.list(ds.formula,
                                           s1.df,
                                           control.boot,
                                           svm.simple)

#OK, problematic. Let's go back a step. We could get the neuralnet package to process the data when not running through caret, right?
library(neuralnet)
nn = neuralnet(ds.formula,
               data = s1.df,
               hidden=c(5))
#let's not plot
names(nn)
print(nn)

calc.success <- function(nn){
  print(nn$call)
  
  res <- compute(nn,s1.df[, -(1:8)])
  print(summary(res))
  print(dim(res$net.result))
  predictions <- apply(res$net.result,1,function(row){return(which(row==max(row)))})
  ground.truth <- apply(s1.df[, 1:8],1,function(row){return(which(row==max(row)))})
  print(table(predictions,ground.truth))
  accuracy <- sum(diag(table(predictions,ground.truth)))/sum(table(predictions,ground.truth))
  print(accuracy)
  
}
calc.success(nn)

#and if this works, then we should be able to *at least* get it working
#using trainControl, with crossValidation
#so this very basic, no-CV test did OK with 5
#can we replicate within the caret framework?
control.none <- trainControl(method="none")
basic.neuralnet = list(
  get.caret.model.spec("neuralnet"
                       ,trControl=control.none,tuning=expand.grid(layer1=5,layer2=0,layer3=0)))

caret.model.list <- caret.train.model.list(ds.formula,
                                           s1.df,
                                           control.none,
                                           basic.neuralnet)

calc.success(caret.model.list[["1neuralnet"]]$finalModel)

debug(train)
undebug(train)
debug(neuralnet)



caret.model.list <- caret.train.model.list(ds.formula,
                                           s1.df,
                                           control.none,
                                           basic.neuralnet)
#OK so looking in at the caret procedure for neuralnet, it seems like caret creates a ".outcome" column
#and then modifies the data to be this. This can't be right! it'll mean we can't learn the outcome.

#makes sense and appears to be the same problem from yesterday; a stackoverflow poster says:
#http://stackoverflow.com/questions/29932731/error-in-createfolds-cut-default-while-running-train-in-caret-with-multiple-bi/29953255#29953255
# "train requires your outcome to be a single dimensional factor (as opposed to multiple binary outcomes). 
# If they are separate outcomes (i.e. not mutually exclusive) you would need to run one model per outcome."
# So...it seems like we can't pass a multinomial variable into neuralnet
# and we can't pass a group of binomial variables into caret
# so it would seem multinomial classification is out.

#any way to run neuralnet with multiple classifiers?
all.names = names(s1.df2)
ds.formula <- as.formula(paste(
  "y"
  ,"~"
  ,paste(all.names[-1], collapse = " + ")))

nn = neuralnet(ds.formula,
               data = s1.df2,
               hidden=c(5))

#nope. only option if we want to use neuralnet with caret is to write a custom handler for it, or modify the one that caret has
#it's possible we could work on this.

#OK, could we use the deepnet package?