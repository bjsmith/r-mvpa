source("testcode/haxby-load-6subjects.R")
ds.results <- haxby.load.6subjects()
getwd()
#save(ds.results,file = "haxby.6subjects.rformat.Rdata")

test.proc <- function(res){
    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    register.machine.cores()

    #try again with some new functions I wrote.
    
    train.list = list(
#      get.caret.model.spec("lda"),
#      get.caret.model.spec("knn",4),
      get.caret.model.spec("nnet"),# expand.grid(.decay = c(0.5, 0.1), .size = 2^c(1:4)*2)),
      get.caret.model.spec("svmLinear",expand.grid(C=c(1:5)))
    )
    
    caret.model.list <- caret.train.model.list(
      res$x,
      res$y,
      control,
      train.list)
    
    results <- resamples(caret.model.list)
    
    
    summary(results)
    
    # boxplots of results
    #bwplot(results)
    # dot plots of results
    dotplot(results)
    return(results)
  }

#test.result <- test.proc(ds.results[[1]])#just the first subject
all.test.results <- lapply(ds.results,test.proc)