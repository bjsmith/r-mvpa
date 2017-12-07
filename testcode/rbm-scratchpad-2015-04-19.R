#restricted boltzman machines: initial scratchpad
#some resources:
#http://blog.echen.me/2011/07/18/introduction-to-restricted-boltzmann-machines/
#http://www.r-bloggers.com/restricted-boltzmann-machines-in-r/
#install.packages("deepnet")
library(deepnet)

#tracking popularity of R packages; this might be necessary in order to work out 
#http://www.nicebread.de/finally-tracking-cran-packages-downloads/
success <- list()
for(i in 1:250){
set.seed(42)
set.size <- i
#DNN Train.
Var1 <- c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2))
Var2 <- c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1))
Var3 <- c(rnorm(set.size, 2.1, 0.4), rnorm(set.size, -3, 0.5))
x <- matrix(c(Var1, Var2,Var3), nrow = set.size*2, ncol = 2)
y <- c(rep(1, set.size), rep(0, set.size))
dnn <- dbn.dnn.train(x, y, hidden = c(5, 5)) ## predict by dnn


test_Var1 <- c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2))
test_Var2 <- c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1))
test_Var3 <- c(rnorm(set.size, 2.1, 0.4), rnorm(set.size, -3, 0.5))
test_x <- matrix(c(test_Var1, test_Var2, test_Var3), nrow = set.size*2, ncol = 2)
res <- nn.test(dnn, test_x, y) #Test the network on new data from the same distribution
#print(i)
#print(res)
success <- append(success,res)
}
plot(unlist(success))
#try again, this seems suspicious.
Var1 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
Var2 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
Var3 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
x <- matrix(c(Var1, Var2,Var3), nrow = set.size*2, ncol = 2)
y <- c(rep(1, set.size), rep(0, set.size))
dnn <- dbn.dnn.train(x, y, hidden = c(5, 5)) ## predict by dnn


test_Var1 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
test_Var2 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
test_Var3 <- c(rnorm(set.size, 1, 0), rnorm(set.size, 1, 0))
test_x <- matrix(c(test_Var1, test_Var2, test_Var3), nrow = set.size*2, ncol = 2)
res <- nn.test(dnn, test_x, y) #Test the network on new data from the same distribution


#try one more time...
Var1 <- rev(c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2)))
Var2 <- rev(c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1)))
x <- matrix(c(Var1, Var2,Var3), nrow = set.size*2, ncol = 2)
y <- c(rep(1, set.size), rep(0, set.size))
dnn <- dbn.dnn.train(x, y, hidden = c(5, 5)) ## predict by dnn


test_Var1 <- c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2))
test_Var2 <- c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1))
test_x <- matrix(c(test_Var1, test_Var2, test_Var3), nrow = set.size*2, ncol = 2)
res <- nn.test(dnn, test_x, y) #Test the network on new data from the same distribution

#fuck...really? Exactly 0.25 each time, even when the training data are designed to have nothing to do with the test data? Seems something's wrong.
nn.test
#nn.test looks normal enough, very rudimentary but nothing wrong it it. what about nn.predict?
nn.predict
length(dnn)
names(dnn)
dnn$size
###Wait, what other properties of neural network data do we need to test?
# I want to see it working with more than two categories...
#just try with three categories..
success <- list()
for(i in (10:20)*100){
  set.seed(42)
  set.size <- i
  #DNN Train.
  Var1 <- c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2),rnorm(set.size,6,2))
  Var2 <- c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1),rnorm(set.size,4,0.5))
  Var3 <- c(rnorm(set.size, 2.1, 0.4), rnorm(set.size, -3, 0.5),rnorm(set.size,-2,0.4))
  x <- matrix(c(Var1, Var2,Var3), nrow = set.size*3, ncol = 3)
  y <- c(rep(1, set.size), rep(0, set.size), rep(2, set.size))
  dnn <- dbn.dnn.train(x, y, hidden = c(5, 5)) ## predict by dnn
  
  
  test_Var1 <- c(rnorm(set.size, 1, 0.5), rnorm(set.size, -0.6, 0.2),rnorm(set.size,6,2))
  test_Var2 <- c(rnorm(set.size, -0.8, 0.2), rnorm(set.size, 2, 1),rnorm(set.size,8,3))
  test_Var3 <- c(rnorm(set.size, 2.1, 0.4), rnorm(set.size, -3, 0.5),rnorm(set.size,-2,0.4))
  test_x <- matrix(c(test_Var1, test_Var2, test_Var3), nrow = set.size*3, ncol = 3)
  res <- nn.test(dnn, test_x, y) #Test the network on new data from the same distribution
  #print(i)
  #print(res)
  success <- append(success,res)
}
plot(unlist(success))
#major problems training with this dataset! and we only have two categories...
#not entirely promising.! never mind. Let's just try it with the haxby data.

#OK, we've played around a little....could we apply the RBM to the haxby dataset?
load(file = "haxby.6subjects.rformat.Rdata")

haxby.s1 <- ds.results[[1]]
names(haxby.s1)
#great. If it's the size of the set, we could always try training with just two categories.
#first, let's just try running the haxby set.

#grab a random sample of half of the dataset...
haxby.s1.train.indices <- sample(1:dim(haxby.s1$x)[1],floor(dim(haxby.s1$x)[1]/2))
haxby.s1.test.indices <- which(!(1:(dim(haxby.s1$x)[1]) %in% haxby.s1.train.indices))
haxby.s1.x.train <- haxby.s1$x[haxby.s1.train.indices,]
haxby.s1.x.test <- haxby.s1$x[haxby.s1.test.indices,]
haxby.s1.y.train <- as.integer(haxby.s1$y[haxby.s1.train.indices])
haxby.s1.y.test <- as.integer(haxby.s1$y[haxby.s1.test.indices])

dnn.haxby.s1 <- dbn.dnn.train(haxby.s1.x.train, haxby.s1.y.train, hidden = c(5, 5)) ## predict by dnn
res <- nn.test(dnn.haxby.s1, haxby.s1.x.test, haxby.s1.y.test)
y_p <- nn.predict(dnn.haxby.s1,haxby.s1.x.test)
table(y_p)
#looking at the test function it's now clear to me that this is just designed to train the difference between two classes.
#well...can it do that?


#create a new object with only two categories.
haxby.s1.bivariate.comparison.indices <- which(haxby.s1$y %in% c("scissors","face"))
haxby.s1.bivariate <- haxby.s1
haxby.s1.bivariate$x <- haxby.s1$x[haxby.s1.bivariate.comparison.indices,]
#need to put into integer format and also convert into zeros and ones.
haxby.s1.bivariate$y <- as.integer(
  as.integer(haxby.s1$y[haxby.s1.bivariate.comparison.indices])
  ==max(as.integer(haxby.s1$y[haxby.s1.bivariate.comparison.indices]))
  )
haxby.s1.bivariate$run <- haxby.s1$run[haxby.s1.bivariate.comparison.indices]

#OK now create the bivariate sets.
haxby.s1.bivariate.train.indices <- sample(1:dim(haxby.s1.bivariate$x)[1],floor(dim(haxby.s1.bivariate$x)[1]/2))
haxby.s1.bivariate.test.indices <- which(!(1:(dim(haxby.s1.bivariate$x)[1]) %in% haxby.s1.bivariate.train.indices))
haxby.s1.bivariate.x.train <- haxby.s1.bivariate$x[haxby.s1.bivariate.train.indices,]
haxby.s1.bivariate.x.test <- haxby.s1.bivariate$x[haxby.s1.bivariate.test.indices,]
haxby.s1.bivariate.y.train <- as.integer(haxby.s1.bivariate$y[haxby.s1.bivariate.train.indices])
haxby.s1.bivariate.y.test <- as.integer(haxby.s1.bivariate$y[haxby.s1.bivariate.test.indices])

dnn.haxby.s1 <- dbn.dnn.train(haxby.s1.bivariate.x.train, haxby.s1.bivariate.y.train
                              , hidden = c(5,5)
                              , numepochs = 100) ## predict by dnn
#res <- nn.test(dnn.haxby.s1, haxby.s1.bivariate.x.test, haxby.s1.bivariate.y.test)
y_p <- nn.predict(dnn.haxby.s1,haxby.s1.bivariate.x.test)
table(y_p)

