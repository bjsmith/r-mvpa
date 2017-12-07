myf <- function(x,...){
 print(x)
 val <- c(...)
 print(...)
}
myf(4,5.12353148089,digits=2)

#How to save an ellipsis and then pass its value to a function
#there are limits to this! might not work consistently, but it should work
#for my purposes
myf <- function(x,...){
  print(x)
  val <- c(...)
  print(val)
  print(7.1534,unlist(val))
  vals<-unlist(val)
  print(7.1534,vals)
  print(7.1534,vals)
}
mylist<-list(digits=2)
myf(4,list(digits=2))
myf(4,mylist)
myf(4)#seems to do OK when you don't pass anything, too :-)


print(5.12353148089,digits=2)
args <- list("digits"=2)
round(3.14159,args)

formals

#13:35


#overloading
#http://stackoverflow.com/questions/20812208/is-there-function-overloading-in-r
print.SOexample1 <- function(x, ...) {
  cat("Your values:\n============", 
      format(x, width = 6), sep = "\n>>> : ")
  invisible(x)
}

A <- 1:5
class(A) <- "SOexample1"
print.SOexample1(A)
print(A)
print(unclass(A))

#foobar(c(1,2,2,3,4,5,10))
#foobar generic 
#http://r-pkgs.had.co.nz/man.html
#' Foo bar generic
#'
#' @param x Object to foo.
foobar <- function(x) UseMethod("foobar")

#' @describeIn foobar Difference between the mean and the median
foobar.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn foobar First and last values pasted together in a string.
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])

