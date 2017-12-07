#This file is called by every item in this package and loads some essential
#packages.
#https://github.com/STAT545-UBC/Discussion/issues/71
#As in http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
if("devtools" %in% rownames(installed.packages()) == FALSE){
  install.packages("devtools")
}
require("devtools")

if("roxygen2" %in% rownames(installed.packages()) == FALSE){
  devtools::install_github("klutometis/roxygen")
}
require(roxygen2)

required.packages=c(#"bnlearn"#"ISLR","e1071","MASS","class"
                    #,"qGrain"#"tree","randomForest","gbm"
                    #,"pls"
                    #                    ,"caret"
                    #,"ggplot2","gridExtra","lubridate"
)
for (package in required.packages){
  if(package %in% rownames(installed.packages()) == FALSE){
    n <- readline(paste("Package",package,
                        "required. Type 'y' and press",
                        " enter to install or any other input to quit."))
    if(n=="y"){
      install.packages(package,dep=TRUE)
    }else{
      stop(paste("Required package",package,"not installed."))}}
  require(package,character.only=TRUE)
}

catnl <- function(text){
  cat(text)
  cat("\n")
}
verbose <- FALSE
printv <- function(text){
  if (verbose){
    print(text)
  }
}