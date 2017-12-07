
#' Intended for use with caret; specifies a caret model specification for use by companion function
#' caret.train.model.list
#' There is a list of models here: http://topepo.github.io/caret/modelList.html
#' @param method method to be passed to train {caret}
#' @param tuning tuning method to be passed to train {caret} etiher a tuneGrid data frame or an integer to be passed to tuneLength
#' @param ... Values that will be passed directly to the function; in caret, these are values that aren't supported as tuning parameters.
#' @export
#' @examples
#' get.caret.model.spec("knn",tuning = 5, preProcess = "pca")
#' #get a knn model spec, with 5 a tuneLength of 5, and use PCA pre-processing.
#' 
get.caret.model.spec <- function(method,tuning=NULL,preProcess=NULL, ...){
  return(list(
    method=method,tuning = tuning,preProcess = preProcess, params = list(...)))
}