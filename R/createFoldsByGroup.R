
#' Intended for use with caret; creates folds based on the group allocations
#' Use in place of createFolds
#' @param group.allocation a vector describing which group/fold each item belongs to.
#' @export
#' @examples
#' createFoldsByGroup(c(1,1,1,1,2,2,2,2,3,3,3))
#' #create 3 folds; two with 4 members each and the third with 3 members.
#' 
createFoldsByGroup <- function(group.allocation){
  #group.allocation <- runs.rc
  fold.list <- list()
  fold.names <- unique(group.allocation)
  for (f in 1:length(fold.names)){
    #training.obs <- group.allocation!=f
    testing.obs <-which(group.allocation==f)
    
    fold.list[[paste0("Fold",
                      formatC(f,
                              width=floor(log(length(fold.names),10))+1,
                              flag = "0"))]] <-
      testing.obs
  }
  return(fold.list)

}