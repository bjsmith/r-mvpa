#' Detects and registers the number of machine cores. This should be run before using cross-validation with caret
#' Will allow R to use the doMC package to utilize the machine's multiple processors.
#' You will need to call this function for it to be applied.
#' @export
#' @examples
#' register.machine.cores()
#' 
register.machine.cores <- function(){
if (.Platform$OS.type!="windows"){
  machine.cores <- detectCores(all.tests = FALSE, logical = FALSE)
  if(!is.na(machine.cores)){
    print(paste0("Registering ", as.character(machine.cores), " machine cores with doMC"))
    registerDoMC(cores=machine.cores)
  }
}else
{
  print("Couldn't register multiple cores; this feature is currently not available on windows-based systems.")
}
}