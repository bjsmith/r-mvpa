#http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#source("./R/common.R")
setwd("./")
getwd()
document()

#problem with R check errors seems to want to actually run functions while testing them.
#if I haven't defined a default value, I get an error message
#currently occuring with align.3D.img.in.2D
#http://stackoverflow.com/questions/5058347/object-not-found-error-when-creating-a-new-geom-for-a-package
#http://r.789695.n4.nabble.com/R-CMD-check-package-quot-mypkg-Ex-R-quot-failed-td4668443.html

#so R build check now seems to work.
#I can't work out why there's no directory created. It does seem to be creating a compressed file in the appropriate directory
#but that is deleted upon completion of the run. Perhaps we just need to get rid of all the errors...
#or maybe it's wanting something else.
#here's the solution!:
#http://stackoverflow.com/questions/26979968/after-running-r-cmd-check-the-mypackage-rcheck-directory-with-pdf-manual-disapp
