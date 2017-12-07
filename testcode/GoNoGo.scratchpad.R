library(mvpa)

sub104 <- read.image(file="/../mnt//data/GoNoGo/sub104.nii.gz")
dim(sub104)
#cool, we got it! Now we just need the code describing which images are which....
#Feng should already have created these for his contrasts.
#OK now we need to take a look at the EV files

read.textfile <- function(fileName){
  return(readChar(fileName, file.info(fileName)$size))
}
nuisance <- read.textfile("/mnt/data/MSM/sub107_gonogo_run1_nuisance_ev.txt")
sub.104 <- read.textfile("/mnt/data/MSM/sub104_gonogo_run1_male_go_ev.txt")
sub.104.male.go.ts <- read.csv("/mnt/data/MSM/sub104_gonogo_run1_male_go_ev.txt",sep="\t",header=F)
sub.104.female.go.ts <- read.csv("/mnt/data/MSM/sub104_gonogo_run1_female_go_ev.txt",sep="\t",header=F)
sub.104.male.nogo.ts <- read.csv("/mnt/data/MSM/sub104_gonogo_run1_male_nogo_ev.txt",sep="\t",header=F)
sub.104.female.nogo.ts <- read.csv("/mnt/data/MSM/sub104_gonogo_run1_female_nogo_ev.txt",sep="\t",header=F)
dim(sub104)
#now if we're doing classification, 
#we need to classify each of the images in sub104
#based on the contents of sub.104.male

time.series.offset <- 0
bold.offset <- 5
#soo...to project the time points into TR time, we need to divide by 2.
#and add the offset.


#then create a parallel time series for the images from this
time.points <- rep("rest",dim(sub104)[4])
#take the first image that begins after the stimulus is presented, plus the bold offset
time.points[ceiling((sub.104.male.go.ts$V1+time.series.offset+bold.offset)/2)] <- "male_go"
time.points[ceiling((sub.104.female.ts$V1+time.series.offset+bold.offset)/2)] <- "female_go"
time.points[ceiling((sub.104.male.nogo.ts$V1+time.series.offset+bold.offset)/2)] <- "male_nogo"
time.points[ceiling((sub.104.female.nogo.ts$V1+time.series.offset+bold.offset)/2)] <- "female_nogo"
#well, perhaps that's a start. we can try to classify now...just need to mask appropriately. striatum? visual cortex????
#then label time points that fall within the range of a stimulus (as marked in sub.104.male.ts)
# as relevant stimuli...we'll take the time point 5-6 seconds from stimulus presentation

#Time.points may have extended beyond the length of the image ts itself
#This is due to the offset, particularly the BOLD offset bu also any time series offset applied.
time.points = time.points[1:dim(sub104)[4]] #trim back to the legnth of the actual time series
time.points = as.factor(time.points) #and convert into a factor.

# If you are looking for EVs for each condition, you may use below for correct trials:
#   sub104_gonogo_run1_male_go_ev
# sub104_gonogo_run1_male_nogo_ev
# sub104_gonogo_run1_female_go_ev
# sub104_gonogo_run1_female_nogo_ev
# 
# and below for error trials:
#   sub104_gonogo_run1_nuisance_male_go_ev
# sub104_gonogo_run1_nuisance_male_nogo_ev
# sub104_gonogo_run1_nuisance_female_go_ev
# sub104_gonogo_run1_nuisance_female_nogo_ev
# 
# If you are looking for combined ev for go trials, you may want to combine sub104_gonogo_run1_male_go_ev and sub104_gonogo_run1_female_go_ev. Other files were for different purpose that was abandoned a long time ago.

#now, we need a mask for these images. I wonder if they're converted into standard space?
#need to examine a little in FSLview, although could actually do it here.
sub104.avg <- apply(sub104,c(1,2,3),mean)
dim(sub104.avg)
display.3D.img.in.2D(sub104.avg,3)
#we could actually try with the same mask we used for the haxby data.
mask4.vt.raw <- read.image(file="/../mnt//data/mask4_vt.nii.gz")
display.3D.img.in.2D(mask4.vt,3)
dim(mask4.vt)
#change mask into the space occupied by sub104
mask4.vt <- abind(aperm(mask4.vt.raw,c(2,3,1)),array(0,dim(mask4.vt.raw)[2:3]))
dim(mask4.vt)
img.in.2D=t(align.3D.img.in.2D(sub104.avg,3))
additive.in.2D=t(align.3D.img.in.2D(mask4.vt,3))*max(img.in.2D)+img.in.2D
display.2D.img(abind(img.in.2D,img.in.2D,additive.in.2D,along=3))

mask4.V2 <- read.image(file="/../mnt/data/bilateral-GM-V2-BA18.nii.gz")
dim(mask4.V2)
subj1 <- read.image(file = "/../mnt/data/haxby2001/subj1/bold.nii.gz")

#seems like the current GoNoGo data isn't in standard space...
#if we were to get it there, we'd use:
#flirt http://fsl.fmrib.ox.ac.uk/fsl/fsl-4.1.9/flirt/examples.html
#followed by applywarp: http://fsl.fmrib.ox.ac.uk/fsl/fsl-4.1.9/fnirt/combining_warps.html

#can we just get a copy from Feng which is already in the right space?
sub.104.ss <- read.image(file = "/../mnt/data/filtered_func_data.nii.gz")
#alternative is to transform the mask into anatomical space; I think this is what we did last time.
#in fact, shouldn't we have this somehwere?
sub.104.mask.occipital <- read.image(file = "testcode/occipital_pole_example_func.nii.gz")
dim(sub.104.mask)
dim(sub.104.ss)
img.in.2D=t(align.3D.img.in.2D(sub104.avg,3))
additive.in.2D=t(align.3D.img.in.2D(sub.104.mask.occipital,3))*max(img.in.2D)+img.in.2D
display.2D.img(abind(img.in.2D,img.in.2D,additive.in.2D,along=3))
sum(sub.104.mask.occipital)
sum(mask4.V2)

#OK cool. Let's try with this mask. 
#so, to clean up on what we've got above, let's recoup and start from the start
sub.104.ts <- read.image(file = "/../mnt/data/filtered_func_data.nii.gz")
sub.104.mask.occipital <- read.image(file = "testcode/occipital_pole_example_func.nii.gz")

display.img.w.mask <- function(img4d,mask){
  img.avg <- apply(img4d,c(1,2,3),mean)
  img.in.2D=t(align.3D.img.in.2D(sub104.avg,3))
  additive.in.2D=t(align.3D.img.in.2D(mask,3))*max(img.in.2D)+img.in.2D
  display.2D.img(abind(img.in.2D,img.in.2D,additive.in.2D,along=3))
}
display.img.w.mask(sub.104.ts,sub.104.mask.occipital)

#cool. now, we want to pass it into the framework we have
sub.104 <- load.preprocess.subject("/../mnt/data/filtered_func_data.nii.gz"
                                   ,"testcode/occipital_pole_example_func.nii.gz"
                                   ,run.factor.list = data.frame(labels=time.points,chunks=0))

#OK, now we have this in the right format, we can proceed as previously.
display.3D.img.in.2D(sub.104$img.average,3)
register.machine.cores()
svm.spec <- get.caret.model.spec("svmLinear", preProcess="pca")
#lda.spec <- get.caret.model.spec("lda", preProcess = "pca")
knn.spec <- get.caret.model.spec("knn",tuning = 5, preProcess = "pca")
non.nnet.methods <- list(svm.spec, lda.spec, knn.spec)

res <- caret.train.model.list(
  x=sub.104$x
  ,y=sub.104$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = non.nnet.methods
)

summary(res)
results <- resamples(res)
dotplot(results)
sub.104$run
sub.104$y

#OK, what if we modified the classes so that we are only looking at male vs. female
time.points.mf <- as.character(time.points)
time.points.mf[time.points=="male_go"] <- "male"
time.points.mf[time.points=="male_nogo"] <- "male"
time.points.mf[time.points=="female_go"] <- "female"
time.points.mf[time.points=="female_nogo"] <- "female"

sub.104.mf <- load.preprocess.subject("/../mnt/data/filtered_func_data.nii.gz"
                                   ,"testcode/occipital_pole_example_func.nii.gz"
                                   ,run.factor.list = data.frame(labels=time.points.mf,chunks=0))


res <- caret.train.model.list(
  x=sub.104.mf$x
  ,y=sub.104.mf$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = non.nnet.methods
)

summary(res)
results <- resamples(res)
dotplot(results)
table(time.points.mf)
#so we can't get our accuracy above about 60%; this is the same experience i had running mvpa in python.
res[[1]]

#what if we were classifying everything that occured 5-10 seconds after a male stimulus as male?
#this would be on the basis that we generally get more activity for the males than females...
time.points.malebias <- rep("rest",dim(sub104)[4])
#take the first image that begins after the stimulus is presented, plus the bold offset
time.points.malebias[ceiling((sub.104.female.ts$V1+time.series.offset+bold.offset)/2)] <- "female_go"
time.points.malebias[ceiling((sub.104.female.nogo.ts$V1+time.series.offset+bold.offset)/2)] <- "female_nogo"
#add an extra female go and no-go
time.points.malebias[intersect(which(time.points.malebias=="female_go")+1,which(time.points.malebias=="rest"))]="female_go"
time.points.malebias[intersect(which(time.points.malebias=="female_nogo")+1,which(time.points.malebias=="rest"))]="female_nogo"
#now override with the male items
time.points.malebias[ceiling((sub.104.male.go.ts$V1+time.series.offset+bold.offset)/2)] <- "male_go"
time.points.malebias[ceiling((sub.104.male.nogo.ts$V1+time.series.offset+bold.offset)/2)] <- "male_nogo"
time.points.malebias[intersect(which(time.points.malebias=="male_go")+1,which(time.points.malebias %in% c("rest","female_go","female_nogo")))]="male_go"
time.points.malebias[intersect(which(time.points.malebias=="male_nogo")+1,which(time.points.malebias %in% c("rest","female_go","female_nogo")))]="male_nogo"
table(time.points.malebias)

time.points.malebias = time.points.malebias[1:dim(sub104)[4]] #trim back to the legnth of the actual time series


#now go to just the sexes
convert.time.points.cond.to.sex.only <- function(tp){
  time.points.mf <- tp
  time.points.mf[tp=="male_go"] <- "male"
  time.points.mf[tp=="male_nogo"] <- "male"
  time.points.mf[tp=="female_go"] <- "female"
  time.points.mf[tp=="female_nogo"] <- "female"
  return(time.points.mf)
}
time.points.malebias.mf <- as.factor(convert.time.points.cond.to.sex.only(time.points.malebias))
table(time.points.malebias.mf)
#right, now try classification again...
sub.104 <- load.preprocess.subject("/../mnt/data/filtered_func_data.nii.gz"
                                   ,"testcode/occipital_pole_example_func.nii.gz"
                                   ,run.factor.list = data.frame(labels=time.points.malebias.mf,chunks=0))

res <- caret.train.model.list(
  x=sub.104$x
  ,y=sub.104$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=57))
  ,training.list = non.nnet.methods
)
results <- resamples(res)
dotplot(results)
#maybe helps a little bit...but not a whole lot. and this could be attributed to this being an easier task...with more male values
#a strong bias toward male could generate a high score.

#what else could we do? Perhaps this data just isn't that good.
#we could:
#-try other subjects
#-try other masks
#-try other data inputs, e.g., no PCA, more PCA components, etc
summary(sub.104$x) #yes why is this only 270 voxels?
res <- caret.train.model.list(
  x=sub.104$x
  ,y=sub.104$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=100))
  ,training.list = non.nnet.methods
)
results <- resamples(res)
dotplot(results)
#I suppose I should try a neural net soon enough.

nnet.spec <- get.caret.model.spec(
  "nnet"
  , tuning = expand.grid(decay = c(0.3, 0.7), size = c(3,5,7))
  ,preProcess="pca")

method.list <- list(svm.spec, lda.spec, knn.spec, nnet.spec)
res.nnet <- caret.train.model.list(
  x=sub.104$x
  ,y=sub.104$y
  ,trControl = trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions=list(pcaComp=100))
  ,training.list = method.list
)
results <- resamples(res.nnet)
dotplot(results)
table(sub.104$y)
128/(88+128)
binom.test(c(128,88),n=100)

#maybe try searchlight analysis?
#http://mvpa.blogspot.com/2014/01/demo-r-code-to-perform-searchlight.html