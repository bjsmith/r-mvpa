#let's try the seeds example dataset.

seeds <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",
                  header=F,sep="\t")
#exclude class values that aren't integers
#seeds.cleaned1=seeds[!(seeds[, 8] %% 1>0), ]
seeds.cleaned1=seeds[(seeds[, 8] %in% c(1,2,3)), ]
seeds.cleaned = seeds.cleaned1[!is.na(seeds.cleaned1[, 8]), ]
seedstrain<- sample(1:dim(seeds.cleaned)[1],dim(seeds.cleaned)[1]*3/4)
seedstest <- setdiff(1:dim(seeds.cleaned)[1],seedstrain)
ideal <- class.ind(seeds.cleaned[, 8])
#colnames(ideal) <- 1:10
df <- data.frame(ideal, seeds.cleaned[,-8])
#trim out rare values so we have fewer classes than predictors
table(seeds.cleaned[, 8])


nn = neuralnet(as.formula(paste(paste(colnames(df)[1:3],collapse=" + "),"~",paste(colnames(df)[4:10],collapse=" + "))),
                     data = df,
                     hidden=2)

predict(nn,seeds.cleaned[,-8])
nn$result.matrix

plot(nn)