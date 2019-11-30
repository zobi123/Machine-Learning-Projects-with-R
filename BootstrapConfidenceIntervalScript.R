##### BOOTSTRAP CONFIDENCE INTERVAL #####
###  (comparing 2 numeric variables)  ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="~/Desktop/VIDEOS/R Videos Project/Bootstrap Hyp Tests/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
levels(d$feed)
# how many observations in each diet?
table(d$feed)

# let's look at a boxplot of weight gain by those 2 diets
boxplot(d$weight~d$feed, las=1, ylab="weight (g)", 
        xlab="feed",main="Weight by Feed")

# calculate the difference in sample means
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, mean))
# lets calculate the diff in means:   (casein - meatmeal)
Obs.Diff.In.Means <- (mean(d$weight[d$feed=="casein"]) - mean(d$weight[d$feed=="meatmeal"]))  #diff in means
Obs.Diff.In.Means
# and, a fanceir way to do that...  (- to have it be casein-meatmeal)
-diff( with(d, tapply(weight, feed, mean)) ) 

# and, the same for the medians
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, median))
# lets calculate the diff in medians:  (casein - meatmeal)
Obs.Diff.In.Medians <- (median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
Obs.Diff.In.Medians
# and, a fanceir way to do that...  (- to have it be casein-meatmeal)
-diff( with(d, tapply(weight, feed, median)) ) 

###################################
### BOOTSTRAP CONFIDENCE INTERVAL
###################################

# let's run through making conf ints for the difference in means and medians

# let's bootstrap...
set.seed(13579)   # set a seed for consistency/reproducability
n.c <- 12  # the number of observations to sample from casein
n.m <- 11  # the number of observations to sample from meatmeal
B <- 100000  # the number of bootstrap samples...go big or go home right?

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.casein <- matrix( sample(d$weight[d$feed=="casein"], size= B*n.c, 
                              replace=TRUE), ncol=B, nrow=n.c)
Boot.meatmeal <- matrix( sample(d$weight[d$feed=="meatmeal"], size= B*n.m, 
                                replace=TRUE), nrow=n.m, ncol=B)
# check those
dim(Boot.casein); dim(Boot.meatmeal)

# check to make sure they are not empty!
Boot.casein[1:5,1:5]
Boot.meatmeal[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.Diff.In.Means <- colMeans(Boot.casein) - colMeans(Boot.meatmeal)
# check that
length(Boot.Diff.In.Means)
# and, look at the first 10 diff in means
Boot.Diff.In.Means[1:10]

# calculate the difference in MEDIANS for each of the bootsamples
Boot.Diff.In.Medians <- apply(Boot.casein, MARGIN=2, FUN=median) -
  apply(Boot.meatmeal, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians[1:10]

#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# let's look at the PERCENTILE METHOD
# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
quantile(Boot.Diff.In.Means, prob=0.025)
quantile(Boot.Diff.In.Means, prob=0.975)

# and then, the difference in MEDIANS
quantile(Boot.Diff.In.Medians, prob=0.025)
quantile(Boot.Diff.In.Medians, prob=0.975)

### What do you make of the fact that these both cross 0?

### Apart from "statistical significance", what do you think about
###    "scientific significance" here?

# below is code to calculate confidence interval using the BASIC method

# let's look at the BASIC METHOD
# first, for the difference in MEANS
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.975)
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.025)
# and then, the difference in MEDIANS
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.975)
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.025)

########
### Code for confidence interval for difference in 80th percentiles
########

# calculate the observed difference in 80th percentiles
Obs.Diff.In.80per <- (quantile(d$weight[d$feed=="casein"], prob=0.80) - quantile(d$weight[d$feed=="meatmeal"], prob=0.80))
Obs.Diff.In.80per

# calculate the difference in 80th percentile for each of the bootsamples
Boot.Diff.In.80per <- apply(Boot.casein, MARGIN=2, FUN=quantile, prob=0.80) - apply(Boot.meatmeal, MARGIN=2, FUN=quantile, prob=0.80)

# let's look at the PERCENTILE METHOD for the difference in 80th percentile
quantile(Boot.Diff.In.80per, prob=0.025)
quantile(Boot.Diff.In.80per, prob=0.975)
