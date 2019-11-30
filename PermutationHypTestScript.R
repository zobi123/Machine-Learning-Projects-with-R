### PERMUTATION HYPOTHESIS TEST ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="/Users/OldMarin/Desktop/VIDEOS/R Videos Project/Bootstrap Hyp Tests/ChickData.csv", header=T, sep=",")
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

# calculate the difference in sample MEANS
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# lets calculate the absolute diff in means
test.stat1 <- abs(mean(d$weight[d$feed=="casein"]) - 
                    mean(d$weight[d$feed=="meatmeal"])) 
test.stat1


# calculate the difference in sample MEDIANS
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# lets calculate the absolute diff in medians
test.stat2 <- abs(median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
test.stat2

##########################
###  PERMUTATION TEST  ###
##########################

# let's permute...

# for reproducability of results
set.seed(1979)  
# the number of observations to sample
n <- length(d$feed)  
# the number of permutation samples to take
P <- 100000 
# the variable we will resample from 
#     (note, could use the labels(feed) too, and "shuffle this")
variable <- d$weight  

# initialize a matrix to store the permutation data
PermSamples <- matrix(0, nrow=n, ncol=P)
# each column is a permutation sample of data

# now, get those permutation samples, using a loop
# let's take a moment to discuss what that code is doing...
for(i in 1:P){
  PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
}

# we can take a quick look at the first 5 columns of PermSamples
PermSamples[, 1:5]

# now, let's run thru a loop, calculating the test-stats for 
# each sample
### NOTE: could do all these in ONE step...
###       my suggestion, first write code that works, then 
###       tighten it up and make it more efficient
###       (use an 'apply' statement, and write the function 
###        you want)
###    i keep code transparent for teaching purpose

# initialize vectors to store all of the Test-stats:
Perm.test.stat1 <- Perm.test.stat2 <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1[i] <- abs( mean(PermSamples[d$feed=="casein",i]) - 
                               mean(PermSamples[d$feed=="meatmeal",i]) )
  # calculate the perm-test-stat2 and save it
  Perm.test.stat2[i] <- abs( median(PermSamples[d$feed=="casein",i]) - 
                               median(PermSamples[d$feed=="meatmeal",i]) )
}

# before going too far with this, let's remind ourselves of 
# the TEST STATS
test.stat1; test.stat2
# and, take a look at the first 15 permutation-TEST STATS for 1 and 2
round(Perm.test.stat1[1:15], 1)
round(Perm.test.stat2[1:15], 1)

# and, let's calculate the permutation p-value...
# notice how we can ask R a true/false question...(for the first 15)
(Perm.test.stat1 >= test.stat1)[1:15]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
mean((Perm.test.stat1 >= test.stat1)[1:15])

#...calculate the p-value, for all P=100,000
mean(Perm.test.stat1 >= test.stat1)

# and, let's calculate the p-value for 
# option 2 of the test statistic (abs diff in medians)
mean( Perm.test.stat2 >= test.stat2)

# now, remember the difference between "statistical significance" 
# and "scientific significance"


########################
### SOME EXTRA STUFF ###
########################

# calculate the mean for each group
with(d, tapply(weight, feed, mean))
# calculate the difference in means for each group
abs( diff( with(d, tapply(weight, feed, mean)) ) )

# and calculate the median for each group
with(d, tapply(weight, feed, median))
# and, calculate the difference in medians 
abs( diff( with(d, tapply(weight, feed, median)) ) )

## let's take a look at the 3 "Classic" hyp tests we could 
# consider (each of which comes with their own limitations...)

# let's look at the Independent 2-sample t-test
# tests Ho: means are equal
t.test(d$weight~d$feed, paired=F, var.eq=F)  
# let's look at the Wilcoxon aka Mann-Whitney U 
# tests Ho: medians are equal
wilcox.test(d$weight~d$feed, paired=F)  
# let's look at the Kolmogorov-Smirnov 2-sample test
# tests Ho: distributions are same
ks.test(d$weight[d$feed=="casein"], d$weight[d$feed=="meatmeal"], paired=F)     

### produce a plot that shows the sampling distribution, and p-value
### for the Permutation approach, for test.stat1
# let's take a look at a density plot of all the Permutation 
# test-stats, and add in our Observed test stat
plot(density(Perm.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

######
######

### Below is code to do the same as Permutation test as above, except 
### this time to shuffle the "labels" (the factor feed type), rather 
### than shuffle the observations
###  NOTE: numeric answer will change slightly, as this will end
###        up with a different set of permutations as above...

# for reproducability of results
set.seed(12345)  
# the number of observations to sample
n <- length(d$feed)  
# the number of permutation samples to take
P <- 10000  
# the variable we will resample from 
#     (note, could use the labels(feed) too, and "shuffle this")
variable <- d$feed 

### NOTE, we are going to sample with replacement from "feed" this 
###    time, and not from "weight"

# initialize a matrix to store the permutation data
PermSamplesOther <- matrix(0, nrow=n, ncol=P)
# each column is a permutation sample of data

# now, get those permutation samples, using a loop
for(i in 1:P){
  PermSamplesOther[,i] <- sample(variable, size= n, replace=FALSE)
}
# let's take a moment to discuss what that code is doing...
dim(PermSamplesOther)
# and look to see the first 5 columns of this...
PermSamplesOther[,1:5]
# note that here "1"="casein" and "2"="meatmeal"

# now, let's run thru a loop, calculating the test-stats for 
# each sample.  i keep code transparent for teaching purpose

# initialize vectors to store all of the Test-stats:
Perm.test.stat1b <- Perm.test.stat2b <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1b[i] <- abs( mean(d$weight[PermSamplesOther[,i]=="1"]) - 
                                mean(d$weight[PermSamplesOther[,i]=="2"]) )
  # calculate the perm-test-stat2 and save it
  Perm.test.stat2b[i] <- abs( median(d$weight[PermSamplesOther[,i]=="1"]) - 
                                median(d$weight[PermSamplesOther[,i]=="2"]) )
}

# before going too far with this, let's remind ourselves of 
# the TEST STATS
test.stat1; test.stat2
# and, take a look at the first 15 permutation-TEST STATS for 1 and 2
round(Perm.test.stat1b[1:15], 1)
round(Perm.test.stat2b[1:15], 1)

# let's take a look at a density plot of all the Permutation 
# test-stats, and add in our Observed test stat
plot(density(Perm.test.stat1b), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

# and, let's calculate the permutation-test p-value...
mean( Perm.test.stat1b >= test.stat1)

# and, let's skip the plots, and just calculate the p-value for 
# option 2 of the test statistic (abs diff in medians)
mean( Perm.test.stat2b >= test.stat2)
