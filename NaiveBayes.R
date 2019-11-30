#Read in the spam data from the uci website separating by commas
spam = read.csv(
  'https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data')
#Transforming the data type to a matrix and removing the column names
spam = as.matrix(spam)
colnames(spam) = NULL

#Creating a random sampling of indices to split the train and test sets
samp = sample(1:nrow(spam),2300)

#The training and test data sets
train = spam[samp,]
test = spam[-samp,]

#Function that performs the naive bayes classification using gaussian 
#probabilites
naiveBayes = function(data){
  #Calculating the prior probability for the zero class
  p0.data = table(data[,58])[[1]]/nrow(data) 
  #Calculating the prior probability for the one class
  p1.data = table(data[,58])[[2]]/nrow(data)
  
  #Separating the zero class data from the one class data an finding
  #the mean and standard devation for each factor
  data0 = data[data[,58] == 0,-58] 
  data0.means = apply(data0,2,mean)
  data0.sds = apply(data0,2,sd)
  data0.sds[data0.sds==0] = 1e-4
  
  #Separating the one class data from the zero class data an finding
  #the mean and standard devation for each factor
  data1 = data[data[,58] == 1,-58]
  data1.means = apply(data1,2,mean)
  data1.sds = apply(data1,2,sd)
  data1.sds[data1.sds==0] = 1e-4
  
  #Creating a list to house the classifications
  classes = c()
  
  #Running the classification on each observation in the data set
  for(i in 1:nrow(data)){
    
    #Function gauss takes in a row of data (x), the mean (m) for each factor,
    #and the standard deviation (s) for each factor
    #The function uses the above information of the find all of the
    #probabilities (densities) for each row of the data by factor
    gauss = function(x,m,s){
      p = 1/( sqrt(2*pi) * s ) * exp( -1*( (x-m)^2 / (2*(s)^2)) )
      return(p)
    }
    
    #Using the gauss function on the ith row of the data omitting the 
    #classifier column to find the probabilities for the zero class
    p0 = gauss(data[i,-58],data0.means,data0.sds)
    #Using the gauss function on the ith row of the data omitting the 
    #classifier column to find the probabilities for the one class
    p1 = gauss(data[i,-58],data1.means,data1.sds)
    
    #Finding the overall probability of p(0), and p(1) by taking the log of the 
    #each prior multiplied by the sum of the log of each factors probability 
    #for ith row of data
    p0.final = log(p0.data)+sum(log(p0))
    p1.final = log(p1.data)+sum(log(p1))
    
    #Classifiying the class bases on which of the two probabilites p(0), p(1)
    #is larger
    argmax = c('p0.final','p1.final')[which.max(c(p0.final,p1.final))]
    classes = c(classes, ifelse(argmax=='p0.final',0,1))
  }
  #returing a list of classified values
  return(classes)
}

#using the gmbomt function on the training data
train.pred = naiveBayes(train)
#using the gmbomt function on the test data
test.pred = naiveBayes(test)

#Function  to generate true positives, true negatives,
#false positives, and false negatives
metrics = function(actual,estimated){
  p.ind = which(actual[,58]==1)
  tp = sum(actual[p.ind,58]==estimated[p.ind])
  tn = sum(actual[-p.ind,58]==estimated[-p.ind])
  fp = sum(actual[-p.ind,58]!=estimated[-p.ind])
  fn = sum(actual[p.ind,58]!=estimated[p.ind])
    return(list('tp'=tp,'tn'=tn,'fp'=fp,'fn'=fn))
}

#Accuracy, precision, recall, confusion matrix on the test

#using the metrics function on the test data
test.met = metrics(test,test.pred)
#accuracy of the test data using tp,tn,fp,fn
test.acc = (test.met$tp + test.met$tn) / (test.met$tp + test.met$tn + test.met$fp + test.met$fn)
#precision of the test data
test.prec = test.met$tp / (test.met$tp + test.met$fp)
#recall of the test data
test.rec = test.met$tp / (test.met$tp + test.met$fn)
#creating a confusion matrix of the test data 
test.confuse = matrix(c(test.met$tp,test.met$fn,test.met$fp,test.met$tn),2,2,byrow = T)
colnames(test.confuse)=c('1','0')
row.names(test.confuse) = c('1','0')

print('accuracy'); test.acc
print('precision'); test.prec
print('recall'); test.rec
print('confusion matrix'); test.confuse

