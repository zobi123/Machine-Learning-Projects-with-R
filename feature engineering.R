Let's start with applying random forest for all the features on the dataset first.

library('Metrics')
library('randomForest')
library('ggplot2')
library('mosaic')
library('ggthemes')
library('dplyr')

#set random seed

set.seed(101) # reproduce of code

#loading dataset

data<-read.csv(file.choose(),stringsAsFactors= T)

#checking dimensions of data

dim(data)

## [1] 3000(rows)  101(attribute/columns)
#specifying outcome variable as factor

data$Y<-as.factor(data$Y)
data$Time<-NULL

#dividing the dataset into train and test
train<-data[1:2000,]
test<-data[2001:3000,]
#applying Random Forest
# Random Forest is ensembling machine machine learning.It is combination of decision tree algorithm. 
It is used for classification and regression problem.

model_rf<-randomForest(Y ~ ., data = train)

preds<-predict(model_rf,test[,-101])

table(preds)

#checking accuracy

auc(preds,test$Y)

#Let's look at the feature importance:
  
importance(model_rf)
applying Random forest for most important 20 features only

model_rf<-randomForest(Y ~ X55+X11+X15+X64+X30
                       +X37+X58+X2+X7+X89+X31+X66+X40+X12+X90
                       +X29+X98+X24+X75+X56,
                       data = train)

preds<-predict(model_rf,test[,-101])
table(preds)
#checking accuracy
auc(preds,test$Y)

