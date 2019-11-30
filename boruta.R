# Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# Data
data("Sonar")
str(Sonar)

# Feature Selection
set.seed(111)
boruta <- Boruta(Class ~ ., data = Sonar, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(Sonar), replace = T, prob = c(0.6, 0.4))
train <- Sonar[ind==1,]
test <- Sonar[ind==2,]

# Random Forest Model
set.seed(333)
rf60 <- randomForest(Class~., data = train)

# Prediction & Confusion Matrix - Test
p <- predict(rf60, test)
confusionMatrix(p, test$Class)
