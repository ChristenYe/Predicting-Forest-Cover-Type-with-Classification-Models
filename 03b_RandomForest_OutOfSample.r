library(dplyr)
library(magrittr)
library(tibble)
library(mlbench)
library(caret)
library(randomForest)

## Read Dataset.

forestD = read.csv("Transformed_Data.csv")


## Split into train and test.
nsamp = nrow(forestD)
set.seed(447)
iperm = sample(nsamp) # random sample of size ntrain
ntrain=round(0.7*nsamp) 
itrain= iperm[1:ntrain]

train = forestD[itrain,]  # training set (70%)
test = forestD[-itrain,]  # training set (30%) (5670)

## Make Cover_Type as a factor.
train$Cover_Type = as.factor(train$Cover_Type)
test$Cover_Type = as.factor(test$Cover_Type)

## 50% and 80% PIs based on lecture notes.
## Credit to our professor Harry.
CategoryPredInterval = function(ProbMatrix, labels)
{ ncases = nrow(ProbMatrix)
pred50 = rep(NA,ncases); pred80 = rep(NA,ncases)
for(i in 1:ncases)
{ p = ProbMatrix[i,]
ip = order(p,decreasing=T)
pOrdered = p[ip] # decreasing order
labelsOrdered = labels[ip] # decreasing order
G = cumsum(pOrdered) # cumulative sum from largest
k1 = min(which(G>=0.5))  # level1= 0.5
k2 = min(which(G>=0.8))  # level2= 0.8
pred1 = labelsOrdered[1:k1]; pred2 = labelsOrdered[1:k2]
pred50[i] = paste(pred1,collapse="")
pred80[i] = paste(pred2,collapse="")
}
list(pred50=pred50, pred80=pred80)
}

## Get the 50% and 80% PIs for each class.
rf = randomForest(Cover_Type~., data=train, importance=TRUE, proximity=TRUE)
rf_probs = predict(rf, newdata=test, type = "prob")
predIntRF <- CategoryPredInterval(rf_probs, labels=c("1","2","3","4","5","6","7"))
print(table(test$Cover_Type,predIntRF$pred50))
print(table(test$Cover_Type,predIntRF$pred80))

## Variable importance.
importance_table <- round(importance(rf), 2)
sorted_importance_table <- importance_table[order(importance_table[, "MeanDecreaseGini"], decreasing = TRUE),]
print(sorted_importance_table)