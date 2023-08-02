library(dplyr)
library(mlbench)
library(rpart) 
library(rpart.plot)
library(pROC)
library(caret)


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

ctree = rpart(Cover_Type~., data=train, method = 'class')
ctree_probs = predict(ctree, newdata=test,type="prob")
predIntCtree = CategoryPredInterval(ctree_probs, labels=c("1","2","3","4","5","6","7"))
print(table(test$Cover_Type, predIntCtree$pred50))
print(table(test$Cover_Type, predIntCtree$pred80))