library(dplyr)
library(caret)
library(naivebayes)

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


## Based on the 5-fold cross-validated results, we used a subset of 6 features 
## for Naive Bayes Classifier
myvars <- c(    "Elevation",
                "Horizontal_Distance_To_Roadways",
                "Horizontal_Distance_To_Fire_Points",
                "Horizontal_Distance_To_Hydrology",
                "Hillshade_9am",
                "Aspect",
                "Cover_Type")

train <- train[myvars]
test <- test[myvars]
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
nbc = naive_bayes(Cover_Type~., data=train, usekernel = T)
nbc_probs = predict(nbc, newdata=test, type = "prob")
predIntRF <- CategoryPredInterval(nbc_probs, labels=c("1","2","3","4","5","6","7"))
print(table(test$Cover_Type,predIntRF$pred50))
print(table(test$Cover_Type,predIntRF$pred80))
