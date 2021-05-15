library(stats)
library(rpart)
library(rpart.plot)
library(randomForest)

rm(list=ls())


set.seed(50)
mydata <- read.table("house-votes-84.data",header=FALSE, sep=",")
colnames(mydata) <- c("party", "handicapped","water", "budget", 
                      "physician", "elsalvador","religious", 
                      "antisatellite", "aidcontras", "missile", 
                      "immigration", "synfuels", "education", 
                      "sue", "crime", "dutyfree", "exportafrica")
attach(mydata)
obs = dim(mydata)[1] # number of observations/members of congress
k = dim(mydata)[2] # number of variables (before dummies)

# Create Dummy Variables

handicapped.y = 1*(handicapped=="y")
handicapped.n = 1*(handicapped=="n")

water.y = 1*(water=="y")
water.n = 1*(water=="n")

budget.y = 1*(budget=="y")
budget.n = 1*(budget=="n")

physician.y = 1*(physician=="y")
physician.n = 1*(physician=="n")

elsalvador.y = 1*(elsalvador=="y")
elsalvador.n = 1*(elsalvador=="n")

religious.y = 1*(religious=="y")
religious.n = 1*(religious=="n")

antisatellite.y = 1*(antisatellite=="y")
antisatellite.n = 1*(antisatellite=="n")

aidcontras.y = 1*(aidcontras=="y")
aidcontras.n = 1*(aidcontras=="n")

missile.y = 1*(missile=="y")
missile.n = 1*(missile=="n")

immigration.y = 1*(immigration=="y")
immigration.n = 1*(immigration=="n")

synfuels.y = 1*(synfuels=="y")
synfuels.n = 1*(synfuels=="n")

education.y = 1*(education=="y")
education.n = 1*(education=="n")

sue.y = 1*(sue=="y")
sue.n = 1*(sue=="n")

crime.y = 1*(crime=="y")
crime.n = 1*(crime=="n")

dutyfree.y = 1*(dutyfree=="y")
dutyfree.n = 1*(dutyfree=="n")

exportafrica.y = 1*(exportafrica=="y")
exportafrica.n = 1*(exportafrica=="n")


# Define Data Set - Y and X

Y = as.data.frame(1*(party=="democrat")) # Output variable as binary

X = cbind(handicapped.y, handicapped.n, water.y, water.n, budget.y, budget.n,
          physician.y, physician.n, elsalvador.y, elsalvador.n,
          religious.y, religious.n, antisatellite.y, antisatellite.n,
          aidcontras.y, aidcontras.n, missile.y, missile.n, immigration.y,
          immigration.n, synfuels.y, synfuels.n, education.y, education.n,
          sue.y,sue.n,crime.y,crime.n,dutyfree.y, dutyfree.n,
          exportafrica.y, exportafrica.n)

fulldataset = as.data.frame(cbind(Y,X))
colnames(fulldataset)[1] = "Y"

p = dim(X)[2]       # variables (after dummies)
nm = dim(X)[1]      # total number of observations, same as obs above
n = round(nm*2/3)   # number of training data observations
m = nm-n            # number of test data observations


# Create Test-Train Split 

train.index = sample(1:nm,size=n)
train = as.data.frame(X[train.index,])   # Train input data
test = as.data.frame(X[-train.index,])   # Test input data
Ytrain= Y[train.index,]   # Train output data
Ytest = Y[-train.index,]  # Test output data

X1train = cbind(train$handicapped.y, train$handicapped.n,train$water.y, 
                train$water.n,train$budget.y,train$budget.n, train$physician.y,  
                train$physician.n, train$elsalvador.y, train$elsalvador.n, 
                train$religious.y, train$religious.n, train$antisatellite.y, 
                train$antisatellite.n, train$aidcontras.y, train$aidcontras.n,
                train$missile.y, train$missile.n, train$immigration.y,
                train$immigration.n, train$synfuels.y, train$synfuels.n, 
                train$education.y, train$education.n, train$sue.y, train$sue.n, 
                train$crime.y, train$crime.n, train$dutyfree.y, train$dutyfree.n,
                train$exportafrica.y, train$exportafrica.n)

colnames(X1train) = cbind("handicapped.y","handicapped.n","water.y", 
                          "water.n","budget.y","budget.n", "physician.y",  
                          "physician.n", "elsalvador.y", "elsalvador.n", 
                          "religious.y", "religious.n", "antisatellite.y", 
                          "antisatellite.n", "aidcontras.y", "aidcontras.n",
                          "missile.y", "missile.n", "immigration.y",
                          "immigration.n", "synfuels.y", "synfuels.n", 
                          "education.y","education.n", "sue.y", "sue.n", 
                          "crime.y", "crime.n", "dutyfree.y", "dutyfree.n",
                          "exportafrica.y", "exportafrica.n")

#######            TRAIN MODELS            #######


traindata = as.data.frame(cbind(Ytrain,X1train))
xtrain = 1:dim(X1train)[1]

# Linear Probability Model           
trainmodel.lpm = lm(Ytrain~X1train)         

# Logistic Model
trainmodel.glm = glm(Ytrain~X1train, family=binomial, maxit=100)

# Classification Tree
rparttree = rpart(Ytrain~.,data=traindata, method="class", minbucket=5)
par(mar=c(1,1,1,1))
prp(rparttree)

# Bagging 
traindata$Ytrain = as.factor(traindata$Ytrain)
trainmodel.bag = randomForest(Ytrain~., data=traindata, type="class",
                              mtry=p, replace=TRUE, importance=TRUE)

# Random Forest
sqrtp = round(sqrt(p))
trainmodel.rdm = randomForest(Ytrain~., data=traindata, type="class",
                              mtry=sqrtp, replace=TRUE, importance=TRUE)




#######         TEST MODELS          #######


X1test = cbind(test$handicapped.y, test$handicapped.n,test$water.y, 
               test$water.n,test$budget.y,test$budget.n, test$physician.y,  
               test$physician.n, test$elsalvador.y, test$elsalvador.n, 
               test$religious.y, test$religious.n, test$antisatellite.y, 
               test$antisatellite.n, test$aidcontras.y, test$aidcontras.n,
               test$missile.y, test$missile.n, test$immigration.y,
               test$immigration.n, test$synfuels.y, test$synfuels.n, 
               test$education.y, test$education.n, test$sue.y, test$sue.n, 
               test$crime.y, test$crime.n, test$dutyfree.y, test$dutyfree.n,
               test$exportafrica.y, test$exportafrica.n)

colnames(X1test) = cbind("handicapped.y","handicapped.n","water.y", 
                         "water.n","budget.y","budget.n", "physician.y",  
                         "physician.n", "elsalvador.y", "elsalvador.n", 
                         "religious.y", "religious.n", "antisatellite.y", 
                         "antisatellite.n", "aidcontras.y", "aidcontras.n",
                         "missile.y", "missile.n", "immigration.y",
                         "immigration.n", "synfuels.y", "synfuels.n", 
                         "education.y","education.n", "sue.y", "sue.n", 
                         "crime.y", "crime.n", "dutyfree.y", "dutyfree.n",
                         "exportafrica.y", "exportafrica.n")

testdata = as.data.frame(cbind(Ytest,X1test))


# LPM 
phat.lpm = cbind(1,X1test) %*% trainmodel.lpm$coefficients
Yhat.lpm = 1*(phat.lpm>0.5)
err.lpm = sum(Yhat.lpm!=Ytest)

# LOGISTIC (GLM)
phat.glm = plogis(cbind(1,X1test) %*% trainmodel.glm$coefficients)
Yhat.glm = 1*(phat.glm>0.5)
err.glm = sum(Yhat.glm!=Ytest)

# BASIC TREE 
Yhat.tree = predict(rparttree, newdata=testdata, type ="class")
confusionmatrix.tree = table(testdata$Ytest, Yhat.tree)
err.tree = sum(confusionmatrix.tree[1,2], confusionmatrix.tree[2,1])

# BAGGING
Yhat.bag = predict(trainmodel.bag, newdata=testdata, type="class")
confusionmatrix.bag = table(testdata$Ytest, Yhat.bag)
err.bag = sum(Yhat.bag!=Ytest)

# RANDOM FOREST
Yhat.rdm = predict(trainmodel.rdm, newdata=testdata, type="class")
confusionmatrix.rdm = table(testdata$Ytest, Yhat.rdm)
err.rdm = sum(Yhat.rdm!=Ytest)


# ERROR RATES

err.lpm
err.glm
confusionmatrix.tree
err.tree
confusionmatrix.bag
err.bag
confusionmatrix.rdm
err.rdm