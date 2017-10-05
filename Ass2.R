####Install packages if needed####
install.packages("tree")
install.packages("e1071")
install.packages(("ROCR"))
install.packages("randomForest")
install.packages("adabag")

####Preliminary Setup####
PBRead <- read.csv("D:/UNi/Semester 2 2017/3152/Assignment2/PBData.csv")
set.seed(26935821) # your student number
PBD <- PBRead[sample(nrow(PBRead), 1000), ] # sample 1000 rows
rm(PBRead)
PBD=PBD[complete.cases(PBD), ]
PBD$subscribed = factor(PBD$subscribed)
PBD.size = nrow(PBD)

####Question1####
#Explore data
PBD.subscribed = PBD[PBD$subscribed=='yes',]
PBD.not.subscribed = PBD[PBD$subscribed=='no',]
PBD.subscribed.size = nrow(PBD.subscribed)
PBD.not.subscribed.size = nrow(PBD.not.subscribed)
PBD.subscribed.portion = (PBD.subscribed.size/PBD.size)
PBD.not.subscribed.portion = 1-PBD.subscribed.portion
summary(PBD)
summary(PBD.subscribed)
summary(PBD.not.subscribed)

####Question2####
#Set training and testing data
set.seed(26935821) #random seed
train.row = sample(1:nrow(PBD), 0.7*nrow(PBD))
PBD.train = PBD[train.row,]
PBD.test = PBD[-train.row,]
rm(train.row)

####Question3####
#Classification models
#Decision Tree model
library(tree)
PBD.tree = tree(subscribed~.,data = PBD.train)
PBD.tree.predict=predict(PBD.tree,PBD.test, type = "class")

#Naive Bayes model
library(e1071)
PBD.Bayes = naiveBayes(subscribed~.,data = PBD.train)
PBD.Bayes.predict=predict(PBD.Bayes, PBD.test)

#Bagging ensemble
library(adabag)
PBD.bag = bagging(subscribed~.,data=PBD.train)
PBD.bag.predict = predict.bagging(PBD.bag, newdata=PBD.test)

#Boosting ensemble
PBD.boost=boosting(subscribed~.,data=PBD.train)
PBD.boost.predict=predict.boosting(PBD.boost,newdata=PBD.test)

#RandomForest ensemble
library(randomForest)
PBD.rf=randomForest(subscribed~.,data=PBD.train)
PBD.rf.predict=predict(PBD.rf,PBD.test)

####Question4####
#Decision tree confusion/ accuracy
table(PBD.test$subscribed, PBD.tree.predict) 
(PBD.tree.accuracy = mean(PBD.tree.predict==PBD.test$subscribed))

#Bayes confusion/ accuracy
table(PBD.test$subscribed, PBD.Bayes.predict)
(PBD.Bayes.accuracy = mean(PBD.Bayes.predict==PBD.test$subscribed))

#BAG confusion/ accuracy
PBD.bag.predict$confusion
(PBD.bag.accuracy=1-(PBD.bag.predict$error))

#Boost confusion/ accuracy
PBD.boost.predict$confusion
(PBD.boost.accuracy=1-(PBD.boost.predict$error))

#RandomForest confusion/accuracy
table(observed=	PBD.test$subscribed,predicted	=	PBD.rf.predict)
(PBD.rf.accuracy = mean(PBD.rf.predict==PBD.test$subscribed))

####Question5####
library("ROCR")
#ROC (receiver operating characteristic) curves and AUC (area under ROC curve)
#Decision tree ROC curve/ AUC
PBD.ROC.tree.predict = predict(PBD.tree, PBD.test, type="vector")
PBD.ROC.tree.prediction=prediction(PBD.ROC.tree.predict[,2],PBD.test$subscribed)
PBD.ROC.tree.perf=performance(PBD.ROC.tree.prediction,"tpr","fpr")
plot(PBD.ROC.tree.perf, main = "ROC curves")
PBD.ROC.tree.AUC = performance(PBD.ROC.tree.prediction,"auc")
print(as.numeric(PBD.ROC.tree.AUC@y.values))

#Bayes ROC curve/ AUC
PBD.ROC.Bayes.predict = predict(PBD.Bayes, PBD.test, type="raw")
PBD.ROC.Bayes.prediction = prediction(PBD.ROC.Bayes.predict[,2],PBD.test$subscribed)
PBD.ROC.Bayes.perf=performance(PBD.ROC.Bayes.prediction,"tpr","fpr")
plot(PBD.ROC.Bayes.perf, add=TRUE, col="blue")
PBD.ROC.Bayes.AUC = performance(PBD.ROC.Bayes.prediction,"auc")
print(as.numeric(PBD.ROC.Bayes.AUC@y.values))


#Random Forest ROC curve/ AUC
PBD.ROC.rf.predict = predict(PBD.rf, PBD.test, type="prob")
PBD.ROC.rf.prediction = prediction(PBD.ROC.rf.predict[,2],PBD.test$subscribed)
PBD.ROC.rf.perf=performance(PBD.ROC.rf.prediction,"tpr","fpr")
plot(PBD.ROC.rf.perf, add=TRUE, col="red")
PBD.ROC.rf.AUC = performance(PBD.ROC.rf.prediction,"auc")
print(as.numeric(PBD.ROC.rf.AUC@y.values))


#Bagging ROC curve/ AUC
PBD.ROC.bag.predict = predict.bagging(PBD.bag, newdata=PBD.test, type="vector")
PBD.ROC.bag.prediction = prediction(PBD.ROC.bag.predict$prob[,2],PBD.test$subscribed)
PBD.ROC.bag.perf = performance(PBD.ROC.bag.prediction, "tpr","fpr")
plot(PBD.ROC.bag.perf, add=TRUE, col = "orange")
PBD.ROC.bag.AUC = performance(PBD.ROC.bag.prediction,"auc")
print(as.numeric(PBD.ROC.bag.AUC@y.values))

#Bagging ROC curve/ AUC
PBD.ROC.boost.predict = predict.boosting(PBD.boost, newdata=PBD.test, type="vector")
PBD.ROC.boost.prediction = prediction(PBD.ROC.boost.predict$prob[,2],PBD.test$subscribed)
PBD.ROC.boost.perf = performance(PBD.ROC.boost.prediction, "tpr","fpr")
plot(PBD.ROC.boost.perf, add=TRUE, col = "green")
PBD.ROC.boost.AUC = performance(PBD.ROC.boost.prediction,"auc")
print(as.numeric(PBD.ROC.boost.AUC@y.values))


####Question 7####
#Most important classifier

#RandomFOrest best classifier
varImpPlot(PBD.rf)
#shows duration to be the best classifier




####Question8####
#RandomForest attempt to improve by increasing tree size and changing mtry value
library(randomForest)
PBD.rf.improved=randomForest(subscribed~.,data=PBD.train, ntree=10000, mtry=4)
PBD.rf.improved.predict=predict(PBD.rf.improved,PBD.test)
table(observed=	PBD.test$subscribed,predicted	=	PBD.rf.improved.predict)
(accuracy = mean(PBD.rf.improved.predict==PBD.test$subscribed))

#Decision tree attempt to improve through pruning
PBD.tree.prune = prune.misclass(PBD.tree, best=2)
PBD.tree.prune.predict = predict(PBD.tree.prune, PBD.test, type="class")
table(predicted = PBD.tree.prune.predict, actual = PBD.test$subscribed)
(accuracy = mean(PBD.tree.prune.predict==PBD.test$subscribed))

#Boosting attempt to improve by changing newmfinal value
PBD.boost.improved = boosting(subscribed~.,data=PBD.train,mfinal=3)
PBD.boost.improved.predict=predict.boosting(PBD.boost.improved,newdata=PBD.test)
(accuracy = 1-PBD.boost.improved.predict$error)





