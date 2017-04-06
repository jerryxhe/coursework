data <- read.csv("breast.train.TXT", header=T)
test <- read.csv("breast.test.TXT", header=T)
for (i in 2:ncol(data)) if(!is.numeric(data[, i])) data[,i] = as.numeric(data[,i]) -1
for (i in 2:ncol(test)) if(!is.numeric(test[, i])) test[,i] = as.numeric(test[,i]) -1
dxtest <- test[,-1]
actual <- dxtest$histologie

print(aggregate(data, list(type=data$histologie), mean))

library(e1071)
library(boost)
library(randomForest)
library(ada)


# boosting
# logitboost

xl <- as.matrix(data[,-c(1:2)])
ylearn <- data[,2] # already numeric

xt <- as.matrix(test[,-c(1:2)])

xlearn <- apply(xl, 2, as.numeric)
xtest <- apply(xt, 2, as.numeric)

prob.bt <- logitboost(xlearn, ylearn, xtest, presel=0)[,100]
pred.bt <- round(prob.bt)

round(100*xtabs(~pred.bt+actual)/length(pred.bt))

# adaboost
model.ada <- ada(histologie ~.-ID, data = data, control = rpart.control(maxdepth = 30, cp = 0.01, minsplit = 20, xval = 10), iter = 50, loss="logistic", type="gentle")    
pred.ada <- predict(model.ada, newdata=test)
round(100*xtabs(~pred.ada+actual)/length(pred.ada))

# naiveBayes

model.nb <- naiveBayes(histologie~.-ID, data=data)
prob.nb <- predict(model.nb, newdata=test[,-2], type="raw")
pred.nb <- round(prob.nb[,2])

round(100*xtabs(~pred.nb+actual)/length(pred.nb))

# random Forest

model.rf <- randomForest(as.factor(histologie) ~.-ID-taille-nombre, data=data,importance=TRUE, na.action=na.omit) 
pred.rf <- predict(model.rf, newdata=test)
round(100*xtabs(~pred.rf+actual)/length(pred.rf))



