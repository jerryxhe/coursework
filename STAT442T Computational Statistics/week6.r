data <- read.table("mars.train.txt", header=T)
test <- read.table("mars.test.txt", header=T)
for (i in 1:ncol(data)) if(!is.numeric(data[, i])) data[,i] = (as.numeric(data[,i])-1)/max(as.numeric(data[,i])-1)
for (i in 1:ncol(test)) if(!is.numeric(test[, i])) test[,i] = (as.numeric(test[,i])-1)/max(as.numeric(test[,i])-1)
m <- matrix
lsr <- function(x, actual) { sqrt(sum((x-actual)^2)) }
abr <- function(x, actual) {  sum(abs((x-actual))) }

library(randomForest)

#GLM
model.glm <- lm(y~., data=data)
pred.glm <- predict(model.glm, newdata=test)
lsr.glm <- lsr(pred.glm, test$y)
abr.glm <- abr(pred.glm, test$y)

#GLM with clustering
# can try to use interaction terms, but then is just simulation a tree.
cl <- kmeans(data[,-1], 7)
tcl <- kmeans(test[,-1], cl$center)
model.cglm <- lapply(c(1:7), function(i) lm(y~., data=data[cl$cluster==i,]))
pred.cglm <- lapply(c(1:7), function(i) predict(model.cglm[[i]], newdata=test[tcl$cluster==i,]))
lsr.cglm <- sqrt(sum(apply(m(1:7), 1, function(i) lsr(pred.cglm[[i]], test$y[tcl$cluster==i]))*2)) 
abr.cglm <- sum(apply(m(1:7), 1, function(i) abr(pred.cglm[[i]], test$y[tcl$cluster==i]))) 
hist(unlist(pred.cglm))

# Trees
model.tr <- rpart(y~., data=data)
pred.tr <- predict(model.tr, newdata=test)
lsr.tr <- lsr(pred.tr, test$y)
abr.tr <- abr(pred.tr, test$y)

# Tree with clustering
model.ctr <- lapply(c(1:7), function(i) rpart(y~., data=data[cl$cluster==i,]))
pred.ctr <- lapply(c(1:7), function(i) predict(model.ctr[[i]], newdata=test[tcl$cluster==i,]))
lsr.ctr <- sqrt(sum(apply(m(1:7), 1, function(i) lsr(pred.ctr[[i]], test$y[tcl$cluster==i]))*2)) 
abr.ctr <- sum(apply(m(1:7), 1, function(i) abr(pred.ctr[[i]], test$y[tcl$cluster==i]))) 

round(100*xtabs(~pred.nb+actual)/length(pred.nb))

# random Forest
# limiting training size
model.rf <- randomForest(y~., data=data[1:1000,], importance=TRUE, type="regression", na.action=na.omit) 
pred.rf <- predict(model.rf, newdata=test)
lsr.rf <- lsr(pred.rf, test$y)
abr.rf <- abr(pred.rf, test$y)

# clustering
model.crf <- lapply(c(1:7), function(i) randomForest(y~., data=data[cl$cluster==i,]))
pred.crf <- lapply(c(1:7), function(i) predict(model.crf[[i]], newdata=test[tcl$cluster==i,]))
lsr.crf <- sqrt(sum(apply(m(1:7), 1, function(i) lsr(pred.crf[[i]], test$y[tcl$cluster==i]))*2)) 
abr.crf <- sum(apply(m(1:7), 1, function(i) abr(pred.crf[[i]], test$y[tcl$cluster==i]))) 
hist(unlist(pred.crf))

# limiting predictors 
# using tree branch importance
# very slow when given the entire dataset
model.tr.rf <- randomForest(y~., data=data[1:5000,c(1, 4, 11, 28)], importance=TRUE, type="regression", na.action=na.omit) 
pred.tr.rf <- predict(model.tr.rf, newdata=test)
lsr.tr.rf <- lsr(pred.tr.rf, test$y)
abr.tr.rf <- abr(pred.tr.rf, test$y)

# Neural net
model.nn <- nnet(y~., data=data, linout=T, size=2)
pred.nn <- predict(model.nn, newdata=test)
lsr.nn <- lsr(pred.nn, test$y)
abr.nn <- abr(pred.nn, test$y)

# Neural net
# principal component analysis
pr <- prcomp(data[,-1], scale=TRUE)
trx <- predict(pr)
model.pnn <- nnet(trx[, c(1, 2)], data$y, linout=T,size=2, type="regression", na.action=na.omit)
tex <- predict(pr, newdata=test[,-1])
pred.pnn <- predict(model.pnn, newdata=tex)
lsr.pnn <- lsr(pred.pnn, test$y)
abr.pnn <- abr(pred.pnn, test$y)

# clustering
model.cnn <- lapply(c(1:7), function(i) nnet(y~., data=data[cl$cluster==i,], size=2, linout=T, na.action=na.omit))
pred.cnn <- lapply(c(1:7), function(i) predict(model.cnn[[i]], newdata=test[tcl$cluster==i,]))
lsr.cnn <- sqrt(sum(apply(m(1:7), 1, function(i) lsr(pred.cnn[[i]], test$y[tcl$cluster==i]))*2)) 
abr.cnn <- sum(apply(m(1:7), 1, function(i) abr(pred.cnn[[i]], test$y[tcl$cluster==i])))
hist(unlist(pred.cnn))


# graphics
index = order(test$y)
matplot(data.frame(test$y[index], pred.glm[index]))
pred.cglmv = numeric(length(test))
for (i in 1:7) pred.cglmv[tcl$cluster==i] <- pred.cglm[[i]]
matplot(data.frame(test$y[index], pred.cglmv[index]))
matplot(data.frame(test$y[index], pred.cglm[index]))
matplot(data.frame(test$y[index], pred.rf[index]))
pred.crfv = numeric(length(test))
for (i in 1:7) pred.crfv[tcl$cluster==i] <- pred.crf[[i]]
matplot(data.frame(test$y[index], pred.crfv[index]))
matplot(data.frame(test$y[index], pred.tr[index]))
pred.ctrv = numeric(length(test))
for (i in 1:7) pred.ctrv[tcl$cluster==i] <- pred.ctr[[i]]

# saving clusters
clbest <- cl
tclbest <- tcl
bestcglm <- model.cglm

pca = data.frame(trx)
pca$y = data$y
pcat = data.frame(tex)
pcat$y = test$y

# another tree implementation
model.tr2 <- tree(y~., data=data)
pred.tr2 <- predict(model.tr2, newdata=test)
lsr.tr2 <- lsr(pred.tr2, test$y)
abr.tr2 <- abr(pred.tr2, test$y)

# another neural net implementation
#net <- newff(n.neurons=c(1,3,2,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm")
#result <- train(net, test[,-1], test$y, error.criterium="LMS", report=TRUE, show.step=100, n.shows=5)

