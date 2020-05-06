library(class) 
library(ISLR) 
library(pROC) 
library(MASS)
library(randomForest)

# benchmarking on unscaled data

set.seed(4061)
n = nrow(Default)
dat = Default[sample(1:n, n, replace=FALSE), ]

i.cv = sample(1:n, round(.7*n), replace=FALSE)
dat.cv = dat[i.cv,] 
dat.valid = dat[-i.cv,] 


K.knn = 3 

K = 10
N = length(i.cv)
folds = cut(1:N, K, labels=FALSE)
acc.knn = acc.glm = acc.lda = acc.qda = numeric(K)
auc.knn = auc.glm = auc.lda = auc.qda = numeric(K)
acc.rf = auc.rf = numeric(K) 
#
for(k in 1:K){ 
  
  i.train	= which(folds!=k)
  dat.train = dat.cv[i.train, ]
  dat.test = dat.cv[-i.train, ]
  
  x.train = dat.train[,-1]
  y.train = dat.train[,1]
  x.test = dat.test[,-1]
  y.test = dat.test[,1]
  x.train[,1] = as.numeric(x.train[,1])
  x.test[,1] = as.numeric(x.test[,1])
  
  knn.o = knn(x.train, x.test, y.train, K.knn)
  glm.o = glm(default~., data=dat.train, family=binomial(logit))
  lda.o = lda(default~., data=dat.train)
  qda.o = qda(default~., data=dat.train)
  rf.o = randomForest(default~., data=dat.train)
  
  knn.p = knn.o
  glm.p = ( predict(glm.o, newdata=dat.test, type="response") > 0.5 )
  lda.p = predict(lda.o, newdata=dat.test)$class
  qda.p = predict(qda.o, newdata=dat.test)$class	
  rf.p = predict(rf.o, newdata=dat.test)
  tb.knn = table(knn.p, y.test)
  tb.glm = table(glm.p, y.test)
  tb.lda = table(lda.p, y.test)
  tb.qda = table(qda.p, y.test)
  tb.rf = table(rf.p, y.test)
  
  acc.knn[k] = sum(diag(tb.knn)) / sum(tb.knn)
  acc.glm[k] = sum(diag(tb.glm)) / sum(tb.glm)
  acc.lda[k] = sum(diag(tb.lda)) / sum(tb.lda)
  acc.qda[k] = sum(diag(tb.qda)) / sum(tb.qda)
  acc.rf[k] = sum(diag(tb.rf)) / sum(tb.rf)
  
  glm.p = predict(glm.o, newdata=dat.test, type="response")
  lda.p = predict(lda.o, newdata=dat.test)$posterior[,2]
  qda.p = predict(qda.o, newdata=dat.test)$posterior[,2]
  
  auc.glm[k] = roc(y.test, glm.p)$auc
  auc.lda[k] = roc(y.test, lda.p)$auc
  auc.qda[k] = roc(y.test, qda.p)$auc
}
boxplot(acc.knn, acc.glm, acc.lda, acc.qda,
        main="Overall CV prediction accuracy",
        names=c("kNN","GLM","LDA","QDA"))
boxplot(auc.glm, auc.lda, auc.qda,
        main="Overall CV AUC",
        names=c("GLM","LDA","QDA"))
boxplot(auc.knn, auc.glm, auc.lda, auc.qda,
        main="Overall CV AUC",
        names=c("kNN","GLM","LDA","QDA"))
