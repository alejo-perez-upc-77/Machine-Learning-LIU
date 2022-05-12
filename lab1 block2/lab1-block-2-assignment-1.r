set.seed(12345)
x1<-runif(100)
x2<-runif(100)
traindata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
trainlabels<-as.factor(y)

library(randomForest)

rforest_1 = randomForest(trainlabels~., data = traindata, ntree = 1, nodesize = 25, keep.forest = TRUE)
rforest_10 = randomForest(trainlabels~., data = traindata, ntree = 10, nodesize = 25, keep.forest = TRUE)
rforest_100 = randomForest(trainlabels~., data = traindata, ntree = 100, nodesize = 25, keep.forest = TRUE)

set.seed(12345)

x1<-runif(1000)
x2<-runif(1000)
testdata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
testlabels<-as.factor(y)
plot(x1,x2,col=(y+1))

y_1 = predict(rforest_1,testdata)
y_10 = predict(rforest_10,testdata)
y_100 = predict(rforest_100,testdata)


missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

cat("missclassification for test data of n = 1000 with 1 tree = ", missclass(y_1,testlabels))
cat("missclassification for test data of n = 1000 with 10 trees = ", missclass(y_10,testlabels))
cat("missclassification for test data of n = 1000 with 100 trees = ", missclass(y_100,testlabels))

missclass_large_sample = function(condition,ns = 25){
mce_1 = mce_10 = mce_100 = c()
set.seed(12345)
for(i in 1:1000){
  
  x1<-runif(100)
  x2<-runif(100)
  train<-cbind(x1,x2)
  y<-as.numeric(eval(parse(text = condition)))
  trainlabels<-as.factor(y)
  
  rf_1 = randomForest(trainlabels~., data = train, ntree = 1, nodesize = ns, keep.forest = TRUE)
  rf_10 = randomForest(trainlabels~., data = train, ntree = 10, nodesize = ns, keep.forest = TRUE)
  rf_100 = randomForest(trainlabels~., data = train, ntree = 100, nodesize = ns, keep.forest = TRUE)
  
  x1<-runif(1000)
  x2<-runif(1000)
  test<-cbind(x1,x2)
  y<-as.numeric(eval(parse(text = condition)))
  testlabels<-as.factor(y)
  
  y_1 = predict(rf_1,newdata = test)
  y_10 = predict(rf_10,test)
  y_100 = predict(rf_100,test)
  
  mce_1[i] = missclass(y_1,testlabels)
  mce_10[i] = missclass(y_10,testlabels)
  mce_100[i] = missclass(y_100,testlabels)
  
}
result = list("Mean_MCE_1tree"=mean(mce_1), "Variance_MCE_1tree"=var(mce_1),
              "Mean_MCE_10trees"=mean(mce_10), "Variance_MCE_10trees"=var(mce_10),
              "Mean_MCE_100trees"=mean(mce_100), "Variance_MCE_100trees"=var(mce_100))
return(result)
}

missclass_large_sample(condition = "x1 < x2")

#b
missclass_large_sample(condition = "x1 < 0.5")

#c
missclass_large_sample(condition = "(x1 < 0.5 & x2 < 0.5) | (x1 > 0.5 & x2 > 0.5)", ns = 12)

#b
set.seed(12345)
x1<-runif(100)
x2<-runif(100)
traindata_b<-cbind(x1,x2)
y<-as.numeric(x1<0.5)
trainlabels_b<-as.factor(y)

set.seed(12345)

x1<-runif(1000)
x2<-runif(1000)
testdata_b<-cbind(x1,x2)
y<-as.numeric(x1<0.5)
testlabels_b<-as.factor(y)
plot(x1,x2,col=(y+1))

missclass_large_sample(trainlabels_b,traindata_b,testdata_b,testlabels_b)

#c
set.seed(12345)
x1<-runif(100)
x2<-runif(100)
traindata_c<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
trainlabels_c<-as.factor(y)

set.seed(12345)

x1<-runif(1000)
x2<-runif(1000)
testdata_c<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
testlabels_c<-as.factor(y)
plot(x1,x2,col=(y+1))

missclass_large_sample(trainlabels_c,traindata_c,testdata_c,testlabels_c,12)









