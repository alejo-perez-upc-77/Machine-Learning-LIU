##Assignment 3

library(glmnet)
data = read.csv("C:/Users/vcshw/Machine Learning and Stats/Sem1/Machine learning/lab/tecator.csv", header = TRUE)
n = nrow(data)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#1
X_train = as.matrix(train[2:101])
Y_train = as.matrix(train$Fat)
fit_train = lm(Fat ~ . , data = train[2:102]) #fitting linear regression to training data
y_hat_train = predict(fit_train)
train_mse = mean((y_hat_train-Y_train)^2) 
cat("MSE for training data is ",train_mse)
cat("MAE for training data is ",mean(abs(y_hat_train - Y_train)))
#scatter.smooth(Y_train,y_hat_train)
new_x=as.matrix((test[, 2:101]))
Y_test = as.matrix((test[,102]))
y_hat_test = predict(fit_train, newdata = as.data.frame(new_x))
#scatter.smooth(Y_test,y_hat_test)
test_mse = mean((y_hat_test-Y_test)^2) 
cat("\nMSE for testing data ",test_mse)
cat("MAE for training data is ",mean(abs(y_hat_test - Y_test)))

#3
covariates = scale(train[ ,2:101])
response = scale(train[ ,102])
model_lasso = glmnet(as.matrix(covariates), response, alpha = 1, family = "gaussian")
plot(model_lasso, xvar = "lambda", label = T)
model=cv.glmnet(as.matrix(covariates),
                response, alpha=1,family="gaussian")
model_lasso_3 = glmnet(as.matrix(covariates), response, alpha = 1, family = "gaussian", lambda = model$lambda[which(model$nzero == 3)])
plot(model_lasso_3, xvar = "lambda", label = T)
cat("Lambda values that give 3 variables are :",model$lambda[which(model$nzero == 3)])

#4
df = function(lambda){
  ld = lambda * diag(ncol(covariates))
  H = covariates %*% solve(t(covariates) %*% covariates + ld) %*% t(covariates)
  DOF = sum(diag(H))
  return(DOF)
}
lambda_values = model_lasso$lambda
degrees_of_freedom = c()
for(i in 1:length(lambda_values)){
  degrees_of_freedom[i] = df(lambda_values[i])
}
plot(lambda_values, degrees_of_freedom)

#5
covariates = scale(train[ ,2:101])
response = scale(train[ ,102])
model_ridge = glmnet(as.matrix(covariates), response, alpha = 0, family = "gaussian")
plot(model_ridge, main = "RIDGE", xvar = "lambda", label = T)

#6
model_l=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian",lambda=seq(0,1,0.001))
best_lambda = model_l$lambda.min
plot(model_l)
plot(model_l$lambda,model_l$cvm,xlab="lambda",ylab="Mean CV error")
cat("Optimal lambda is :",best_lambda)

m = cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian",lambda=c(best_lambda,0.1353353))
cat("CV Error when lambda is ",m$lambda[1] ," =",m$cvm[1])
cat(" and CV Error when lambda is ",m$lambda[2] ," =",m$cvm[2])

model_optimallasso =  glmnet(as.matrix(covariates), response, alpha = 1, family = "gaussian", lambda = best_lambda)
y = as.matrix(scale(test[,102]))
ypred=predict(model_optimallasso, newx=as.matrix(scale(test[, 2:101])), type="response")
scatter.smooth(y,ypred)
cat("coefficient of determination is ",
    sum((ypred-mean(y))^2)/sum((y-mean(y))^2), "and MSE is ", mean((y-ypred)^2))

#7
betas = as.vector((as.matrix(coef(model_optimallasso))[-1, ])) # removing the first row for intercept
resid = response - (covariates %*% betas)
sigma = sd(resid)

ypred=predict(model_optimallasso, newx=as.matrix(scale(test[, 2:101])), type="response")
set.seed(90000)
y_gen = rnorm(108,ypred,sigma)
scatter.smooth(y,y_gen)
cat("coefficient of determination is ",sum((y_gen-mean(y))^2)/sum((y-mean(y))^2)," MSE is ",mean((y_gen - y)^2))

