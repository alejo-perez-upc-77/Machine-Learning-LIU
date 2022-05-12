library(ggplot2)
library(kknn)
library(reshape2)
#### 1 ####

digits <- read.csv(file = 'C:/Users/alejo/Desktop/Machine Learning/optdigits.csv')
digits[,ncol(digits)] <- data.frame(sapply(digits[,ncol(digits)], as.character),stringsAsFactors = TRUE)
names(digits) <- c(seq.int(ncol(digits)-1),"target")
#digits[1,]

### Plotting one observation ###

plot_observation <- function(id, M){
  number <- matrix(M[id, 1:(ncol(M)-1)], 8,8)
  mode(number) = "numeric"
  image(number[,nrow(number):1], col=grey(seq(0, 1, length = 256)))
  return(M[id, ncol(M)])
}

plot_observation(5, digits)

##### 2 ######

### Train/Valation/Test Division ###

set.seed(12345)

n=dim(digits)[1]
set.seed(12345) 

id=sample(1:n, floor(n*0.5)) 
dfTraining=digits[id,]

id1=setdiff(1:n, id)
set.seed(12345) 

id2=sample(id1, floor(n*0.25)) 
dfValidation=digits[id2,]

id3=setdiff(id1,id2)
dfTest=digits[id3,] 

### Let's fit the Model and predict results for Testing Set ###

kknn_model <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfTest, k = 30, kernel = "rectangular")

fit <- fitted(kknn_model)
CM_test <- table(dfTest$target, fit)
CM_test
accuracy_test <- (sum(diag(CM_test)))/sum(CM_test)
cat("Misclassification error in testing set is", 1 - accuracy_test)

### Let's fit the Model and predicting results for Testing Set ###

kknn_model <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfTraining, k = 30, kernel = "rectangular")
#summary(kknn_model)

fit <- fitted(kknn_model)
CM_train <- table(dfTraining$target, fit)
CM_train
accuracy_train <- (sum(diag(CM_train)))/sum(CM_train)
cat("Misclassification in training set is", 1 - accuracy_train)

##### 3 ######

# Getting the probabilities of prediction probabilities, joining with the actual category and the prediction made

prob_df <- kknn_model$prob
max_prob <- colnames(prob_df)[apply(prob_df,1,which.max)]
train_probs <- data.frame(prob_df)
train_probs$actual <- dfTraining$target
train_probs$pred <- kknn_model$fitted.values
train_probs$max_prob <- max_prob

# Subsetting of the actual 8s and ordering by probabilities  

actual_8s <- train_probs[train_probs["actual"]==8,]
predicted_actual_8s <- actual_8s[actual_8s["pred"]==8,]

# 8 predicted correctly with high certainty
best_8_indexes <- as.numeric(row.names(predicted_actual_8s[order(-predicted_actual_8s[,9]),][1:2,]))

# 8 predicted wrongly with high certainty
worst_8_indexes <- as.numeric(row.names(predicted_actual_8s[order(predicted_actual_8s[,9]),][1:3,]))

  
dfTraining[best_8_indexes,]
dfTraining[worst_8_indexes,]

# plot of 8 predicted correctly with high certainty

plot_observation(best_8_indexes[1], dfTraining)
plot_observation(best_8_indexes[2], dfTraining)

# plot of 8 predicted wrongly with high certainty

plot_observation(worst_8_indexes[1], dfTraining)
plot_observation(worst_8_indexes[2], dfTraining)
plot_observation(worst_8_indexes[3], dfTraining)

# plot of 8 predicted wrongly with high certainty

KKNN_misclassification_training <- c()
KKNN_misclassification_validation <- c()

##### 4 ######

#Let's fit a model for each k in the range

for (k in 1:30){
  
  # Fit for dfValidation and dfTraining
  
  KKNN_model_valid <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfValidation, k = k, kernel = "rectangular")
  KKNN_model_train <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfTraining, k = k, kernel = "rectangular")
  
  # Calculation of Accuracies
  
  fit_train <- fitted(KKNN_model_train)
  CM_train <- table(dfTraining$target, fit_train)
  KKNN_misclassification_training[k] <- 1 - (sum(diag(CM_train)))/sum(CM_train)
  
  fit_valid <- fitted(KKNN_model_valid)
  CM_valid <- table(dfValidation$target, fit_valid)
  KKNN_misclassification_validation[k] <- 1 -(sum(diag(CM_valid)))/sum(CM_valid)

    
}

# Plot of Accuracies

metrics <- data.frame(KKNN_misclassification_validation, KKNN_misclassification_training, 1:30)
names(metrics) <- c("validation", "training", "k")


metricsMelted <- melt(metrics, id.var='k')
names(metricsMelted)[3] <- "Misclassification_Error"
head(metricsMelted)
ggplot(metricsMelted, aes(x=k, y=Misclassification_Error, col=variable)) + geom_line()

best_K <- (which(KKNN_misclassification_validation==min(KKNN_misclassification_validation)))
cat("The best performant K parameter are k =", as.character(best_K))

# Testing of optimal K with Testing set

kknn_model <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfTest, k = max(best_K), kernel = "rectangular")

fit <- fitted(kknn_model)
CM_test <- table(dfTest$target, fit)
CM_test
accuracy_test <- 1- (sum(diag(CM_test)))/sum(CM_test)
cat("Misclassification error is", accuracy_test)


### 4 ###

hot_encode <- function(i){
  v <- rep(0,10)
  v[i+1] <- 1
  return(I(v))
}

empirical_risk <- c()

# Let's fit a model for each k in the range

for (k in 1:30){
  
  KKNN_model_valid <- kknn(dfTraining[["target"]] ~ ., dfTraining, dfValidation, k = k, kernel = "rectangular")
  
  # Extraction of probabilities, we build a df with the target hot encoded to add a column of cross-entropy loss
  
  prob_df <- KKNN_model_valid$prob
  max_prob <- colnames(prob_df)[apply(prob_df,1,which.max)]
  valid_probs <- data.frame(prob_df)
  valid_probs$actual <- dfValidation$target
  valid_probs$pred <- KKNN_model_valid$fitted.values
  valid_probs$max_prob <- max_prob

  valid_probs$coded <- (lapply(as.numeric(valid_probs$actual )-1,  hot_encode))
  
  # column of cross-entropy loss
  for (row in 1:nrow(valid_probs)){
    valid_probs[row, "cross_entropy"] <- -sum(log(valid_probs[row, 1:10]+1e-15, base = 10)*valid_probs[[row,"coded"]] )
  }
  
  empirical_risk[k] <- mean(valid_probs$cross_entropy)
  
}
#### Differences of errors

# Plot of Empirical Risk for each K

metrics <- data.frame(empirical_risk, 1:30)
names(metrics) <- c("cross_entropy", "k")

metricsMelted <- melt(metrics, id.var='k')

names(metricsMelted)[3] <- "cross_entropy"
head(metricsMelted)
ggplot(metricsMelted, aes(x=k, y=cross_entropy, col=variable)) + geom_line()

best_K <- which(empirical_risk==min(empirical_risk))
cat("The best performant K parameter is k =", as.character(best_K))

