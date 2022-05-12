# data0=read_csv("geneexp.csv")
# data=data0[, -1]
# data$CellType=as.factor(data$CellType)
# set.seed(12345)
# n <- dim(data)[1]
# id=sample(1:n, floor(n*0.7))
# train=data[id,]
# test=data[-id,]
# 
# library(pamr)
# rownames(train)=1:nrow(train)
# genenames <- colnames(train[ ,-ncol(train)])
# x <- t(train[ ,-ncol(train)])
# y=train[[ncol(train)]]
# mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=genenames)
# model=pamr.train(mydata)
# pamr.plotcen(model, mydata, threshold = 6.7)
# pamr.plotcen(model, mydata, threshold=6.7)
# a=pamr.listgenes(model,mydata,threshold=6.7)
# cat( paste( colnames(data)[as.numeric(a[,1])], collapse='\n' ) )
# cvmodel=pamr.cv(model,mydata)
# print(cvmodel)
# pamr.plotcv(cvmodel)
# 
# 
# 
# library(glmnet)
# x <- as.matrix(train[ ,-ncol(train)])
# cv.lelastic <- cv.glmnet(x, train$CellType, alpha = 0.5, family = "multinomial")
# elastic <- glmnet(x, train$CellType, alpha = 0.5, family = "multinomial",
#                       lambda = cv.lelastic$lambda.min)
# 
# res <- predict(elastic, newx = as.matrix(test[ ,-ncol(test)]), s = cv.lelastic$lambda.min, type = "class")



library(readr)
data0=read_csv("geneexp.csv")
data=data0[, -1]
data$CellType=as.factor(data$CellType)
set.seed(12345)
n <- dim(data)[1]
id=sample(1:n, floor(n*0.7))
train=data[id,]
test=data[-id,]

test <- function(name){
  y <- ifelse(data$CellType == name, 1, 0 )
  df <- data.frame(character(), numeric())
  for (i in 1:(dim(data)[2]-1)) {
    test <- t.test(unlist(data[ ,i])~y, data = data, alternative = "two.sided")
    df <- rbind(df, c(colnames(data)[i], test$p.value))
  }
  colnames(df) <- c("name", "pvalue")
  return(df)
}


results <- test("CD4")

plotres <- function(name){
  
  results <- test(name)
      
  results <- results[order(as.numeric(results$pvalue)), ]
  
  M <- length(results$pvalue)
  for (i in 1:M) {
    if(as.numeric(results$pvalue[i]) > (0.05 * i / M)){
      break
    }
    
  }
  L <- i-1
  p <- as.numeric(results$pvalue[L])
  rejected <- ifelse(as.numeric(results$pvalue) <= p, 1, 0 )
  
  row.names(results) <- c(1:length(results$pvalue))
  library(ggplot2)
  ggplot(results, aes(x=c(1:2085), y=pvalue)) +
    geom_point(shape=1)      # Use hollow circles
  
  title <- paste0("t.test for ", name, " features using BH method")
  plot(c(1:L),
       results$pvalue[1:L],
       pch = 20,
       col = "red",
       xlim = c(0,M),
       ylim = c(0,1),
       cex = 0.5,
       xlab = "features ordered by p-value",
       ylab = "p-value",
       main = title)
  abline(v=L, col="blue")
  points(c(L+1:M), results$pvalue[L+1:M], pch = 20, col = "blue", xlim = c(0,M), ylim = c(0,1), cex = 0.5)
}

scatterplot(results$pvalue ~ c(1:M) | cyl, data = mtcars, 
            smoother = FALSE, grid = FALSE, frame = FALSE)