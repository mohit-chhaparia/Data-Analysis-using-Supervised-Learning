#CODE SUBMITTED IN THE FIRST REPORT:
#Loading_Data
library(readxl)
library(caTools)
library(ISLR)
library(tree)
datast <- data.frame(read_excel("/Users/mohitdeepakchhaparia/Downloads/train.xlsx"))
testtest <- data.frame(read_excel("/Users/mohitdeepakchhaparia/Downloads/test.xlsx"))
testpredict <- testtest[, c("X1", "X2", "X5", "X7", "X3", "X8")]
attach(datast)
cor(datast) #To find correlation among variables (All the analysis is done as per correlation, that is, without the predictors X4, and X6)

#Split 75% As Training Data:
set.seed(1)
sample <- sample.split(Y1, SplitRatio = 0.75) #Splitting 75% of dataset as training data and the remaining as test data
train <- data.frame(subset(datast, sample == TRUE))
train <- train[, c("X1", "X2", "X3", "X5", "X7", "X8", "Y1")] #training dataset
test <- data.frame(subset(datast, sample == FALSE))
test.predictors <- data.frame(test[,c("X1", "X2", "X3", "X5", "X7", "X8")]) #predictors of test dataset
test.response <- test[,c("Y1")] #response of test dataset

#Boosting
library(gbm)
set.seed(1)
boost.boosting <- gbm(Y1~., data = train, distribution = "gaussian", n.trees = 20000, interaction.depth = 2, cv.folds = 10, shrinkage = 0.005)
min(boost.boosting$cv.error) #Minimum CV error of the boosting model on training dataset (without X4, and X6)
summary(boost.boosting) #Relative Influence Plot (Here we find that X1, X2, and X5 have the highest relative influence on Y1)
par(mfrow=c(1,3))
plot(boost.boosting, i = "X1") #Partial Dependence Plot (Dependence of Y1 on X1)
plot(boost.boosting, i = "X2") #Partial Dependence Plot (Dependence of Y1 on X2)
plot(boost.boosting, i = "X5") #Partial Dependence Plot (Dependence of Y1 on X5)
yhat.boost <- predict(boost.boosting, newdata = test.predictors, n.trees = 20000) #Predicting y-hat using the found boosting model and test dataset
mean((yhat.boost - test.response)^2) #Mean Square Error on test dataset
gbm.perf(boost.boosting, method = "cv")

#Final_Model_To_Be_Used_On_Test_Data
datastt <- datast[, c("X1", "X2", "X5", "X7", "X3", "X8", "Y1")]
set.seed(1)
tree.making <- tree(formula = Y1~., data = datast)
set.seed(1)
boost.boosting <- gbm(Y1~., data = datastt, distribution = "gaussian", n.trees = 20000, interaction.depth = 2, cv.folds = 10, shrinkage = 0.005)
min(boost.boosting$cv.error) #Minimum CV error of the boosting model on entire dataset (without X4, and X6)
summary(boost.boosting) #Relative Influence Plot (Here we find that X1, X2, and X5 have the highest relative influence on Y1)
par(mfrow=c(1,3))
plot(boost.boosting, i = "X1") #Partial Dependence Plot (Dependence of Y1 on X1)
plot(boost.boosting, i = "X2") #Partial Dependence Plot (Dependence of Y1 on X2)
plot(boost.boosting, i = "X5") #Partial Dependence Plot (Dependence of Y1 on X5)
gbm.perf(boost.boosting, method = "cv")



#RUNNING TEST DATA ON CODE SUBMITTED IN THE FIRST REPORT:
yhat.boost <- predict(boost.boosting, newdata = testpredict, n.trees = 20000)
mean((yhat.boost - testtest$Y1)^2) #Mean Test Error as per the test data using the model submitted in the first report
oldError <- mean((yhat.boost - testtest$Y1)^2)



#Loading_Data
library(readxl)
library(caTools)
library(ISLR)
library(tree)
datast <- data.frame(read_excel("/Users/mohitdeepakchhaparia/Downloads/train.xlsx"))
testtest <- data.frame(read_excel("/Users/mohitdeepakchhaparia/Downloads/test.xlsx"))
testpredict <- testtest[, c("X1", "X2", "X5", "X6", "X7", "X3", "X8")] #Dataset - Using 7 test predictors out of 8 from Test Dataset
attach(datast)
datastt <- datast[, c("X1", "X2", "X5", "X6", "X7", "X3", "X8", "Y1")]

hyper_grid <- expand.grid(
  a <- c(5000, 7500, 10000, 20000),
  b <- c(4, 5),
  e <- c(0, 5, 10),
  d <- c(0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.007, 0.08397, 0.08, 0.09, 0.009, 0.1),
  err <- 0
)
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  set.seed(1)
  boost.boosting <- gbm(Y1~., data = datastt, distribution = "gaussian", n.trees = hyper_grid$Var1[i], interaction.depth = hyper_grid$Var2[i], cv.folds = hyper_grid$Var3[i], shrinkage = hyper_grid$Var4[i])
  yhat.boost <- predict(boost.boosting, newdata = testpredict, n.trees = hyper_grid$Var1[i])
  hyper_grid$Var5[i] <- mean((yhat.boost - testtest$Y1)^2)
  print(i) #Number of Iterations completed
}

#PARAMETERS FOR MINIMUM ERROR
min(hyper_grid$Var5)
which.min(hyper_grid$Var5)
m <- which.min(hyper_grid$Var5)
hyper_grid[which.min(hyper_grid$Var5),]

#PARAMETERS FOR MAXIMUM ERROR
max(hyper_grid$Var5)
which.max(hyper_grid$Var5)
hyper_grid[which.max(hyper_grid$Var5),]

#ERROR VS N.TREES
#How number of trees affects test error
plot(hyper_grid$Var1,hyper_grid$Var5, xlab = "n.trees", ylab = "Error")

#ERROR VS INTERACTION.DEPTH
#How interaction depth affects test error
plot(hyper_grid$Var2,hyper_grid$Var5, xlab = "interaction.depth", ylab = "Error")

#ERROR VS CV.FOLDS
#How number of folds affects test error
plot(hyper_grid$Var3,hyper_grid$Var5, xlab = "cv.folds", ylab = "Error")

#ERROR VS SHRINKAGE
#How shrinkage or learning parameter affects test error
plot(hyper_grid$Var4,hyper_grid$Var5, xlab = "shrinkage", ylab = "Error")

#FITING THE MODEL FOR NEW PARAMETERS (TRAINING DATA)
set.seed(1)
boost.boosting <- gbm(Y1~., data = datastt, distribution = "gaussian", n.trees = hyper_grid$Var1[m], interaction.depth = hyper_grid$Var2[m], cv.folds = hyper_grid$Var3[m], shrinkage = hyper_grid$Var4[m])

#PREDICTING USING THE FITTED MODEL FOR NEW PARAMETERS (TEST DATA)
yhat.boost <- predict(boost.boosting, newdata = testpredict, n.trees = hyper_grid$Var1[m])

#MSE (TEST DATA) USING NEW PARAMETERS
mean((yhat.boost - testtest$Y1)^2) #Mean Test Error as per the test data using the model submitted in this report (Or the final improved model)
newError <- mean((yhat.boost - testtest$Y1)^2)

#PERCENT DECREASE IN ERROR AFTER CHANGING THE PARAMETERS
(oldError - newError) * 100 / oldError