car<- read.csv("car_data.csv")
#setting seed 

set.seed(71923)

#partitioning data into test and training

train_insts = sample(nrow(car), .7*nrow(car))
car_train <- car[train_insts,]
car_test <- car[-train_insts,]

#boxplots between vehOdo and IsBadBuy

attach(car_train)
boxplot(VehOdo~IsBadBuy, xlab="Bad Buy",ylab="Odometer reading of the vehicle")
boxplot(VehicleAge~IsBadBuy,xlab="Bad Buy",ylab="Age of the Vehicle")

#two way table 
car_table <- table(Make,IsBadBuy)
car_table
car_norm= car_table/(rowSums(car_table))
car_norm

#3 linear regression model
lin_model <- lm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=car_train)
summary(lin_model)

#making predictions
car_lin_train_preds <- predict(lin_model,newdata=car_train)
car_lin_test_preds <- predict(lin_model,newdata=car_test)

##and generate predictions on the validation data
valid_preds_test <- predict(lin_model, newdata = car_test)
valid_preds_train <- predict(lin_model, newdata = car_train)
##these are the actual values for the validation data
valid_actuals_test <- car_test$IsBadBuy
valid_actuals_train <- car_train$IsBadBuy
##calculate Average Error (AE) for predictions vs. actuals on validation data
AE_car_test <- mean(valid_preds_test-valid_actuals_test)
AE_car_train <- mean(valid_preds_train-valid_actuals_train)
##calculate Root Mean Squared Error for predictions vs. actuals on validation data
RMSE_car_test <- sqrt(mean((valid_actuals_test - valid_preds_test)^2))
RMSE_car_train <- sqrt(mean((valid_actuals_train - valid_preds_train)^2))
RMSE_car_test
RMSE_car_train
AE_car_test
AE_car_train
##let's define some reusable functions!

##this one makes a confusion matrix for a given c
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  ##careful with positives and negatives here!
  confusion_matrix <- table(actuals,classifications)
}

car_lin_train_preds <- predict(lin_model,newdata=car_train)
car_lin_test_preds <- predict(lin_model,newdata=car_test)

car_lin_matrix <- confusion_matrix(car_lin_test_preds, car_test$IsBadBuy,.5)
car_lin_matrix

accuracy_baseline <- (sum(ifelse(car_test$IsBadBuy==0,1,0))/nrow(car_test))
nrow(car_test)
sum(car_test$IsBadBuy)
accuracy_baseline

##a classification accuracy measures function that takes a confusion matrix as input
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  ##accuracy = total number of correct classifications/total number of classifications
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  ##TPR = Percent of actual positives identified as such (sensitivity)
  tpr <- TP/(TP+FN)
  
  ##TNR = Percent of actual negatives identified as such (specificity)
  tnr <- TN/(TN+FP)
  
  ##I'll leave it as an exercise for you to compute the other basic confusion matrix metrics
  
  ##return the list of metrics you want
  return(c(acc, tpr, tnr))
}
car_lin_metrics <- class_performance(car_lin_matrix)
car_lin_metrics

##log model 
log_model<- glm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=car_train,family = "binomial")
summary(log_model)

car_log_train_preds <- predict(log_model,newdata=car_train,type="response")
car_log_test_preds <- predict(log_model,newdata=car_test,type = "response")

car_log_matrix <- confusion_matrix(car_log_test_preds, car_test$IsBadBuy,.5)
car_log_matrix
car_lin_matrix
car_log_metrics <- class_performance(car_log_matrix)
car_log_metrics
car_lin_metrics

df1 <- data.frame(Auction="MANHEIM", VehicleAge=1, Make="NISSAN", Color="RED", WheelType='NULL', VehOdo=10,000, Size="COMPACT", MMRAcquisitionAuctionAveragePrice=8000, MMRAcquisitionRetailAveragePrice=10000)
predict(log_model, newdata = df1, type = "response")
