options(scipen=999)
library(ROSE)
library(ggplot2)
library(e1071)
library(caret)
library(randomForest)
# Dimensions of the data
dim(Sales_P2)
head(Sales_P2)

metrics <- function(High_NPS, y_pred){
  
  #Step 4: Report the performance 
  # Confusion Matrix
  cm <- table(High_NPS, y_pred)
  
  # Precentage Confusion Matrix
  cm_per <- prop.table(cm)
  
  # Mean Square Error
  e = (High_NPS - y_pred)
  mse <- mean(e ** 2) #mean square error
  
  # Accuracy
  accuracy <- sum(diag(cm)) / sum(cm)
  accuracy
  
  #Precision and Recall
  precision <- cm[2,2] / (cm[2,2] + cm[1,2])
  recall <- cm[2,2] / (cm[2,2] + cm[2,1])
  
  metrics <- list(confusion_matrix = cm, 
                  confusion_matrix_perc = cm_per, 
                  mse = mse, accuracy = accuracy, precision = precision,
                  recall = recall)
  return(metrics)
}

# Function to calculate and return the metrics of the model
Over_sampling <- function(formula){

  data <- TechSales_Reps_Data
  data <- subset(data, Business == "Software" & College == "Yes")
  data$High_NPS <- ifelse(data$NPS >= 9, 1, 0)
  data$High_NPS_bin <- ifelse(data$NPS >= 9, "Yes", "No")
  
  # Cross-Validations
  #STEP 1: Divide the data into training and Test set (70% and 30%)
  data_oversampled <- ovun.sample(formula, data, method = "over", p = 0.5, seed = 42)$data
  data_oversampled$High_NPS = ifelse(data_oversampled$High_NPS == '1', 1, 0)
  
  data_oversampled
  
  # STEP 1: Divide the data into training and test (validation)
  set.seed(42)
  n = dim(data_oversampled)[1] # num of rows in the dataset
  train = sample(1:n, .7*n) # indexes of rows that will go to training set
  data_train = data_oversampled[train,]
  data_test = data_oversampled[-train,]

  return(list(data_train = data_train, data_test = data_test))
}

########################################## Support Vector Machine #############################################
#Model 2 - Parameters: High correlation with NPS
df_over <- Over_sampling(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary)
df_train <- df_over$data_train
df_test <- df_over$data_test

svm2 <- svm(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality , data = df_train)
p = predict(svm2, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_svm2 <- metrics(df_test$High_NPS, y_pred)
print("SVM - MODEL 2")
print(metrics_svm2)

#Model 4 - Parameters: High_NPS ~ Female + Feedback*Personality + Salary*Certficates
df_over <- Over_sampling(High_NPS ~ Female + Personality + Certficates + Feedback + Salary)
df_train <- df_over$data_train
df_test <- df_over$data_test

svm4 <- svm(High_NPS ~ Female + Feedback*Personality + Salary*Certficates, data = df_train)
p = predict(svm4, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_svm4 <- metrics(df_test$High_NPS, y_pred)
print("SVM - MODEL 3")
print(metrics_svm4)

################################### Random Forest #####################################################
# Model 2
df_over <- Over_sampling(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary)
df_train <- df_over$data_train
df_test <- df_over$data_test

RF_fit2 = randomForest(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary, data = df_train)
p = predict(RF_fit2, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_rf2 <- metrics(df_test$High_NPS, y_pred)
print("Random Forest - Model 2")
print(metrics_rf2)

# Model 3
df_over <- Over_sampling(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality)
df_train <- df_over$data_train
df_test <- df_over$data_test

RF_fit3 = randomForest(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality, data = df_train)
p = predict(RF_fit3, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_rf3 <- metrics(df_test$High_NPS, y_pred)
print("Random Forest - Model 3")
print(metrics_rf3)

####################### Logistic Regression ########################################################
#Model 2 - Parameters: High correlation with NPS 
df_over <- Over_sampling(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality)
df_train <- df_over$data_train
df_test <- df_over$data_test

LR_fit2 = glm(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality, data = df_train, family = 'binomial')
p = predict(LR_fit2, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_lr2 <- metrics(df_test$High_NPS, y_pred)
print("Logistic Regression  - Model 2")
print(metrics_lr2)

#Model 3 - Parameters: High_NPS ~ log(Salary) + log(Years) + Salary*Certficates + Feedback*Personality 
df_over <- Over_sampling(High_NPS ~ Years + Salary + Feedback + Certficates + Personality)
df_train <- df_over$data_train
df_test <- df_over$data_test

LR_fit3 = glm(High_NPS ~ log(Years) + log(Salary) + Salary*Certficates + Feedback*Personality, data = df_train, family = 'binomial')
p = predict(LR_fit3, df_test, type='response')
y_pred = ifelse(p>=.5, 1, 0)

metrics_lr3 <- metrics(df_test$High_NPS, y_pred)
print("Logistic Regression  - Model 3")
print(metrics_lr3)