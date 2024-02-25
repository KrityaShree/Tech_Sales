options(scipen=999)
library(randomForest)
data <- Sales_P2
# Dimensions of the data
dim(data)
head(data)

# Preprocessing
colSums(is.na(data))
sum(sapply(data, function(x) any(is.nan(x))))
sum(sapply(data, function(x) any(is.infinite(x))))
sum(is.nan(data$High_NPS))
sum(is.infinite(data$High_NPS))

# Correlation

# Between numeric variables (HeatMap)
library(ggcorrplot)
ggcorrplot(cor(data[,c("Age", "Years", "Certficates", "Feedback", 
                           "Salary", "NPS")]), 
           type = "lower", lab = TRUE, lab_size = 3, title = "Correlation Heatmap")

# Between categorical variables
cont_table <- table(data$Personality, data$High_NPS)
chisq.test(cont_table)
# This means that the two variables are independent

cont_table <- table(data$Female, data$High_NPS)
chisq.test(cont_table)

# This means that the two variables may be dependent (Don't know how much accurate this is)
library(MASS)
library(msm)
library(polycor)
library(ltm)
biserial.cor(data$Age, data$High_NPS)
biserial.cor(data$Years, data$High_NPS)
biserial.cor(data$Certficates, data$High_NPS)
biserial.cor(data$Feedback, data$High_NPS)
biserial.cor(data$Salary, data$High_NPS)

# Function to calculate and return the metrics of the model
Random_Forest <- function(formula){
  
  data <- Sales_P2
  
  # Cross-Validations
  #STEP 1: Divide the data into training and Test set (70% and 30%)
  set.seed(42)
  data$Personality <- as.factor(data$Personality)
  data$Female <- as.factor(data$Female)
  data$High_NPS <- as.factor(data$High_NPS)
  
  n = dim(data)[1] #Number of rows in the dataset
  train = sample(1:n, .7*n, replace=FALSE)
  test = setdiff(1:n, train)
  
  df_train = data[train,]
  df_test = data[test,]
  
  #STEP 2: Logistic Regression
  fit = randomForest(formula, data = df_train)
  summary(fit)
  
  #STEP 3: # Making predictions
  #p = predict(fit, df_test, type='response')
  #y_pred_bin = ifelse(p>=0.5, "yes", "no")
  y_pred = predict(fit, df_test)
  
  #Step 4: Report the performance 
  # Confusion Matrix
  cm <- table(df_test$High_NPS, y_pred)
  
  # Precentage Confusion Matrix
  cm_per <- prop.table(cm)
  
  # Mean Square Error
  e = (df_test$High_NPS - y_pred)
  mse <- mean(e ** 2) #mean square error
  
  # Accuracy
  accuracy <- sum(diag(cm)) / sum(cm)
  accuracy
  
  #Precision and Recall
  precision <- cm[2,2] / (cm[2,2] + cm[1,2])
  recall <- cm[2,2] / (cm[2,2] + cm[2,1])
  
  metrics <- list(summary = summary(fit), confusion_matrix = cm, 
                  confusion_matrix_perc = cm_per, 
                  mse = mse, accuracy = accuracy, precision = precision,
                  recall = recall)
  return(metrics)
}

#Model 1 - Parameters: All relavent attributes (Age + Female + Years + Personality + Certficates + Feedback + Salary)
metrics_m1 <- Random_Forest(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary)
print("MODEL 1")
print(metrics_m1)

#Model 2 - Parameters: High correlation with NPS 
metrics_m2 <- Random_Forest(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality)
print("MODEL 2")
print(metrics_m2)

#Model 3 - Parameters: High_NPS ~ Feedback*Certficates + Female + Age*Salary
metrics_m3 <- Random_Forest(High_NPS ~ Feedback*Certficates + Female + Age*Salary)
print("MODEL 3")
print(metrics_m3)

#Model 4 - Parameters: (Female + Feedback*Personality + Salary*Certficates)
metrics_m4 <- Random_Forest(High_NPS ~ Female + Feedback*Personality + Salary*Certficates)
print("MODEL 4")
print(metrics_m4)

#Model 5 - Parameters: Female*Age + Salary*Certficates + Personality*Personality
metrics_m5 <- Random_Forest(High_NPS ~ Female*Age + Salary*Certficates + Personality*Personality)
print("MODEL 5")
print(metrics_m5)

#M1, M2