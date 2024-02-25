options(scipen=999)

# Dimensions of the data
dim(Sales_Project2)
head(Sales_Project2)

# Preprocessing
colSums(is.na(Sales_Project2))
sum(sapply(Sales_Project2, function(x) any(is.nan(x))))
sum(sapply(Sales_Project2, function(x) any(is.infinite(x))))
sum(is.nan(Sales_Project2$High_NPS))
sum(is.infinite(Sales_Project2$High_NPS))

# Correlation

# Between numeric variables (HeatMap)
library(ggcorrplot)
ggcorrplot(cor(Sales_P2[,c("Age", "Years", "Certficates", "Feedback", 
                                      "Salary", "NPS")]), 
           type = "lower", lab = TRUE, lab_size = 3, title = "Correlation Heatmap")

# Between categorical variables
cont_table <- table(Sales_Project2$Personality, Sales_Project2$High_NPS)
chisq.test(cont_table)
# This means that the two variables are independent

cont_table <- table(Sales_Project2$Female, Sales_Project2$High_NPS)
chisq.test(cont_table)

# This means that the two variables may be dependent (Don't know how much accurate this is)
library(ltm)
biserial.cor(Sales_Project2$Age, Sales_Project2$High_NPS)
biserial.cor(Sales_Project2$Years, Sales_Project2$High_NPS)
biserial.cor(Sales_Project2$Certficates, Sales_Project2$High_NPS)
biserial.cor(Sales_Project2$Feedback, Sales_Project2$High_NPS)
biserial.cor(Sales_Project2$Salary, Sales_Project2$High_NPS)

# Function to calculate and return the metrics of the model
Logistic_regression <- function(formula){
  
  data <- Sales_P2
  
  # Cross-Validations
  #STEP 1: Divide the data into training and Test set (70% and 30%)
  set.seed(42)
  data$High_NPS = ifelse(data$High_NPS == "Yes", 1, 0)
  n = dim(data)[1] #Number of rows in the dataset
  train = sample(1:n, .7*n, replace=FALSE)
  test = setdiff(1:n, train)
  
  df_train = data[train,]
  df_test = data[test,]
  
  #STEP 2: Logistic Regression
  fit = glm(formula, data = df_train, family = 'binomial')
  summary(fit)
  
  #STEP 3: # Making predictions
  p = predict(fit, df_test, type='response')
  y_pred_bin = ifelse(p>=0.5, "yes", "no")
  y_pred = predict(fit, df_test)
 
  #Step 4: Report the performance 
  # Confusion Matrix
  cm <- table(df_test$High_NPS, y_pred_bin)

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
metrics_m1 <- Logistic_regression(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary)
print("MODEL 1")
print(metrics_m1)

#Model 2 - Parameters: High correlation with NPS 
metrics_m2 <- Logistic_regression(High_NPS ~ Female + Years + Salary + Feedback + Certficates + Personality)
print("MODEL 2")
print(metrics_m2)

#Model 3 - Parameters: High_NPS ~ log(Salary) + log(Years) + Salary*Certficates + Feedback*Personality
metrics_m3 <- Logistic_regression(High_NPS ~ log(Years) + log(Salary) + Salary*Certficates + Feedback*Personality)
print("MODEL 3")
print(metrics_m3)

#Model 4 - Parameters: (Female + Feedback*Personality + Salary*Certficates)
metrics_m4 <- Logistic_regression(High_NPS ~ Female + Feedback*Personality + Salary*Certficates)
print("MODEL 4")
print(metrics_m4)

#Model 5 - Parameters: Female*Age + Salary*Certficates + Personality*Personality
metrics_m5 <- Logistic_regression(High_NPS ~ Female*Age + Salary*Certficates + Personality*Personality)
print("MODEL 5")
print(metrics_m5)

#M2, M3




