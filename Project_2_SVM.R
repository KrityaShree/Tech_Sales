# load the required libraries
library(e1071) # for svm() function
library(caret) # for trainControl() and train() functions
library(tidyverse) # for data manipulation and visualization

options(scipen=999)

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
ggcorrplot(cor(Sales_P2[,c("Age", "Years", "Certficates", "Feedback", 
                           "Salary", "NPS")]), 
           type = "lower", lab = TRUE, lab_size = 3, title = "Correlation Heatmap")

# Between categorical variables
cont_table <- table(data$Personality, data$High_NPS)
chisq.test(cont_table)
# This means that the two variables are independent

cont_table <- table(data$Female, data$High_NPS)
chisq.test(cont_table)
options(scipen=999)

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
ggcorrplot(cor(Sales_P2[,c("Age", "Years", "Certficates", "Feedback", 
                           "Salary", "NPS")]), 
           type = "lower", lab = TRUE, lab_size = 3, title = "Correlation Heatmap")

# Between categorical variables
cont_table <- table(data$Personality, data$High_NPS)
chisq.test(cont_table)
# This means that the two variables are independent

cont_table <- table(data$Female, data$High_NPS)
chisq.test(cont_table)

# This means that the two variables may be dependent (Don't know how much accurate this is)
library(ltm)
biserial.cor(data$Age, data$High_NPS)
biserial.cor(data$Years, data$High_NPS)
biserial.cor(data$Certficates, data$High_NPS)
biserial.cor(data$Feedback, data$High_NPS)
biserial.cor(data$Salary, data$High_NPS)

# Function to calculate and return the metrics of the model
svm <- function(formula){
  
  data <- Sales_P2
}
  
  # Cross-Validations
  #STEP 1: Divide the data into training and Test set (70% and 30%)
  set.seed(42)
  data$High_NPS = ifelse(data$High_NPS == "Yes", 1, 0)
  n = dim(data)[1] #Number of rows in the dataset
  train = sample(1:n, .7*n, replace=FALSE)
  test = setdiff(1:n, train)
  
  df_train = data[train,]
  df_test = data[test,]
  
  df_train

  #STEP 2: Support Vector Machine
  fit <- svm(High_NPS ~ Age + Female + Years + Personality + Certficates + Feedback + Salary, data = df_train)
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
  precision
  recall

