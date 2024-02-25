options(scipen=999)
library(ROSE)
library(ggplot2)

data <- TechSales_Reps_Data
data <- subset(data, Business == "Software" & College == "Yes")
data$High_NPS <- ifelse(data$NPS >= 9, 1, 0)
data$High_NPS_bin <- ifelse(data$NPS >= 9, "Yes", "No")

# Cross-Validations
#STEP 1: Divide the data into training and Test set (70% and 30%)
data_oversampled <- ovun.sample(High_NPS_bin ~ ., data, method = "over", p = 0.5, seed = 42)$data
data_oversampled$High_NPS = ifelse(data_oversampled$High_NPS == '1', 1, 0)

data_oversampled

write.csv(data_oversampled, file = "Data_Frame_Oversample.csv", row.names = FALSE)

library(ggcorrplot)
ggcorrplot(cor(Sales_P2[,c("Age", "Years", "Certficates", "Feedback", 
                           "Salary", "NPS")]), 
           type = "lower", lab = TRUE, lab_size = 3, title = "Correlation Heatmap")
