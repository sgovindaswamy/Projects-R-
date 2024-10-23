# Objective:
# The dataset is designed for exploring factors influencing customer churn and retention. 
# It is ideal for developing predictive models to identify at-risk customers and understand the dynamics of customer turnover. 
# Key analyses can include identifying patterns in customer demographics, contract types, and service usage that contribute to churn.

# Applications:
# Churn Prediction: Build models to predict customer churn based on their attributes and service details.
# Customer Segmentation: Segment customers to understand different behavior patterns and tailor retention strategies.
# Trend Analysis: Analyze trends in customer tenure, monthly charges, and contract types to gain insights into churn drivers.
# Feature Engineering: Use the dataset to create features for more sophisticated churn prediction models.
library(tidyverse)
library(caret)
library(repr)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggpubr)
library(dplyr)
library(summarytools)
library(corrplot)
library(ggplot2)
library(randomForest)
library(pROC)

data <- read.csv("C:/Users/sanja/Documents/GitHub/Projects-R-/Customer_churn_prediction/customer_churn_data.csv")
glimpse(data)
data <- data[complete.cases(data),] #remove na's in case if there is any
categorical_variables <- data %>% select(where(is.character))

# Summary of the dataset
dfSummary(data)

#Convert the character columns into numeric columns
data <- data %>% mutate(across(where(is.character), ~ as.numeric(as.factor(.))))
str(data)

#####################################################################################################

#EXPLORATORY DATA ANALYSIS

data <- data %>% select(-CustomerID, -Age) #drop the unnecessary columns
numeric_data <- data %>% select(where(is.numeric))
correlation_matrix <-cor(numeric_data, use = "complete.obs")
#The correlation plot indicates that there is a strong correlation between Tenure and Total charges (corr = 0.89)

corrplot(correlation_matrix, method = "circle", type = "upper", 
         t1.col = "black", 
         t1.srt = 45, 
         addCoef.col = "black", 
         numeric.cex = 0.7)

#CATEGORICAL BAR PLOTS

categorical_variables$Gender <- factor(categorical_variables$Gender)
categorical_variables$ContractType <- factor(categorical_variables$ContractType)
categorical_variables$InternetService <- factor(categorical_variables$InternetService)
categorical_variables$TechSupport <- factor(categorical_variables$TechSupport)
categorical_variables$Churn <- factor(categorical_variables$Churn)

# Create individual plots and then combine them together
plot_gender <- ggplot(categorical_variables, aes(x = Gender, fill = Churn)) + 
  geom_bar(position = "fill") + 
  labs(title = "Churn by Gender") +
  scale_x_discrete(labels = levels(categorical_variables$Gender)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot_Contract_type <- ggplot(categorical_variables, aes(x = ContractType, fill = Churn)) + 
  geom_bar(position = "fill") + 
  labs(title = "Churn by ContractType") +
  scale_x_discrete(labels = levels(categorical_variables$ContractType)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot_Internet_Service <- ggplot(categorical_variables, aes(x = InternetService, fill = Churn)) + 
  geom_bar(position = "fill") + 
  labs(title = "Churn by Internet Service") +
  scale_x_discrete(labels = levels(categorical_variables$InternetService)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot_TechSupport <- ggplot(categorical_variables, aes(x = TechSupport, fill = Churn)) + 
  geom_bar(position = "fill") + 
  labs(title = "Churn by Tech Support") +
  scale_x_discrete(labels = levels(categorical_variables$TechSupport)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

fig1 <- ggarrange(plot_gender, plot_Contract_type, plot_Internet_Service, plot_TechSupport,
                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom") # Combine the plots into a single figure

fig1 <- annotate_figure(fig1, bottom = text_grob("Churn Percentage in Categorical Variables", 
                                                 col = "blue", face = "bold", size = 14)) # Annotate the figure
print(fig1)# Print the final combined plot


#######################################################################################################

#ANALYSIS WITH DIFFERENT MODELS

#1) Logistic regression

set.seed(123)
split <- sample.split(numeric_data$Churn, SplitRatio = 0.70)
train <- numeric_data[split, ]
test <- numeric_data[!split, ]

prop.table(table(numeric_data$Churn)) # 1 denotes "No", 2 denotes "Yes"

train$Churn <- ifelse(train$Churn == 1, 1, 0) # For glm, convert Churn into binary. 1 stays as 1 and 2 becomes 0
test$Churn <- ifelse(test$Churn == 1, 1, 0)
glm <- glm(Churn ~., data = train, family = "binomial")
summary(glm)

# Measuring accuracy for GLM
pred <- predict(glm, data = train, type = "response")
glmtab1 <- table(train$Churn, pred >= 0.5) # Confusion Matrix 

#Based on the confusion matrix, the TP ("Yes" encoded as "0") = 601, TN ("No" encoded as "1") = 49 
True_positives_train <- 601
True_negatives_train <- 49
acc_glm_train <- (True_positives_train + True_negatives_train) / nrow(train)

predtest <- predict(glm, newdata = test, type = "response")
glmtab2 <- table(test$Churn, predtest >= 0.5)
True_positives_test <- 252 # based on glmtab2 (Confusion matrix)
True_negatives_test <- 21
acc_glm_test <- (True_positives_test + True_negatives_test) / nrow(test)

#2) Random Forest

fit_rf <- randomForest(Churn ~., data = train, proximity = FALSE, importance = FALSE)
print(fit_rf)
predrf_train <- predict(fit_rf, data = train, type = "response")
predrf_train <- ifelse(predrf_train > 0.5, 1, 0)
head(predrf_train)
rftab <- table(predrf_train, train$Churn)
accuracy_rf_train <- sum(diag(rftab)) / sum(rftab)
print(paste("Accuracy of Random Forest on Training Set:", accuracy_rf_train))

predrf_test <- predict(fit_rf, newdata = test, type="response")
predrf_test <- ifelse(predrf_test > 0.5, 1, 0)
rftab_test <- table(predrf_test, test$Churn)
accuracy_rf_test <- sum(diag(rftab_test))/sum(rftab_test)
print(paste("Accuracy of random forest on test set:", accuracy_rf_test))

#Visualization

plot(fit_rf, main = "Random Forest Error Plot") # Error plot
varImpPlot(fit_rf, main = "Variable importance plot") # Importance plot

#Interpretation

# VarImpPlot shows the IncNodePurity quantifies how much a particular variable helps improve the "purity" of the nodes 
# in a tree during the training process. Higher IncNodePurity: Variables with
# higher values for IncNodePurity contribute more to improving the purity of the nodes in the decision trees. 
# These variables are important in predicting the target variable.
# Lower IncNodePurity: Variables with lower values contribute less to the model. 
# In extreme cases, they may not improve the model's splits at all.

############################################################################################

# COMPARISON BETWEEN 2 MODELS

# ROC Curve for Logistic Regression
roc_logit <- roc(test$Churn, predtest)
auc_logit <- auc(roc_logit)
# ROC Curve for Random Forest
roc_rf <- roc(test$Churn, predrf_test)
auc_rf <- auc(roc_rf)
# Create a combined ROC plot
plot(roc_logit, col = "blue", main = "ROC Curves for Logistic Regression and Random Forest", legacy.axes = TRUE)
plot(roc_rf, add = TRUE, col = "red")
# Add legend
legend("bottomright", legend = c(paste("Logistic Regression AUC =", round(auc_logit, 3)),
                                 paste("Random Forest AUC =", round(auc_rf, 3))),
       col = c("blue", "red"), lwd = 2)

#################################################################################################