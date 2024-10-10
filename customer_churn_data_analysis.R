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

data <- read.csv("C:/Users/sanja/Documents/GitHub/Projects-R-/customer_churn_data.csv")
print(data)


