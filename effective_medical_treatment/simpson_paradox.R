setwd("C:/Users/sanja/Documents/GitHub/Projects-R-/Assessing_the_effectiveness_of_medical_treatment")
current_dir <- getwd()
install.packages("openxlsx")
library(openxlsx)
library(ggplot2)
install.packages("stats")
library(stats)

data <- read.csv("kidney_stone_data.csv")

############## Descriptive statistics visualization##########################""

#Overall success rate in an aggreated view without considering the confounder variable 

ggplot(data = data, aes(x = treatment, y=success)) + stat_summary(fun = mean, geom = "bar", fill = "skyblue") + 
  labs(title = "Overall success rate by treatment", x = "Treatment", y = "mean success rate")

#Success rate in a non-aggregrated view using kidney stone size as the confounder variable

ggplot(data = data, aes(x= treatment, y=success, fill=stone_size)) + stat_summary(fun = mean, geom="bar", position ="dodge") +
  labs(title = "Overall success rate by treatment and kidney stone size", x = "Treatment", y = "mean success rate")

###########################################################################

#Step 1: Contingency table using x and y

table(data$treatment, data$success)
prop.table(table(data$treatment, data$success), 1)

#Step 2: Contingency table using x and y along with confounder variable

table(data$treatment, data$success, data$stone_size)
prop.table(table(data$treatment, data$success, data$stone_size), c(1, 2))

#Step 3: Multinomial logistic regression to infer simpson paradox and identify best treatment

table_treatment_success <- table(data$treatment, data$success)

model_binary <- glm(success ~ treatment, family = binomial(), data = data)
summary(model_binary)
pred_model_binary <- predict(model_binary, type = "response")
data$pred1 <- pred_model_binary

model_triple_var <- glm(success ~ treatment + stone_size, family = binomial(), data = data)
summary(model_triple_var)
pred_model_triple <- predict(model_triple_var, type = "response")
data$pred2 <- pred_model_triple


# Plot Predicted Probabilities for both models
ggplot(data) +
  geom_jitter(aes(x = treatment, y = pred1, color = "Model 1 (no stone_size)"), width = 0.2, height = 0, alpha = 0.6) +
  geom_smooth(aes(x = treatment, y = pred1, color = "Model 1 (no stone_size)"), method = "loess", se = FALSE, size = 1) +
  geom_jitter(aes(x = treatment, y = pred2, color = "Model 2 (2 different stone size)"), width = 0.2, height = 0, alpha = 0.6) +
  geom_smooth(aes(x = treatment, y = pred2, color = "Model 2 (2 different stone size)"), method = "loess", se = FALSE, size = 1) +
  labs(title = "Comparison of Predicted Probabilities for Both Models",
       y = "Predicted Probability of Success") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.title = element_blank())


###############################################################################################"
AIC(model_binary, model_triple_var)
