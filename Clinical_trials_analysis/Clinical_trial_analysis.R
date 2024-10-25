library(dplyr)
library(summarytools)
library(caret)
library(earth)
library(pROC)
library(ggplot2)
data <- read.csv("C:/Users/sanja/Documents/GitHub/Projects-R-/Clinical_trials_analysis/TCGA_GBM_LGG_Mutations_all.csv")
glimpse(data)

annotated_data <- read.csv("C:/Users/sanja/Documents/GitHub/Projects-R-/Clinical_trials_analysis/TCGA_InfoWithGrade.csv")
glimpse(annotated_data)


#Summary of the dataset
summary <- dfSummary(data)
view(summary, file = "C:/Users/sanja/Documents/Github/Projects-R-/Clinical_trials_analysis/dataset_summary.html")

summary_annotated_data <- dfSummary(annotated_data)
view(summary_annotated_data, file = "C:/Users/sanja/Documents/Github/Projects-R-/Clinical_trials_analysis/annotated_summary.html")


# LOGISTIC REGRESSION 

glm_model <- glm(Grade ~ IDH1 + TP53 + ATRX + PTEN +
                   EGFR + CIC + MUC16 + PIK3CA + NF1 +
                   PIK3R1 + FUBP1 + RB1 + NOTCH1 + BCOR + 
                   CSMD3 + SMARCA4 + GRIN2A + IDH2 + FAT4 +
                   PDGFRA, data = annotated_data, family = "binomial")
summary(glm_model)

predicted_prob <- predict(glm_model, type = "response")  # Predicted probabilities

predicted_class <- ifelse(predicted_prob > 0.5, 1, 0) #assuming threshold 0.5

accuracy <- mean(predicted_class == annotated_data$Grade) #Accuracy
cat("Accuracy:", accuracy, "\n") 

precision <- sum(predicted_class == 1 & annotated_data$Grade == 1) / sum(predicted_class == 1) #Precision
recall <- sum(predicted_class == 1
              & annotated_data$Grade == 1) / sum(annotated_data$Grade == 1) #Recall
f1_score <- 2 * (precision * recall) / (precision + recall) #F1 score

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

png(filename = "roc_curve_logistic_regression.png", width = 1200, height = 1000)
roc_curve <- roc(annotated_data$Grade, predicted_prob) #ROC curve
plot(roc_curve, main = "ROC Curve for logistic regression model", col = "blue") # Plot ROC curve
abline(a = 0, b = 1, col = "red", lty = 2)
dev.off()

auc(roc_curve)  # Calculate AUC

# MULTIVARIABLE ADAPTIVE REGRESSION SPLINES

mars_model <- earth(Grade ~ IDH1 + TP53 + ATRX + PTEN +
                      EGFR + CIC + MUC16 + PIK3CA + NF1 +
                      PIK3R1 + FUBP1 + RB1 + NOTCH1 + BCOR + 
                      CSMD3 + SMARCA4 + GRIN2A + IDH2 + FAT4 +
                      PDGFRA, data = annotated_data)
summary(mars_model)
mars_predictions <- predict(mars_model, annotated_data, type = "class")

png(filename = "Residuals VS predicted values (MARS model).png", width = 1200, height = 1000)
residuals <- mars_model$residuals # Residuals Plot
plot(mars_predictions, residuals, 
     main = "Residuals VS predicted values (MARS model)",
     xlab = "Predicted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

png(filename = "Observed vs Predicted (MARS model).png", width = 1200, height =  1000)
plot(annotated_data$Grade, mars_predictions,
     main = "Observed vs Predicted (MARS model)",
     xlab = "Observed Values",
     ylab = "Predicted Values")
abline(0, 1, col = "blue") # Plot observed vs predicted values
dev.off()

png(filename = "Variable Importance (MARS model).png", width = 1200, height = 1000)
var_imp <- evimp(mars_model) # Variable importance plot
plot(var_imp, main = "Variable Importance (MARS model)")
dev.off()

mse <- mean((mars_predictions - annotated_data$Grade)^2) # Calculate Mean Squared Error (MSE)
cat("Mean Squared Error:", mse, "\n") #Lower values indicate better performance.

ss_total <- sum((annotated_data$Grade - mean(annotated_data$Grade))^2) # Calculate R-squared
ss_residual <- sum(residuals^2)
r_squared <- 1 - (ss_residual / ss_total)
cat("R-squared:", r_squared, "\n") #A value closer to 1 indicates a good fit.

# CLUSTER SIMILAR MUTATION PROFILES

gene_data <- annotated_data[, c("IDH1", "TP53", "ATRX", "PTEN", "EGFR","CIC", "MUC16", "PIK3CA", "NF1", 
                                  "PIK3R1", "FUBP1", "RB1", "NOTCH1", 
                                  "BCOR", "CSMD3", "SMARCA4", "GRIN2A", 
                                  "IDH2", "FAT4", "PDGFRA")]
gene_data_scaled <- scale(gene_data)
set.seed(123)
kmeans_result <- kmeans(gene_data_scaled, centers = 3, nstart = 25) # nstart is the number of times the algorithm will run
pca_result <- prcomp(gene_data_scaled, center = TRUE, scale. = TRUE)
pca_data <- data.frame(PC1 = pca_result$x[, 1], 
                       PC2 = pca_result$x[, 2], 
                       cluster = as.factor(kmeans_result$cluster))

png(filename = "PCA plot of gene mutations with k-means clustering.png", width = 1200, height = 1000)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point(size = 3) + 
  labs(title = "PCA plot of gene mutations with k-means clustering", 
       x = "Principal component 1", 
       y = "Principal component 2") +
  theme_minimal()
dev.off()

annotated_data$cluster <- kmeans_result$cluster
gene_clusters <- annotated_data[, c("IDH1", "TP53", "ATRX", "PTEN", "EGFR", 
                                    "CIC", "MUC16", "PIK3CA", "NF1", 
                                    "PIK3R1", "FUBP1", "RB1", "NOTCH1", 
                                    "BCOR", "CSMD3", "SMARCA4", "GRIN2A", 
                                    "IDH2", "FAT4", "PDGFRA", "cluster")]
for(i in 1:max(gene_clusters)){
  cat("\ncluster", i, "contains the following samples : \n")
  print(gene_clusters[gene_clusters$cluster == i, ]) #Mutated gene samples belonging to each cluster
}







