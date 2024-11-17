library(dplyr)
library(summarytools)
library(caret)
library(earth)
library(pROC)
library(ggplot2)
library(survival)
library(survminer)

setwd("C:/Users/sanja/Documents/GitHub/Projects-R-/Survival_analysis")
current_dir <- getwd()
data <- read.csv("Breast Cancer METABRIC.csv")
glimpse(data)


# Function to calculate the mode
get_mode <- function(x) {
  uniq_vals <- unique(na.omit(x))  # Remove NA and find unique values
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Replace missing values with mode for a specific column (e.g., column1)
data$Overall.Survival.Status[is.na(data$Overall.Survival.Status)] <- get_mode(data$Overall.Survival.Status)
print(any(is.na(data$Overall.Survival.Status)))

data$Overall.Survival..Months.[is.na(data$Overall.Survival..Months.)] <- get_mode(data$Overall.Survival..Months.)
print(any(is.na(data$Overall.Survival..Months.)))

data$Relapse.Free.Status[is.na(data$Relapse.Free.Status)] <- get_mode(data$Relapse.Free.Status)
print(any(is.na(data$Relapse.Free.Status)))

data$Relapse.Free.Status..Months.[is.na(data$Relapse.Free.Status..Months.)] <- get_mode(data$Relapse.Free.Status..Months.)
print(any(is.na(data$Relapse.Free.Status..Months.)))

# KAPLAN MEIER CURVES

# For survival event, "Deceased" label is more common that corresponds to event happened, 
# but in relapse event "Not Recurred" label is more common and that corresponds to event didn't happen.
# Event of interest or the most common event should be coded as 1 by convention


data$Relapse.Free.Status <- ifelse(data$Relapse.Free.Status == "Recurred", 0, 1) 
surv_object_relapse <- Surv(data$Relapse.Free.Status..Months., data$Relapse.Free.Status)
km_fit_relapse <- survfit(surv_object_relapse ~ 1) # 1 denotes we are calculating the kaplan Meier curve without including any predictor
png("KaplanMeier_relapse_plot.png", width = 1200, height = 800)
# Plot the Kaplan-Meier curve with a risk table
ggsurvplot(
  km_fit_relapse,  # The Kaplan-Meier fit object
  data = data,  # The data used to create the survival object
  xlab = "Time in months",
  ylab = "Probability of relapse to not recurr",
  main = "Kaplan-Meier Relapse Curve",
  col = "blue",  # Line color for the curve
  lty = 1,       # Line type for the curve
  conf.int = TRUE,  # Show confidence interval
  risk.table = TRUE,  # Add risk table below the curve
  risk.table.col = "strata",  
  ggtheme = theme_minimal()  # Use a minimal theme for the plot
)
dev.off()


data$Overall.Survival.Status <- ifelse(data$Overall.Survival.Status == "Living", 0, 1)
surv_object_survival <- Surv(data$Overall.Survival..Months., data$Overall.Survival.Status)
km_fit_surv <- survfit(surv_object_survival ~ 1)
png("kaplanMeier_survival_plot.png", width = 1200, height = 800)
# Plot the Kaplan-Meier curve with a risk table
ggsurvplot(
  km_fit_surv,  # The Kaplan-Meier fit object
  data = data,  # The data used to create the survival object
  xlab = "Time in months",
  ylab = "Probability of deadth to occur",
  main = "Kaplan-Meier Relapse Curve",
  col = "blue",  # Line color for the curve
  lty = 1,       # Line type for the curve
  conf.int = TRUE,  # Show confidence interval
  risk.table = TRUE,  # Add risk table below the curve
  ggtheme = theme_minimal()  
)
dev.off()
