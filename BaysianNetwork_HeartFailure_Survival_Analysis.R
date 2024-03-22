# Install and load necessary packages
install.packages("readr")
install.packages("bnlearn")
install.packages("mice")

library(readr)
library(bnlearn)
library(survival)
library(ggplot2)
library(mice)
# Read the heart failure dataset into a data frame
heart_data <- read_csv("heart_failure_clinical_records_dataset.csv")

summary(heart_data)
md.pattern(heart_data)

# Fit a Cox proportional hazards model
model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, data = heart_data)

# Extract the coefficient values, standard errors, and p-values
results <- summary(model)$coef[, c(1, 2, 4, 5)]

# Calculate the exponentiated coefficients
results[, 3] <- exp(results[, 2])

# Print the results
colnames(results) <- c("coef", "log(HR)", "exp(coef)", "Pr(>|z|)")
print(results)

