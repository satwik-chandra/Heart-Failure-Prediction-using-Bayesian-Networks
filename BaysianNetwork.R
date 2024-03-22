install.packages("dagitty")
install.packages("survminer")
library(survival)
library(bnlearn)
library(ipcwswitch)
library(dagitty)
library(ggplot2)
library(survminer)
#Download Dataset

working_directory <- setwd("D:\\RStudioProjects\\Baysian Networks on Heart Failure")

heart_data <- read.csv("heart_failure_clinical_records_dataset.csv")
data <- heart_data

head(heart_data)

# Create a density plot of follow-up times
follow_up_density_plot <- ggplot(heart_data, aes(x = time)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = "Follow-up Time", y = "Density", title = "Follow-up Time Density Plot") +
  theme_minimal()

# Display the plot
print(follow_up_density_plot)


# Create the survival object
surv_object <- Surv(heart_data$time, event = heart_data$DEATH_EVENT, data = heart_data)

# Fit the Kaplan-Meier estimator
km_fit <- survfit(surv_object ~ 1)


# Plot the Kaplan-Meier curve
plot(km_fit, xlab = "Time", ylab = "Survival Probability", main = "Kaplan-Meier Estimate")

# Add a legend (optional)
legend("bottomleft", legend = "All Patients", lty = 1, col = 1, bty = "n")


# Cox Model 
cox_fit <- coxph(surv_object ~ age + anaemia + creatinine_phosphokinase +       ejection_fraction + high_blood_pressure + platelets
                 +serum_creatinine + serum_sodium + sex + smoking, data = heart_data)

# Print the summary
summary(cox_fit)


# Plot the baseline survival function
ggsurvplot(survfit(cox_fit), heart_data, palette = "#2E9FDF",
           ggtheme = theme_minimal())



# Create a censoring indicator variable
cutoff <- 100
heart_data$censoring_indicator <- ifelse(heart_data$time < cutoff & !heart_data$DEATH_EVENT, 1, 0)

# Fit Kaplan-Meier estimator on censoring indicator
km_censoring <- survfit(Surv(time, censoring_indicator) ~ 1, data = heart_data)

# Compute IPCW weights
heart_data$IPCW_weights <- 1 / summary(km_censoring, times = heart_data$time)$surv


#DAG



# Create an empty network with the nodes
node_names <- colnames(heart_data)
dag <- empty.graph(node_names)
dag <- set.arc(dag, "age", "ejection_fraction")
dag <- set.arc(dag, "age", "serum_creatinine")
dag <- set.arc(dag, "age", "serum_sodium")
dag <- set.arc(dag, "age", "DEATH_EVENT")
dag <- set.arc(dag, "anaemia", "platelets")
dag <- set.arc(dag, "anaemia", "high_blood_pressure")
dag <- set.arc(dag, "creatinine_phosphokinase", "ejection_fraction")
dag <- set.arc(dag, "creatinine_phosphokinase", "serum_creatinine")
dag <- set.arc(dag, "ejection_fraction", "DEATH_EVENT")
dag <- set.arc(dag, "high_blood_pressure", "DEATH_EVENT")
dag <- set.arc(dag, "high_blood_pressure", "serum_sodium")
dag <- set.arc(dag, "platelets", "serum_creatinine")
dag <- set.arc(dag, "serum_creatinine", "DEATH_EVENT")
dag <- set.arc(dag, "serum_sodium", "DEATH_EVENT")
dag <- set.arc(dag, "sex", "smoking")
dag <- set.arc(dag, "smoking", "DEATH_EVENT")


#Fixing Data Types
data$anaemia <- as.factor(heart_data$anaemia)
data$sex <- as.factor(heart_data$sex)
data$smoking <- as.factor(heart_data$smoking)
data$DEATH_EVENT <- as.factor(heart_data$DEATH_EVENT)
data$diabetes <- as.factor(heart_data$diabetes)
data$creatinine_phosphokinase <- as.factor(heart_data$creatinine_phosphokinase)
data$ejection_fraction <- as.factor(heart_data$ejection_fraction)
data$high_blood_pressure <- as.factor(heart_data$high_blood_pressure)
data$platelets <- as.factor(heart_data$platelets)
data$serum_creatinine <- as.factor(heart_data$serum_creatinine)
data$serum_sodium <- as.factor(heart_data$serum_sodium)
data$IPCW_weights <- heart_data$IPCW_weights

#removing columns from dataset that we don’t want to include in network
data <- subset(data, select = -diabetes)




# Train the network using the EM algorithm with IPCW weights
fitted_bn <- bn.fit(dag, data = data, method="EM", init=init_params, weights=heart_data$IPCW_weights, max.iter=100)






