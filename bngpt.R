install.packages("latticeExtra")
library(bnlearn)
library(survival)
library(pec)
library(readr)
library(latticeExtra)
library(dagitty)
library(ggplot2)
library(survminer)
heart_data <- read.csv("heart_failure_clinical_records_dataset.csv")

str(heart_data)
summary(heart_data)

##EXPERT KNOWLEDGE (converting continuous to discrete)
## age = 
## creatinine_phosphokinase = 10 to 120
## ejection_fraction = 50 to 70
## platelets - 150,000 to 450,000
## serum_creatinine = 0.6 to 1.3
## serum_sodium = 135 to 145

## 0 = LOW, 1 = NORMAL, 2 = HIGH


# Convert age to a discrete variable
heart_data$age <- cut(
  heart_data$age, 
  breaks = c(-Inf,40, 50, 60, 70, 80, 95), 
  labels = c(1, 2, 3, 4, 5, 6)
)


# Convert creatinine_phosphokinase to a discrete variable
heart_data$creatinine_phosphokinase <- cut(
  heart_data$creatinine_phosphokinase, 
  breaks = c(-Inf,10, 120, Inf), 
  labels = c(0, 1, 2)
)

# Convert ejection_fraction to a discrete variable
heart_data$ejection_fraction <- cut(
  heart_data$ejection_fraction, 
  breaks = c(-Inf,50, 70,Inf), 
  labels = c(0, 1, 2)
)

# Convert platelets to a discrete variable
heart_data$platelets <- cut(
  heart_data$platelets, 
  breaks = c(-Inf,150000, 450000,Inf), 
  labels = c(0, 1, 2)
)

# Convert serum_creatinine to a discrete variable
heart_data$serum_creatinine <- cut(
  heart_data$serum_creatinine, 
  breaks = c(-Inf,0.6, 1.3, Inf), 
  labels = c(0, 1, 2)
)

# Convert serum_sodium to a discrete variable
heart_data$serum_sodium <- cut(
  heart_data$serum_sodium, 
  breaks = c(-Inf, 135, 145, Inf), 
  labels = c(0, 1, 2)
)
subj_cont_data = heart_data

is.na(subj_cont_data)

subj_cont_data$age <- as.numeric(heart_data$age)
subj_cont_data$time <- as.numeric(heart_data$time)
subj_cont_data$anaemia <- as.numeric(heart_data$anaemia)
subj_cont_data$creatinine_phosphokinase <- as.numeric(heart_data$creatinine_phosphokinase)
subj_cont_data$diabetes <- as.numeric(heart_data$diabetes)
subj_cont_data$ejection_fraction <- as.numeric(heart_data$ejection_fraction)
subj_cont_data$high_blood_pressure <- as.numeric(heart_data$high_blood_pressure)
subj_cont_data$platelets <- as.numeric(heart_data$platelets)
subj_cont_data$serum_creatinine <- as.numeric(heart_data$serum_creatinine)
subj_cont_data$serum_sodium <- as.numeric(heart_data$serum_sodium)
subj_cont_data$sex <- as.numeric(heart_data$sex)
subj_cont_data$smoking <- as.numeric(heart_data$smoking)
subj_cont_data$DEATH_EVENT <- as.numeric(heart_data$DEATH_EVENT)

str(subj_cont_data)


# Calculate the ipcw weights using Kaplan-Meier method
fit <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = subj_cont_data)

#function to get the Kaplan-Meier weights
get_km_weights <- function(times, fit) {
  res <- numeric(length(times))
  for (i in seq_along(times)) {
    res[i] <- 1 / summary(fit, times = times[i])$surv
  }
  return(res)
}

# Create a censoring indicator variable
cutoff <- 100
subj_cont_data$censoring_indicator <- ifelse(heart_data$time < cutoff & !heart_data$DEATH_EVENT, 1, 0)

# Add weights to the dataset
subj_cont_data$weights <- ifelse(subj_cont_data$censoring_indicator == 1, get_km_weights(subj_cont_data$time, fit), 0)

# remove the time field from the dataset, since it is not needed now
subj_cont_data <- subj_cont_data[,!names(subj_cont_data) %in% c("time")]
# Split the data into train and test sets
set.seed(123)
train_idx <- sample(1:nrow(subj_cont_data), round(0.75 * nrow(subj_cont_data)))

train_data <- subj_cont_data[train_idx, ]
test_data <- subj_cont_data[-train_idx, ]


#WE NEED TO MODIFY THE DAG, THIS IS NOT THE FINAL DAG
node_names <- c("age", "anaemia", "platelets", "serum_creatinine", "DEATH_EVENT",
                "high_blood_pressure", "serum_sodium", "sex", "smoking",
                "creatinine_phosphokinase", "ejection_fraction")



dag <- empty.graph(node_names)
dag <- set.arc(dag, "age", "DEATH_EVENT")
dag <- set.arc(dag, "anaemia", "platelets")
dag <- set.arc(dag, "platelets", "serum_creatinine")
dag <- set.arc(dag, "serum_creatinine", "DEATH_EVENT")
dag <- set.arc(dag, "anaemia", "high_blood_pressure")
dag <- set.arc(dag, "high_blood_pressure", "serum_sodium")
dag <- set.arc(dag, "serum_sodium", "DEATH_EVENT")
dag <- set.arc(dag, "high_blood_pressure", "DEATH_EVENT")
dag <- set.arc(dag, "sex", "smoking")
dag <- set.arc(dag, "smoking", "DEATH_EVENT")
dag <- set.arc(dag, "creatinine_phosphokinase", "serum_creatinine")
dag <- set.arc(dag, "creatinine_phosphokinase", "ejection_fraction")
dag <- set.arc(dag, "ejection_fraction", "DEATH_EVENT")


plot(dag)
#extract the weights before removing them from the data to create a bn
train_weights <- train_data$weights
#removing columns from dataset that we don’t want to include in network, like weights, censoring indicator, diabetes
train_data <- train_data[,!names(train_data) %in% c("weights", "censoring_indicator", "diabetes")]
test_data <- test_data[,!names(train_data) %in% c("weights", "censoring_indicator", "diabetes")]

head(data)


# Train the network using the mle algorithm with IPCW weights
fitted_bn <- bn.fit(dag, data = train_data, method="mle", weights=train_weights, max.iter=100)

structure(fitted_bn)

test_data_prob <- predict(fitted_bn, node = "DEATH_EVENT", data = test_data, method = "bayes-lw")

predicted_outcome <- ifelse(test_data_prob >= 0.5, 1, 0)
accuracy <- sum(predicted_outcome == test_data$DEATH_EVENT) / length(test_data$DEATH_EVENT)
print(paste("Accuracy:", accuracy))

fitted_bn
