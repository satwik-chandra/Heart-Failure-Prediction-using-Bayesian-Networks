######## LIBRARIES ##############
library(bnlearn)
library(survival)
library(pec)
library(readr)
library(latticeExtra)
library(dagitty)
library(ggplot2)
library(survminer)
library(corrplot)
library(tidyr)
library(bnstruct)

### Function definitions ######

#function to get the Kaplan-Meier weights
get_km_weights <- function(times, fit) {
  res <- numeric(length(times))
  for (i in seq_along(times)) {
    res[i] <- 1 / summary(fit, times = times[i])$surv
  }
  return(res)
}

############ Reading the data ################
heart_data <- read.csv("heart_failure_clinical_records_dataset.csv")

str(heart_data)

summary(heart_data)


## Check for missingness

print("Position of missing values -")
which(is.na(heart_data))
  
# count total missing values 
print("Count of total missing values - ")
sum(is.na(heart_data))


#make a correlation plot for the dataset

cor_matrix <- cor(heart_data)

# Basic correlation plot
corrplot(cor_matrix, method = "circle")  # 'method' can be "circle", "square", "ellipse", or "number"

# Customized correlation plot
corrplot(cor_matrix, method = "color", 
         type = "lower",                     # Display lower triangle
         tl.cex = 0.7,                        # Label font size
         tl.col = "black",                    # Label color
         col = colorRampPalette(c("white", "blue"))(20),  # Color scheme
         addCoef.col = "black",               # Coefficient label color
         number.cex = 0.7,                    # Coefficient font size
         title = "Correlation Plot")          # Plot title

# After observingthe plot, we remove the columns sex and diabetes because of 0 correlation with DEATH_EVENT

heart_data <- subset(heart_data, select = -c(sex, diabetes))

# Sample 3 + 3 points from death/censored groups, and plot them for comparison

plot_censoring <- heart_data %>% group_by(DEATH_EVENT) %>% sample_n(3) %>% ungroup() %>% select(time, DEATH_EVENT)

plot_censoring %>%
  mutate(
    time_start = 0, 
    case_id = factor(c(1:nrow(plot_censoring))),
    death_event = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))
  ) %>%
  pivot_longer(
    cols = c(time, time_start),
    names_to = "source",
    values_to = "time"
  ) %>%
  ggplot(aes(x = time, y = case_id, group = factor(case_id))) + 
  geom_bar(stat = "Identity", aes(fill = death_event), colour = "black", width = 0.3) +
  ggtitle("Time till Death/Censoring - 6 sampled cases from dataset") + 
  theme(plot.title = element_text(size = 22), 
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.title = element_text(size = 16))


# Kaplan-meier analysis
km_model <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = heart_data)
summary(km_model, times = seq(from = 0, to = 290, by = 30))

# Plot Kaplan-Meier plot
options(repr.plot.width = 18, repr.plot.height = 8)


ggsurvplot(km_model, data = heart_data, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.15)


# Create a censoring indicator variable
cutoff <- 100
heart_data$censoring_indicator <- ifelse(heart_data$time < cutoff & !heart_data$DEATH_EVENT, 1, 0)


# Add weights to the dataset
heart_data$weights <- ifelse(heart_data$censoring_indicator == 1, get_km_weights(heart_data$time, km_model), 0)


############## Discretizing the dataset ####################

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

# Convert time to a discrete variable
heart_data$time <- cut(
  heart_data$time, 
  breaks = c(0, 100, 200, Inf), 
  labels = c(0, 1, 2)
)

subj_cont_data = heart_data

subj_cont_data$age <- as.numeric(heart_data$age)
subj_cont_data$time <- as.numeric(heart_data$time)
subj_cont_data$anaemia <- as.numeric(heart_data$anaemia)
subj_cont_data$creatinine_phosphokinase <- as.numeric(heart_data$creatinine_phosphokinase)
subj_cont_data$ejection_fraction <- as.numeric(heart_data$ejection_fraction)
subj_cont_data$high_blood_pressure <- as.numeric(heart_data$high_blood_pressure)
subj_cont_data$platelets <- as.numeric(heart_data$platelets)
subj_cont_data$serum_creatinine <- as.numeric(heart_data$serum_creatinine)
subj_cont_data$serum_sodium <- as.numeric(heart_data$serum_sodium)
subj_cont_data$smoking <- as.numeric(heart_data$smoking)
subj_cont_data$DEATH_EVENT <- as.numeric(heart_data$DEATH_EVENT)

subj_cont_data

############ Export the dataset ##############
write.csv(subj_cont_data, "D:\\RStudioProjects\\Baysian Networks on Heart Failure\\Python\\bn_dataset_with_weights&v.csv", row.names=FALSE)
##########################################################################################

# Split the data into train and test sets
set.seed(123)
train_idx <- sample(1:nrow(subj_cont_data), round(0.75 * nrow(subj_cont_data)))

train_data <- subj_cont_data[train_idx, ]
test_data <- subj_cont_data[-train_idx, ]

