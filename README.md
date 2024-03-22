# Bayesian Networks for Censored Data: Heart Failure Analysis
#### This repository contains a report on using Bayesian Networks to analyze censored data from heart failure patients in Pakistan. The report explores various methods for handling censored data, including the Kaplan-Meier estimator, Cox's Proportional Hazard Model, and Bayesian Network approaches.

### Table of Contents
- Introduction
- Methods
- Results
- Discussion and Conclusion

### Introduction
Cardiovascular disease is a leading cause of death globally, and understanding the relationship between risk factors and cardiovascular risk is crucial for improving treatment outcomes. This report aims to develop models that can accurately predict the probability of survival for heart failure patients based on various covariates.

### Methods
The report explores the following methods:

- Bayesian Network with Inverse Probability of Censoring Weights (IPCW): This method incorporates IPCW to handle censored data and uses the Chow-Liu algorithm or expert knowledge to construct the Bayesian Network structure.
- Cox's Proportional Hazard (CPH) Model: A semi-parametric regression model for dealing with right-censored failure time data.
- Bayesian Network Interpretation of the CPH Model (BN-Cox Model): A Bayesian Network representation of the CPH model, capturing the relationship between risk factors, time, and survival.
### Results
The report presents the results of the different models, including accuracy scores, confusion matrices, and classification matrices. The BN-Cox model achieved highly similar results to the original CPH model, demonstrating its effectiveness in representing the CPH model as a Bayesian Network.

### Discussion and Conclusion
The report discusses the limitations of the Bayesian Network models for this particular dataset, which included a relatively low number of observations. It suggests that Bayesian Network approaches may be more suitable for larger, more complex datasets, such as those generated from electronic health records.
