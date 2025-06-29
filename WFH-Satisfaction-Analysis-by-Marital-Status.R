rm(list=ls())

install.packages("estimatr")
install.packages("fixest")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("haven")
install.packages("stargazer")
install.packages("dplyr")
install.packages("sandwich")
install.packages("lmtest")

### Packages
library(estimatr)
library(dplyr)
library(foreign)
library(stargazer)
library(haven)
library(ggplot2)
library(Hmisc)
library(ggplot2)
library(chron)
library(lattice)
library(dummies)
library(lfe)
library(fixest)
library(sandwich)
library(lmtest)
library(miceadds)
library(multiwayvcov)
library(dplyr)
library(sandwich)
library(lmtest)

Satisfaction_data <-data.frame(Satisfaction)
stargazer(Satisfaction_data,type="text")

# Filter the data for employees working from home (expgroup_treatment = 1)
Satisfaction_wfh_data <- Satisfaction_data[Satisfaction_data$expgroup_treatment == 1, ]

# View the filtered data
View(Satisfaction_wfh_data)



#Checking for any null values in Overall Satisfaction column
any(is.na(Satisfaction_wfh_data))


# Create summary statistics table as HTML
Satisfaction_de <- data.frame(Satisfaction_wfh_data)
stargazer(Satisfaction_de, 
          type = "html", 
          title = "Figure 1: Dataset Overview", 
          out = "Satisfaction_summary.html")




# 1. Marital Status Distribution: Married vs Single (WFH) 
  ggplot(data = Satisfaction_wfh_data, aes(x = factor(married), fill = factor(married))) + 
  geom_bar() +
  labs(
    x = "Marital Status",
    y = "Count", 
    title = "Figure 2: Marital Status Distribution of Employees" 
  ) +
  scale_x_discrete(labels = c("0" = "Single", "1" = "Married")) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "darkorange"), 
    labels = c("Single", "Married"),
    name = "Marital Status"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# 2. Average Satisfaction by Marital Status 
  Satisfaction_wfh_data %>%
    group_by(married) %>%
    summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE)) %>%
    ggplot(aes(x = factor(married), y = avg_satisfaction, fill = factor(married))) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(x = "Marital Status", y = "Average Satisfaction", 
         title = "Figure 3: Average Satisfaction by Marital Status") +
    scale_x_discrete(labels = c("0" = "Single", "1" = "Married")) +
    scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  

# 3. Average Satisfaction by Gender and Marital Status 
  Satisfaction_wfh_data %>%
    group_by(men, married) %>%
    summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE)) %>%
    ggplot(aes(x = factor(men), y = avg_satisfaction, fill = factor(married))) +
    geom_bar(stat = "identity", position = "dodge", show.legend = TRUE) +
    labs(x = "Gender", y = "Average Satisfaction",
         title = "Figure 4: Average Satisfaction by Gender & Marital Status") +
    scale_x_discrete(labels = c("0" = "Women", "1" = "Men")) +
    scale_fill_manual(name = "Marital Status", 
                      labels = c("Single", "Married"), 
                      values = c("0" = "steelblue", "1" = "darkorange")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  

# 4. Average Satisfaction by Having or Not Having Bedroom (WFH employees)

  Satisfaction_wfh <- Satisfaction_wfh_data  # Reset, as you did
  
  Satisfaction_wfh %>%
    group_by(bedroom) %>%
    summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE)) %>%
    ggplot(aes(x = factor(bedroom), y = avg_satisfaction, fill = factor(bedroom))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "Figure 5: Average Satisfaction by Bedroom ",
      x = "Has Separate Bedroom",
      y = "Average Satisfaction"
    ) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange")) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  Satisfaction_wfh <- Satisfaction_wfh_data
  
  # Create the plot for average satisfaction by marital status
  Satisfaction_wfh %>%
    group_by(married) %>%
    summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE)) %>%
    ggplot(aes(x = factor(married), y = avg_satisfaction, fill = factor(married))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "Figure: Average Satisfaction by Marital Status",
      x = "Marital Status",
      y = "Average Satisfaction"
    ) +
    scale_x_discrete(labels = c("0" = "Single", "1" = "Married")) +
    scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange")) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  

  
#HYPOTHESIS
# Perform the t-test to check if marital status affects satisfaction
t_test_result <- t.test(satisfaction ~ factor(married), data = Satisfaction_wfh_data, 
                        var.equal = TRUE)  

# Display the result
print(t_test_result)


#MODELS


# Model 1: Simple Model with Married and Satisfaction (No Controls)
model_married <- lm(satisfaction ~ married, data = Satisfaction_wfh_data)

# #Model 2:  Main Model with Married and Relevant Control Variables
model_main <- lm(satisfaction ~ married + age + tenure + grosswage + bedroom + men + children + high_educ,
                 data = Satisfaction_wfh_data)
 
# Compute clustered standard errors for each model individually
cluster_se1 <- sqrt(diag(vcovCL(model_married, cluster = ~personid)))
cluster_se2 <- sqrt(diag(vcovCL(model_main, cluster = ~personid)))



# Generate Stargazer output with all models
stargazer(model_married, model_main, model_genderint,
          type = "html",  # Use "text" for console output if preferred
          se = list(cluster_se1, cluster_se2, cluster_se3),
          title = "  Table 1 : Dedicated Bedroom Workspace Is a Key Predictor of Satisfaction Among WFH Employees",
          dep.var.labels = "Satisfaction",
          column.labels = c("Model 1", "Model 2"),
          covariate.labels = c("Married", "Age", "Tenure", "Gross Wage", "Bedroom", 
                               "Men", "Children", "Higher Education", 
                               "Married × Men","Constant"),
          digits = 3,
          align = TRUE,
          out = "all_models_clustered.html") 

