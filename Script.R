# Importing Dataset

Data = read.csv2("./Dataset/TelecomChurn.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")
variables = colnames(Data)
categorical_variables = c("State", "International.plan", "Voice.mail.plan", "Area.code")
target_variable = "Churn"
numerical_variables = setdiff(variables, c(categorical_variables, target_variable))

for (var in numerical_variables) {
  Data[[var]] = as.numeric(Data[[var]])
}

for (var in categorical_variables) {
  Data[[var]] = as.factor(Data[[var]])
}

Data[[target_variable]] = as.factor(Data[[target_variable]])

str(Data)
## EDA

## CHURN VS STATE
### CHI SQUARED TEST

# Create a contingency table
contingency_table <- table(Data$State, Data$Churn)

# Perform chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Print the results
print(chi_squared_test)

## X-squared = 83.044, df = 50, p-value = 0.002296. Since the p-value is lower than 0.05, we 
## can reject the null hypothesis and hence there is a significant association between the two variables.

library(ggplot2)

states = unique(Data$State)


for (state in states) {
  churn_count = table(Data[Data$State == state, "Churn"])
  proportion_churn <- churn_count["True"] / sum(churn_count)
  cat(state, ":", churn_count, "   Proportion Churn: ", proportion_churn, "\n")
}



