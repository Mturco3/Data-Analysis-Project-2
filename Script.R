# Importing libraries

library(ggplot2)
library(dplyr)
library(skimr)
library(readr)
library(sf)
library(usmap)
library(grid)
library(gridExtra)
library(corrplot)
library(caret)
library(ROSE)


# Importing Dataset
Data = read.csv2("./Dataset/TelecomChurn.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")
variables = colnames(Data)
categorical_variables = c("State", "International.plan", "Voice.mail.plan", "Area.code")
target_variable = "Churn"
numerical_variables = setdiff(variables, c(categorical_variables, target_variable))

# Ensuring that variables are converted in the correct form
Data[[target_variable]] = as.factor(Data[[target_variable]])
for (var in numerical_variables) {
  Data[[var]] = as.numeric(Data[[var]])
}
for (var in categorical_variables) {
  Data[[var]] = as.factor(Data[[var]])
}

rm(var) # Dropping the variable used in the for loop

# Showing the results
str(Data)

# EDA

# Showing the name of the variables in the dataset
print(variables)

# Counting number of distinct features for each variable before a more accurate EDA
for (col in variables) {
  print(paste("Number of distinct values in",col, "is", length(unique(Data[[col]]))))
  cat("\n")
}
rm(col) # Dropping the variable used in the for loop

# Data summary with Skim library
skim(Data)


## TARGET VARIABLE DISTRIBUTION 

# Computing churn count and proportion
count_df = Data %>%
  count(Churn, name = "Count")
count_df$proportion = count_df$Count/sum(count_df$Count)


histogram_plot = ggplot(Data, aes(x = Churn, fill = Churn)) + 
  geom_bar(color = "black", alpha = 0.7) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust= 2) + 
  ggtitle("Churn Count") + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
  )

# Creating pie histogram plot with ggplot library
piechart_plot = ggplot(data = count_df, aes(x = "", y = proportion, fill = Churn)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.7) + 
  coord_polar("y") +
  theme_minimal() + 
  geom_text(aes(label = paste0(round(proportion * 100, 0), "%")), 
            position = position_stack(vjust = 0.5)) + 
  labs(x = NULL, y = NULL, fill = NULL, 
       title = paste("Churn Proportion")) + 
  scale_fill_manual(values = c("red", "blue", "green")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid = element_blank())

# Representing the plots

combined_plot = grid.arrange(
  arrangeGrob(histogram_plot, piechart_plot, nrow = 1, ncol = 2),
  top = textGrob("Target Variable Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)


## CATEGORICAL VARIABLES

# We are going to analyze both the distribution of the variables themselves
# and also the histogram considering the relationship with churn

plot_list = list()
plot_list_relationship = list()

# Loop through the categorical variables to create individual plots
for (variable in categorical_variables) {
  if (variable == "State") {
    next
  }
  plot_relationship = ggplot(Data, aes_string(x = variable, fill = "Churn")) + 
    geom_bar(position = "dodge", color = "black", alpha = 0.7) + 
    scale_fill_manual(values = c("blue", "red")) + 
    xlab(variable) + 
    ylab("Count") + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  
  count_df = Data %>%
    count(!!sym(variable), name = "Count")
  count_df$proportion = count_df$Count/sum(count_df$Count)
 
  
  plot = ggplot(data = count_df, aes(x = "", y = proportion, fill = !!sym(variable))) + 
    geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.7) + 
    theme_classic() + 
    coord_polar("y") +
    geom_text(aes(label = paste0(round(proportion * 100, 0), "%")), 
              position = position_stack(vjust = 0.5)) + 
    labs(x = NULL, y = NULL, fill = NULL, 
         title = paste("Distribution of", variable)) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    scale_fill_manual(values = c("red", "blue", "green")) + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  
  
  # Add the plot to the list
  plot_list_relationship[[variable]] = plot_relationship
  plot_list[[variable]] = plot
}

# Arrange the plots in a grid layout
combined_plot = do.call(grid.arrange, c(plot_list, nrow = 2, ncol = 2))
combined_plot_relationship = do.call(grid.arrange, c(plot_list_relationship, nrow = 2, ncol = 2))


## CHURN VS STATE

### PLOTTING CHURN RATE ACROSS STATES

Data$ChurnNumeric = ifelse(Data$Churn == "True", 1, 0)

churn_rate = Data %>%
  group_by(State) %>%
  summarize(ChurnRate = mean(ChurnNumeric))

states = statepop
names(states)[names(states) == "abbr"] <- "State"
churn_rate_states <- merge(states, churn_rate, by = "State", all.x = TRUE)

plot_usmap(data = churn_rate_states, values = "ChurnRate", labels = TRUE) +
  scale_fill_gradient(low = "lightblue",
                      high = "red",
                      name = NULL) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(0.8, "in"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Churn Rate Across US States")

# Dropping variables since it is now useless
Data$ChurnNumeric = NULL

### CHI SQUARED TEST

# Create a contingency table and perform the chi-squared test. Then we report the results
contingency_table = table(Data$State, Data$Churn)
chi_squared_test = chisq.test(contingency_table)
print(chi_squared_test)

## X-squared = 83.044, df = 50, p-value = 0.002296. Since the p-value is lower than 0.05, we 
## can reject the null hypothesis and hence there is a significant association between the two variables.


## NUMERICAL VARIABLES

Numerical_Data = Data[numerical_variables]
means_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = mean)
median_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = median)
sd_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = sd)

par(mfrow = c(3,3), mar = c(2,4,4,1))

for(i in 1:length(numerical_variables)){
  hist(Numerical_Data[,i], freq = F, main = names(Numerical_Data)[i],
       col = rgb(.7,.7,.7), border = "white", xlab = "")
  abline(v = means_vec[i], lwd = 2)
  abline(v = median_vec[i], lwd = 2, col = rgb(.7,0,0))
  legend("top", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7,0,0)),cex = .8, bty = "n")
}



## CHURN AND TOTAL DAY MINUTES 

ggplot(Data, aes(x = Churn, y = `Total.day.minutes`, fill = Churn)) + 
  geom_boxplot() + 
  ggtitle("Total Day Minutes by Churn Status") + 
  xlab("Churn") + 
  ylab("Total Day Minutes") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
  )

## CORRELATION ANALYSIS

### COLLINEARITY

cor_matrix <- cor(Data[numerical_variables], use="complete.obs")
corrplot(cor_matrix, method = "color", tl.srt = 45, tl.col = "black",
         addCoef.col = "black", 
         number.cex = 0.7,
         addgrid.col = "grey",
         tl.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200))

cor_matrix[!lower.tri(cor_matrix)] = 0


# Find the pairs with correlation greater than the threshold
threshold <- 0.8
high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)

# Create a data frame with the results
high_corr_df <- data.frame(
  Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

variables_to_drop = high_corr_df$Variable1

for (variable in variables_to_drop) {
  Data[[variable]] = NULL
  numerical_variables = setdiff(numerical_variables, variable)
}


### CORRELATION WITH TARGET VARIABLE

target_variable_numeric = as.numeric(Data$Churn)

correlations <- sapply(Data[numerical_variables], function(x) cor(x, target_variable_numeric, use = "complete.obs"))

# Omit the target variable from the plot if it's included in the Data frame
correlations <- abs(correlations[names(correlations) != "target_var"])

# Create the bar plot with larger font size for names, then restore default sizes
par(mar = c(5, 8, 4, 2) + 0.1)  
barplot(correlations, 
        main="Correlation with Target Variable",
        horiz=TRUE, 
        cex.names=0.7, 
        las=2, 
        col = "deepskyblue")
par(mar = c(5, 4, 4, 2) + 0.1) 


## PREPROCESSING

### DEALING WITH DUPLICATES

# Counting duplicates row and printing the result
duplicates = sum(duplicated(Data))
print(paste("There are", duplicates, "duplicates rows in the Dataset"))

# No need to deal with duplicates

### DEALING WITH MISSING VALUES

#Renaming missing values with NA notation, and counting how many rows contain missing values, then printing result
Data[Data == 'Unknown'] <- NA
na_counts_per_row <- rowSums(is.na(Data))
rows_with_na <- sum(na_counts_per_row > 0)
cat("Rows with NA before preprocessing:", rows_with_na, "\n")

# No need to deal with missing values

### DEALING WITH COLLINEARITY

# We have seen in our EDA that there are more features with a really high correlation
# and hence we have to drop them.

cor_matrix[!lower.tri(cor_matrix)] = 0

# Find the pairs with correlation greater than the threshold
threshold <- 0.8
high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)

# Create a data frame with the results
high_corr_df <- data.frame(
  Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

variables_to_drop = high_corr_df$Variable1

for (variable in variables_to_drop) {
  Data[[variable]] = NULL
  numerical_variables = setdiff(numerical_variables, variable)
}

## DATASET SPLIT

### NORMAL SPLIT

id_train <- sample(1:nrow(Data), size = 0.75*nrow(Data), replace = F)
train_data <- Data[id_train,]
test_data <- Data[-id_train,]

# Response variable distribution in the original data
cat("Distribution of the target variable in the original set:\n")
cat("Counts:")
print(table(Data$Churn))
cat("Proportions:")
print(prop.table(table(Data$Churn)))
cat("\n")

# Response variable distribution in the train test
cat("Distribution of the target variable in the train set:\n")
cat("Counts:")
print(table(train_data$Churn))
cat("Proportions:")
print(prop.table(table(train_data$Churn)))
cat("\n")

# Response variable distribution in the test set
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(val_data$Churn))
cat("Proportions:")
print(prop.table(table(val_data$Churn)))

### UNDERSAMPLING

set.seed(1)

majority_indices <- which(Data$Churn == "False")
minority_count <- sum(Data$Churn == "True")

sampled_indices <- sample(majority_indices, size = minority_count, replace = FALSE)
train_data_undersample <- Data[c(sampled_indices, which(Data$Churn == "True")), ]

# Response variable distribution in the validation set
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(train_data_undersample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_undersample$Churn)))

### OVERSAMPLING

class_counts <- table(train_data$Churn)
max_class_count <- max(class_counts)

# Calculate the total number of samples needed for balanced oversampling
# We want each class to have max_class_count samples
target_N <- 2 * max_class_count

# Perform oversampling
train_data_oversample <- ovun.sample(Churn ~ ., data = train_data, method = "over", N = target_N)$data
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(train_data_oversample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_oversample$Churn)))

### SCALING

cols_to_scale = numerical_variables

# Normal data
train_mean <- apply(train_data[, cols_to_scale], MARGIN = 2, FUN = mean)
train_sd <- apply(train_data[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_scaled = train_data
train_data_scaled[, cols_to_scale] = scale(train_data[, cols_to_scale], 
                                                      center = train_mean, 
                                                      scale = train_sd)

# Oversample data
train_oversample_mean = apply(train_data_oversample[, cols_to_scale], MARGIN = 2, FUN = mean)
train_overssample_sd = apply(train_data_oversample[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_oversample_scaled = train_data_oversample
train_data_oversample_scaled[, cols_to_scale] = scale(train_data_oversample[, cols_to_scale], 
                                                      center = train_oversample_mean, 
                                                      scale = train_overssample_sd)

# Undersample data
train_undersample_mean = apply(train_data_undersample[, cols_to_scale], MARGIN = 2, FUN = mean)
train_undersample_sd = apply(train_data_undersample[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_undersample_scaled = train_data_undersample
train_data_undersample_scaled[, cols_to_scale] = scale(train_data_undersample[, cols_to_scale], 
                                                      center = train_undersample_mean, 
                                                      scale = train_undersample_sd)


# Scale validation data using training data's parameters
test_data_scaled = test_data
test_data_scaled[, cols_to_scale] = scale(test_data[, cols_to_scale], center = train_mean, scale = train_sd)

test_data_scaled_oversample = test_data
test_data_scaled_oversample[, cols_to_scale] = scale(test_data_scaled_oversample[, cols_to_scale], 
                                                     center = train_oversample_mean, 
                                                     scale = train_overssample_sd)

test_data_scaled_undersample = test_data
test_data_scaled_undersample[, cols_to_scale] = scale(test_data_undersample[, cols_to_scale], 
                                                      center = train_undersample_mean, 
                                                      scale = train_undersample_sd)

## LOW DIMENSIONAL MODEL

#### In the EDA we have seen that International Plan seems to have an impact on the Churn Rate
#### We consider also a simple model trained only with total day minutes since, as long as customer
# service calls was the one with the highest correlation with the target variable

### LOGISTIC REGRESSION MODEL WITH INTERNATIONAL PLAN AND TOTAL DAY MINUTES

#### Normal Data

set.seed(123)
simple_model <- glm(Churn ~ Total.day.minutes * International.plan, 
                           family = binomial(link = "logit"), 
                           data = Data)

# View model summary
summary(simple_model)

# Computing predictive probabilities
predicted_probabilities_logistic <- predict(simple_model, newdata = Data, type="response")
Data$predicted_probabilities <- predict(simple_model, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line() + 
  labs(title = "Probability of Churn by Total Day Minutes and International Plan", 
       y = "Probability of Churn", x = "Total Day Minutes") +
  scale_color_manual(values = c("red", "blue"))

# We drop the column of predicted probabilities since it is now useless
Data$predicted_probabilities = NULL

#### Undersample data

set.seed(123)
simple_model <- glm(Churn ~ Total.day.minutes * International.plan, 
                    family = binomial(link = "logit"), 
                    data = train_data_undersample)

# View model summary
summary(simple_model)

# Computing predictive probabilities
predicted_probabilities_logistic <- predict(simple_model, newdata = Data, type="response")
Data$predicted_probabilities <- predict(simple_model, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line() + 
  labs(title = "Probability of Churn by Total Day Minutes and International Plan", 
       y = "Probability of Churn", x = "Total Day Minutes") +
  scale_color_manual(values = c("red", "blue"))

# We drop the column of predicted probabilities since it is now useless
train_data_undersample$predicted_probabilities = NULL



simple_model <- glm(Churn ~ Total.day.minutes * International.plan, 
                    family = binomial(link = "logit"), 
                    data = undersample_Data)

predicted_probabilities_logistic <- predict(simple_model, newdata = Data, type="response")
Data$predicted_probabilities <- predict(simple_model, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("red", "blue"))


### LOGISTIC REGRESSION MODEL WITH CUSTOMER SERVICE CALLS

set.seed(1)
simple_model_cutomer_calls <- glm(Churn ~ Customer.service.calls, 
                    family = binomial(link = "logit"), 
                    data = Data)
#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("red", "blue"))

# We drop the column of predicted probabilities since it is now useless
Data$predicted_probabilities = NULL

# View model summary
summary(simple_model_cutomer_calls)

predicted_probabilities_logistic <- predict(simple_model_cutomer_calls, newdata = Data, type="response")

Data$predicted_probabilities <- predict(simple_model_cutomer_calls, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Customer.service.calls, y = predicted_probabilities)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income")

Data$predicted_probabilities = NULL # We drop the column of predicted probabilities since it is now useless
