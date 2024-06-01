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
library(latex2exp)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(doParallel)

# Importing Dataset
Data = read.csv2("./Dataset/TelecomChurn.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")
variables = colnames(Data)
categorical_variables = c("State", "International.plan", "Voice.mail.plan", "Area.code")
target_variable = "Churn"
numerical_variables = setdiff(variables, c(categorical_variables, target_variable))
predictors = setdiff(variables, target_variable)

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
churn_distribution = Data %>%
  count(Churn, name = "Count")
churn_distribution$proportion = churn_distribution$Count/sum(churn_distribution$Count)

# Creating histogram with ggplot library
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
piechart_plot = ggplot(data = churn_distribution, aes(x = "", y = proportion, fill = Churn)) + 
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

grid.arrange(
  arrangeGrob(histogram_plot, piechart_plot, nrow = 1, ncol = 2),
  top = textGrob("Target Variable Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)

# Removing useless elements
rm(histogram_plot)
rm(piechart_plot)

## CATEGORICAL VARIABLES

# We are going to analyze both the distribution of the variables themselves
# and also the histogram considering the relationship with churn

# Initialize vectors to store plots
plot_list = list()
plot_list_relationship = list()

# Loop through the categorical variables to create individual plots
for (variable in categorical_variables) {
  if (variable == "State") {
    next
  }
  
  # Creating histograms
  plot_relationship = ggplot(Data, aes_string(x = variable, fill = "Churn")) + 
    geom_bar(position = "dodge", color = "black", alpha = 0.7) + 
    scale_fill_manual(values = c("blue", "red")) + 
    geom_text(stat = 'count', aes(label = after_stat(count)), position = position_dodge(width = 1), vjust = -0.4) +
    xlab(variable) +
    ylim(0, 2700) +
    ylab("Count") + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "none"
    )
  
  # Storing the legend separately for histograms (we want to represent only one in the final plot)
  get_legend <- function(myplot) {
    tmp <- ggplot_gtable(ggplot_build(myplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  legend = get_legend(ggplot(Data, aes_string(x = categorical_variables[1], fill = "Churn")) + 
                         geom_bar(position = "dodge", color = "black", alpha = 0.7) + 
                         scale_fill_manual(values = c("blue", "red")) + 
                         theme_minimal() + 
                         theme(legend.title = element_text(size = 14),
                               legend.text = element_text(size = 12)))
  
  # Computing proportions for categorical variables
  count_df = Data %>%
    count(!!sym(variable), name = "Count")
  count_df$proportion = count_df$Count/sum(count_df$Count)
 
  # Creating pie charts
  plot = ggplot(data = count_df, aes(x = "", y = proportion, fill = !!sym(variable))) + 
    geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.7) + 
    theme_classic() + 
    coord_polar("y") +
    geom_text(aes(label = paste0(round(proportion * 100, 0), "%")), 
              position = position_stack(vjust = 0.7)) + 
    labs(x = NULL, y = NULL, fill = NULL, 
         title = paste("Distribution of", variable)) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    scale_fill_manual(values = c("red", "blue", "green")) + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
  

  # Adding the plots to the list
  plot_list_relationship[[variable]] = plot_relationship
  plot_list[[variable]] = plot
}

# Displaying plots
grid.arrange(
  arrangeGrob(grobs = plot_list, nrow = 2, ncol = 2),
  top = textGrob("Categorical Variables Distribution", 
                 gp = gpar(fontsize = 20, fontface = "bold"),
                 just = "center")
)
grid.arrange(
  arrangeGrob(grobs = plot_list_relationship, nrow = 1, ncol = 3),
  top = textGrob("Churn Distribution Across Categorical Variables", 
                 gp = gpar(fontsize = 20, fontface = "bold"),
                 just = "center"),
  right = legend,
  left = "Count"
)

# Removing useless variables
rm(legend)
rm(plot_list)
rm(plot_list_relationship)
rm(plot)
rm(plot_relationship)
rm(variable)
rm(count_df)

### CHI SQUARED TEST INTERNATIONAL PLAN

# Create a contingency table and perform the chi-squared test. Then we report the results
contingency_table = table(Data$State, Data$Churn)
chi_squared_test = chisq.test(contingency_table)
print(chi_squared_test)

# Removing useless data and variables
rm(chi_squared_test)
rm(contingency_table)

## CHURN VS STATE

### PLOTTING CHURN RATE ACROSS STATES

Data$ChurnNumeric = ifelse(Data$Churn == "True", 1, 0)

churn_rate_states = Data %>%
  group_by(State) %>%
  summarize(ChurnRate = mean(ChurnNumeric))

states = statepop
names(states)[names(states) == "abbr"] <- "State"
churn_rate_states = merge(states, churn_rate_states, by = "State", all.x = TRUE)

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

# Removing useless Data
rm(states)
rm(churn_rate_states)

### CHI SQUARED TEST STATE

# Create a contingency table and perform the chi-squared test. Then we report the results
contingency_table = table(Data$State, Data$Churn)
chi_squared_test = chisq.test(contingency_table)
print(chi_squared_test)

# Removing useless data and variables
rm(chi_squared_test)
rm(contingency_table)

## X-squared = 83.044, df = 50, p-value = 0.002296. Since the p-value is lower than 0.05, we 
## can reject the null hypothesis and hence there is a significant association between the two variables.


## NUMERICAL VARIABLES

Numerical_Data = Data[numerical_variables]
means_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = mean)
median_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = median)
sd_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = sd)

plot_list_numerical_relationship = list()
plot_list_numerical = list()

# Loop through each numerical variable to create individual histograms
for (variable in numerical_variables) {
  # Setting appropriate bin width
  binwidth = ceiling(max(Data[[variable]], na.rm = TRUE)/10)
  
  # Creating histograms
  plot_numerical = ggplot(Data, aes_string(x = variable)) + 
    geom_histogram(binwidth = binwidth, color = "black", fill = "grey", alpha = 0.7) + 
    geom_vline(aes_string(xintercept = means_vec[variable]), color = "blue", linetype = "dashed", size = 1) + 
    geom_vline(aes_string(xintercept = median_vec[variable]), color = "red", linetype = "dashed", size = 1) + 
    labs(title = paste("Histogram of", variable), x = variable, y = "Frequency") +
    theme_minimal() + 
    theme(
      plot.title = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  # Creating 
  plot_numerical_relationship = ggplot(Data, aes_string(x = variable, fill = "Churn")) +
    geom_histogram(position = "identity", alpha = 0.7, binwidth = binwidth) +
    labs(title = variable) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    ) +
    scale_fill_manual(name = "Churn", labels = c("No", "Yes"), values = c("blue", "red"))
  
  # Storing legend
  legend = get_legend(ggplot(Data, aes_string(x = variable, fill = "Churn")) +
                        geom_histogram(position = "identity", alpha = 0.7, binwidth = binwidth) +
                        labs(title = variable) +
                        theme_minimal() +
                        theme(legend.title = element_text(size = 14),
                              legend.text = element_text(size = 12),
                              legend.direction = "horizontal") +
                        scale_fill_manual(name = "Churn", labels = c("No", "Yes"), values = c("blue", "red"))
                      ) 
  
  # Add the plot to the list
  plot_list_numerical[[variable]] = plot_numerical
  plot_list_numerical_relationship[[variable]] = plot_numerical_relationship
}

# Plotting the results
grid.arrange(
  arrangeGrob(grobs = plot_list_numerical, nrow = 5, ncol = 3),
  top = textGrob("Numerical Variables Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)

grid.arrange(
  arrangeGrob(grobs = plot_list_numerical_relationship, nrow = 5, ncol = 3),
  top = textGrob("Numerical Variables Distribution", gp = gpar(fontsize = 20, fontface = "bold")),
  bottom = legend,
  left = textGrob("Count", rot = 90, gp = gpar(fontsize = 14))
)

# Removing variables that are now useless
# rm(means_vec)
# rm(median_vec)
# rm(sd_vec)
rm(binwidth)
rm(plot_list_numerical)
rm(plot_numerical)
rm(plot_list_numerical_relationship)
rm(plot_numerical_relationship)
rm(variable)
rm(get_legend)
rm(legend)
rm(means_vec)
rm(median_vec)
rm(sd_vec)

# We have to better analyze the relationship between churn and total day minutes,
# since the data suggest that customers that churn have an higher total day minutes.
# Also, it seems that customers who made more service calls are more inclined to churn.

### CHURN AND TOTAL DAY MINUTES 

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

# Perform an ANOVA to test the hypothesis H0 that the two means (of the groups),
# are the same, suggesting that having more minutes is not a valid indicator for Churn.

anova_total_minutes = aov(Total.day.minutes ~ Churn, data = Data)
summary(anova_total_minutes)

# Dropping variables
rm(anova_total_minutes)

### CHURN AND CUSTOMER SERVICE CALLS

ggplot(Data, aes_string(x = "Customer.service.calls", fill = "Churn")) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 0.5) +
  ggtitle("Customer Service Calls by Churn Status") + 
  xlab("Service Calls") + 
  ylab("Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = 0:9) +
  scale_fill_manual(name = "Churn", labels = c("No", "Yes"), values = c("blue", "red"))

# Perform an ANOVA to test the hypothesis H0 that the two means (of the groups),
# are the same, suggesting that having more minutes is not a valid indicator for Churn.

anova_service_calls = aov(Customer.service.calls ~ Churn, data = Data)
summary(anova_service_calls)

# Dropping variables
rm(anova_service_calls)


## CORRELATION ANALYSIS

### COLLINEARITY

cor_matrix = cor(Data[numerical_variables], use="complete.obs")
corrplot(cor_matrix, method = "color", tl.srt = 45, tl.col = "black",
         addCoef.col = "black", 
         number.cex = 0.7,
         addgrid.col = "grey",
         tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))


### CORRELATION WITH TARGET VARIABLE

# Compute and store correlations
target_variable_numeric = as.numeric(Data$Churn)
correlations = sapply(Data[numerical_variables], function(x) cor(x, target_variable_numeric, use = "complete.obs"))

# Omit the target variable from the plot if it's included in the Data frame
correlations = abs(correlations[names(correlations) != "target_var"])

# Create the bar plot with larger font size for names, then restore default sizes
par(mar = c(5, 8, 4, 2) + 0.1)  
barplot(correlations, 
        main="Correlation with Target Variable",
        horiz=TRUE, 
        cex.names=0.7, 
        las=2, 
        col = "deepskyblue")
par(mar = c(5, 4, 4, 2) + 0.1) 

# Dropping variables 
rm(correlations)
rm(target_variable_numeric)


## PREPROCESSING

### DEALING WITH DUPLICATES

# Counting duplicates row and printing the result
duplicates = sum(duplicated(Data))
print(paste("There are", duplicates, "duplicates rows in the Dataset"))

# No need to deal with duplicates

# Removing variables
rm(duplicates)

### DEALING WITH MISSING VALUES

#Renaming missing values with NA notation, and counting how many rows contain missing values, then printing result
na_counts_per_row = rowSums(is.na(Data))
rows_with_na = sum(na_counts_per_row > 0)
cat("Rows with NA before preprocessing:", rows_with_na, "\n")

# No need to deal with missing values

# Removing variables
rm(rows_with_na)
rm(na_counts_per_row)

### DEALING WITH COLLINEARITY

# We have seen in our EDA that there are more features with a really high correlation
# and hence we have to drop them.

cor_matrix[!lower.tri(cor_matrix)] = 0

# Find the pairs with correlation greater than the threshold
threshold = 0.8
high_corr_pairs = which(abs(cor_matrix) > threshold, arr.ind = TRUE)

# Create a data frame with the results
high_corr_df = data.frame(
  Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

variables_to_drop = high_corr_df$Variable1

for (variable in variables_to_drop) {
  Data[[variable]] = NULL
  cat("Dropped", variable)
  cat("\n")
  numerical_variables = setdiff(numerical_variables, variable)
  variables = setdiff(variables, variable)
  predictors = setdiff(predictors, variable)
}

# Dropping useless variable
rm(variable)
rm(variables_to_drop)
rm(high_corr_df)
rm(high_corr_pairs)
rm(threshold)
rm(cor_matrix)

### DEALING WITH CATEGORICAL VARIABLE WITH TOO MANY LEVELS

# Create a list to store regions and their states
dic = list()
dic$Northeast = c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
dic$Midwest = c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI")
dic$South = c("AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV")
dic$West = c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")


# Function to find the region for a state
find_region <- function(state) {
  for (region in names(dic)) {
    if (state %in% dic[[region]]) {
      return(region)
    }
  }
  return(NA)
}

# Apply the function to each state in the data frame to add the region column
Data$Region = sapply(Data$State, find_region)

# Converting region in a factor and dropping (but storing) the State variable
Data$Region = as.factor(Data$Region)
states = Data$State
Data$State = NULL
variables = setdiff(variables, "State")
categorical_variables = setdiff(categorical_variables, "State")
predictors = setdiff(predictors, "State")
variables = c(variables, "Region")
categorical_variables = c(categorical_variables, "Region")
predictors = c(predictors, "Region")

# Dropping variables
rm(dic)
rm(find_region)


## DATASET SPLIT

### NORMAL SPLIT

set.seed(1)

id_train = sample(1:nrow(Data), size = 0.75*nrow(Data), replace = F)
train_data = Data[id_train,]
test_data = Data[-id_train,]

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
print(table(test_data$Churn))
cat("Proportions:")
print(prop.table(table(test_data$Churn)))

### UNDERSAMPLING

set.seed(1)


# Calculate the total number of samples needed for balanced under sampling
# We want each class to have min_class_count samples
target_N = 2 * min(table(train_data$Churn))

# Perform under sampling
train_data_undersample <- ovun.sample(Churn ~ ., data = train_data, method = "under", N = target_N)$data

# Response variable distribution in the train set with under sampling
cat("Distribution of the target variable in the train set with undersampling:\n")
cat("Counts:")
print(table(train_data_undersample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_undersample$Churn)))

### OVERSAMPLING

set.seed(1)

# Calculate the total number of samples needed for balanced oversampling
# We want each class to have max_class_count samples
target_N <- 2 * max(table(train_data$Churn))

# Perform oversampling
train_data_oversample <- ovun.sample(Churn ~ ., data = train_data, method = "over", N = target_N)$data

# Response variable distribution in the train set with over sampling
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(train_data_oversample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_oversample$Churn)))

# Dropping useless variables
rm(target_N)

### SCALING

cols_to_scale = numerical_variables

# Normal data
train_mean = apply(train_data[, cols_to_scale], MARGIN = 2, FUN = mean)
train_sd = apply(train_data[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_scaled = train_data
train_data_scaled[, cols_to_scale] = scale(train_data[, cols_to_scale], 
                                                      center = train_mean, 
                                                      scale = train_sd)

# Oversample data
train_oversample_mean = apply(train_data_oversample[, cols_to_scale], MARGIN = 2, FUN = mean)
train_overssample_sd = apply(train_data_oversample[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_scaled_oversample = train_data_oversample
train_data_scaled_oversample[, cols_to_scale] = scale(train_data_oversample[, cols_to_scale], 
                                                      center = train_oversample_mean, 
                                                      scale = train_overssample_sd)

# Undersample data
train_undersample_mean = apply(train_data_undersample[, cols_to_scale], MARGIN = 2, FUN = mean)
train_undersample_sd = apply(train_data_undersample[, cols_to_scale], MARGIN = 2, FUN = sd)
train_data_scaled_undersample = train_data_undersample
train_data_scaled_undersample[, cols_to_scale] = scale(train_data_undersample[, cols_to_scale], 
                                                      center = train_undersample_mean, 
                                                      scale = train_undersample_sd)


# Scale validation data using training data's parameters
test_data_scaled = test_data
test_data_scaled[, cols_to_scale] = scale(test_data_scaled[, cols_to_scale], 
                                          center = train_mean, 
                                          scale = train_sd)

test_data_scaled_oversample = test_data
test_data_scaled_oversample[, cols_to_scale] = scale(test_data_scaled_oversample[, cols_to_scale], 
                                                     center = train_oversample_mean, 
                                                     scale = train_overssample_sd)

test_data_scaled_undersample = test_data
test_data_scaled_undersample[, cols_to_scale] = scale(test_data_scaled_undersample[, cols_to_scale], 
                                                      center = train_undersample_mean, 
                                                      scale = train_undersample_sd)
# Dropping useless variables
rm(cols_to_scale)
rm(train_mean)
rm(train_oversample_mean)
rm(train_undersample_mean)
rm(train_sd)
rm(train_overssample_sd)
rm(train_undersample_sd)
rm(id_train)

## LOW DIMENSIONAL MODEL

#### In the EDA we have seen that International Plan seems to have an impact on the Churn Rate
#### We consider also a simple model trained only with total day minutes since, as long as customer
# service calls was the one with the highest correlation with the target variable

### LOGISTIC REGRESSION MODEL WITH INTERNATIONAL PLAN AND TOTAL DAY MINUTES

#### Normal Data

set.seed(1)
model_international_minutes = glm(Churn ~ Total.day.minutes * International.plan, 
                           family = binomial(link = "logit"), 
                           data = test_data)
# View model summary
summary(model_international_minutes)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_international_minutes, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line(size = 1.2) + 
  labs(title = "Probability of Churn by Total Day Minutes and International Plan (Unbalanced training set)", 
       y = "Probability of Churn", x = "Total Day Minutes") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25),  
    panel.background = element_rect(fill = "white") 
  )

# We drop the column of predicted probabilities since it is now useless
Data$predicted_probabilities = NULL


#### Under sample data

set.seed(1)
model_international_minutes_undersample = glm(Churn ~ Total.day.minutes * International.plan, 
                    family = binomial(link = "logit"), 
                    data = train_data_undersample)

# View model summary
summary(model_international_minutes_undersample)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_international_minutes_undersample, 
                                        newdata = Data, 
                                        type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line(size = 1.2) + 
  labs(title = "Probability of Churn by Total Day Minutes and International Plan (Balanced training set 1)", 
       y = "Probability of Churn", x = "Total Day Minutes") +
  scale_color_manual(values = c("red", "blue")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
    panel.background = element_rect(fill = "white") 
  )

# We drop the column of predicted probabilities since it is now useless
train_data_undersample$predicted_probabilities = NULL


#### Over sample data

set.seed(1)
model_international_minutes_oversample = glm(Churn ~ Total.day.minutes * International.plan, 
                                              family = binomial(link = "logit"), 
                                              data = train_data_oversample)

# View model summary
summary(model_international_minutes_oversample)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_international_minutes_oversample, 
                                        newdata = Data, 
                                        type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line(size = 1.2) + 
  labs(title = "Probability of Churn by Total Day Minutes and International Plan (Balanced training set 2)", 
       y = "Probability of Churn", x = "Total Day Minutes") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
    panel.background = element_rect(fill = "white") 
  )

# We drop the column of predicted probabilities since it is now useless
train_data_undersample$predicted_probabilities = NULL

# Dropping the models that are not going to be used again
rm(model_international_minutes)
rm(model_international_minutes_oversample)
rm(model_international_minutes_undersample)

### LOGISTIC REGRESSION MODEL WITH CUSTOMER SERVICE CALLS

#### Normal Data

set.seed(1)

# Building model
model_customer_service_calls = glm(Churn ~ Customer.service.calls, 
                                   family = binomial(link = "logit"), 
                                   data = test_data)
# View model summary
summary(model_customer_service_calls)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_customer_service_calls, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Customer.service.calls, y = predicted_probabilities)) + 
  geom_line(size = 1.2, color = "blue") + 
  labs(title = "Probability of Churn by Customer Service Calls (Unbalanced training set)", 
       y = "Probability of Churn", x = "Customer Service Calls") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
    panel.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

  
# We drop the column of predicted probabilities since it is now useless
Data$predicted_probabilities = NULL


#### Under sample data

set.seed(1)
model_customer_service_calls_undersample = glm(Churn ~ Customer.service.calls, 
                                              family = binomial(link = "logit"), 
                                              data = train_data_undersample)

# View model summary
summary(model_customer_service_calls_undersample)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_customer_service_calls_undersample, 
                                        newdata = Data, 
                                        type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Customer.service.calls, y = predicted_probabilities)) + 
  geom_line(size = 1.2, color = "blue") + 
  labs(title = "Probability of Churn by Customer Service Calls (Balanced training set 1)", 
       y = "Probability of Churn", x = "Customer Service Calls") +
  theme_minimal() +
  ylim(0,1) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
    panel.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# We drop the column of predicted probabilities since it is now useless
train_data_undersample$predicted_probabilities = NULL


#### Over sample data

set.seed(1)
model_customer_service_calls_oversample = glm(Churn ~ Customer.service.calls, 
                                             family = binomial(link = "logit"), 
                                             data = train_data_oversample)

# View model summary
summary(model_customer_service_calls_oversample)

# Computing and storing predictive probabilities
Data$predicted_probabilities = predict(model_customer_service_calls_oversample, 
                                        newdata = Data, 
                                        type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Customer.service.calls, y = predicted_probabilities)) + 
  geom_line(size = 1.2, color = "blue") + 
  labs(title = "Probability of Churn by Customer Service Calls (Balanced training set 2)", 
       y = "Probability of Churn", x = "Customer Service Calls") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black"),  
    axis.text.y = element_text(size = 12, color = "black"),  
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
    panel.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# We drop the column of predicted probabilities since it is now useless
train_data_undersample$predicted_probabilities = NULL

# Dropping useless models

rm(model_customer_service_calls)
rm(model_customer_service_calls_oversample)
rm(model_customer_service_calls_undersample)

### CLASSIFICATION TREE

#### Normal Data

simple_tree = rpart(Churn ~ Total.day.minutes + International.plan + Customer.service.calls, 
              data = train_data, 
              method = "class")

# Plotting tree
rpart.plot(simple_tree, main = "Classification Tree - Normal Data")

# Print the complexity parameter table
printcp(simple_tree)

# Calculate and print variable importance
importance_normal = varImp(simple_tree, scale = FALSE)
print(importance_normal)

# Compute and print the confusion matrix
predictions_tree = predict(simple_tree, train_data, type = "class")
confusion_matrix_tree = table(predictions_tree, train_data$Churn)
print(confusion_matrix_tree)

# Dropping useless variables
rm(simple_tree)
rm(importance_normal)
rm(predictions_tree)
rm(confusion_matrix_tree)

#### Undersample Data

simple_tree_undersample = rpart(Churn ~ Total.day.minutes + International.plan + Customer.service.calls, 
                                data = train_data_undersample, 
                                method = "class")

# Plotting tree
rpart.plot(simple_tree_undersample, main = "Classification Tree - Undersample Data")

# Print the complexity parameter table
printcp(simple_tree_undersample)

# Calculate and print variable importance
importance_normal_undersample = varImp(simple_tree_undersample, scale = FALSE)
print(importance_normal_undersample)

# Compute and print the confusion matrix
predictions_tree_undersample = predict(simple_tree_undersample, train_data_undersample, type = "class")
confusion_matrix_tree_undersample = table(predictions_tree_undersample, train_data_undersample$Churn)
print(confusion_matrix_tree_undersample)

# Dropping useless variables
rm(simple_tree_undersample)
rm(importance_normal_undersample)
rm(predictions_tree_undersample)
rm(confusion_matrix_tree_undersample)

#### Over sample data

simple_tree_oversample = rpart(Churn ~ Total.day.minutes + International.plan + Customer.service.calls, 
                               data = train_data_oversample, 
                               method = "class")

# Plotting tree
rpart.plot(simple_tree_oversample, main = "Classification Tree - Oversample Data")

# Print the complexity parameter table
printcp(simple_tree_oversample)

# Calculate and print variable importance
importance_oversample = varImp(simple_tree_oversample, scale = FALSE)
print(importance_oversample)

predictions_tree_oversample = predict(simple_tree_oversample, train_data_oversample, type = "class")
confusion_matrix_tree_oversample = table(predictions_tree_oversample, train_data_oversample$Churn)
print(confusion_matrix_tree_oversample)

# Dropping useless variables
rm(simple_tree_oversample)
rm(importance_oversample)
rm(predictions_tree_oversample)
rm(confusion_matrix_tree_oversample)


# BEST MODEL SELECTION

#### Function to compare performances

get.metrics<- function(conf.mat) {
  true.positives <- conf.mat[2,2]
  true.negatives <- conf.mat[1,1]
  false.positives <- conf.mat[1,2]
  false.negatives <- conf.mat[2,1]
  num.observations <- true.positives + true.negatives + false.positives + false.negatives
  
  accuracy <- (true.positives + true.negatives) / num.observations
  precision <- (true.positives) / (true.positives + false.positives)
  recall <- true.positives / (true.positives + false.negatives)
  f1 <- 2 * ((precision * recall) / (precision + recall))
  
  metrics <- data.frame(t(c(accuracy, precision, recall, f1)))
  columns <- c("Accuracy", "Precision", "Recall", "F1")
  colnames(metrics) <- columns
  
  return(metrics)
}

# Create a list to store every confusion matrix

confusion_matrices = list()

## Baseline Logistic Regression Model

#### Normal

model_baseline_logistic = glm(Churn ~ ., 
                              data = train_data_scaled, 
                              family = "binomial")
summary(model_baseline_logistic)

# Storing the results in a confusion matrix
baseline_logistic_predictions = ifelse(predict(model_baseline_logistic, 
                                               newdata = test_data_scaled) > 0.5, 1, 0)
confusion_matrix_baseline_logistic = table(baseline_logistic_predictions, 
                                           test_data_scaled$Churn)
# Dropping predictions
rm(baseline_logistic_predictions)

# Storing the c.m. in the list
confusion_matrices[["model_baseline_logistic"]] = confusion_matrix_baseline_logistic

#### Oversample

model_baseline_logistic_oversample = glm(Churn ~ ., 
                                         data = train_data_scaled_oversample, 
                                         family = "binomial")
summary(model_baseline_logistic_oversample)

# Storing the results in a confusion matrix
baseline_logistic_predictions_oversample = ifelse(predict(model_baseline_logistic_oversample, 
                                                          newdata = test_data_scaled_oversample) > 0.5, 1, 0)
confusion_matrix_baseline_logistic_oversample = table(baseline_logistic_predictions_oversample, 
                                                      test_data_scaled_oversample$Churn)
# Dropping predictions
rm(baseline_logistic_predictions_oversample)

# Storing the c.m. in the list
confusion_matrices[["model_baseline_logistic_oversample"]] = confusion_matrix_baseline_logistic_oversample

#### Undersample

model_baseline_logistic_undersample = glm(Churn ~ ., 
                                          data = train_data_scaled_undersample, 
                                          family = "binomial")

# Storing the results in a confusion matrix
summary(model_baseline_logistic_undersample)
baseline_logistic_predictions_undersample = ifelse(predict(model_baseline_logistic_undersample, 
                                                           newdata = test_data_scaled_undersample) > 0.5, 1, 0)
confusion_matrix_baseline_logistic_undersample = table(baseline_logistic_predictions_undersample, 
                                                       test_data_scaled_undersample$Churn)
# Dropping predictions
rm(baseline_logistic_predictions_undersample)

# Storing the c.m. in the list
confusion_matrices[["model_baseline_logistic_undersample"]] = confusion_matrix_baseline_logistic_undersample

#### Comparisson with the simplest model (without any variable)

# Since the model with the best performance was the one trained with oversample data we will consier
# it for the comparisson

model_0 = glm(Churn ~ 1,
                   family = "binomial",
                   data = train_data_scaled_oversample)

anova(model_0, model_baseline_logistic_oversample, test = "Chisq")

# The null model (Model 1) fits the data poorly with a deviance of 5905.6.
# The full model (Model 2), which includes all the specified predictors, 
# fits the data significantly better, with a deviance of 4437.3.
# The reduction in deviance (1468.3) is highly significant, with a p-value < 2.2e-16.
# Including the predictors in Model 2 significantly improves the fit of the model, 
# indicating that the predictors collectively provide valuable information in predicting customer 
# churn.

## Variable selection with AIC and BIC

### AIC

#### Normal

aic_model_forward = step(glm(Churn ~ 1, 
                              family = "binomial", 
                              data = test_data_scaled), 
                          scope = formula(model_baseline_logistic), 
                          direction = "forward")
aic_model_backward = step(model_baseline_logistic, direction = "backward")
aic_model_both = step(model_baseline_logistic, direction = "both")

# We can now consider the number of features selected 
# (Without considering Intercept)
num_features_aic_forward = length(coef(aic_model_forward)) - 1  
num_features_aic_backward = length(coef(aic_model_backward)) - 1
num_features_aic_both = length(coef(aic_model_both)) - 1

# We are also interested in understanding which are the excluded features

selected_variables_aic_forward = names(coef(aic_model_forward))[-1]  
selected_variables_aic_backward = names(coef(aic_model_backward))[-1]  
selected_variables_aic_both = names(coef(aic_model_both))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection", num_features_aic_forward, "features have been selected.")
cat("\n")
for (variable in selected_variables_aic_forward) {
  cat(variable, "has been selected.")
  cat("\n")
}
cat("With backward selection", num_features_aic_backward, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_backward) {
  cat(variable, "has been selected.")
  cat("\n")
}
cat("With both directions selection", num_features_aic_both, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_both) {
  cat(variable, "has been selected.")
  cat("\n")
}

# We select the simplest model, hence the one with the lowest features selected
feature_counts = c(forward = num_features_aic_forward, 
                   backward = num_features_aic_backward, 
                   both = num_features_aic_both)
selected_model_name = names(which.min(feature_counts))
if (selected_model_name == "forward") {
  model_aic_final = aic_model_forward
} else if (selected_model_name == "backward") {
  model_aic_final = final_model <- aic_model_backward
} else {
  model_aic_final = aic_model_both
}

# Computing and storing predictions on the test data
aic_model_predictions = ifelse(predict(model_aic_final, test_data_scaled) > 0.5, 1, 0)
confusion_matrix_aic = table(aic_model_predictions, test_data_scaled$Churn)
confusion_matrices[["aic"]] = confusion_matrix_aic

# Dropping useless models and variables
rm(aic_model_backward)
rm(aic_model_both)
rm(aic_model_forward)
rm(aic_model_predictions)
rm(selected_model_name)
rm(selected_variables_aic_both)
rm(selected_variables_aic_backward)
rm(selected_variables_aic_forward)
rm(num_features_aic_both)
rm(num_features_aic_backward)
rm(num_features_aic_forward)
rm(feature_counts)


#### Undersample

aic_model_forward_undersample = step(glm(Churn ~ 1, 
                              family = "binomial", 
                              data = test_data_scaled_undersample), 
                          scope = formula(model_baseline_logistic_undersample), 
                          direction = "forward")
aic_model_backward_undersample = step(model_baseline_logistic_undersample, direction = "backward")
aic_model_both_undersample = step(model_baseline_logistic_undersample, direction = "both")

# Number of features selected by each model
num_features_aic_forward_undersample = length(coef(aic_model_forward_undersample)) - 1  # Subtract 1 for the intercept
num_features_aic_backward_undersample = length(coef(aic_model_backward_undersample)) - 1
num_features_aic_both_undersample = length(coef(aic_model_both_undersample)) - 1

# We are also interested in understanding which are the excluded features
selected_variables_aic_forward_undersample = names(coef(aic_model_forward_undersample))[-1]  
selected_variables_aic_backward_undersample = names(coef(aic_model_backward_undersample))[-1]  
selected_variables_aic_both_undersample = names(coef(aic_model_both_undersample))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection", num_features_aic_forward_undersample, "features have been selected.")
cat("\n")
for (variable in selected_variables_aic_forward_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With backward selection", num_features_aic_backward_undersample, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_backward_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With both directions selection", num_features_aic_both_undersample, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_both_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

# Select the simplest model, hence the one with the lowest features selected
feature_counts_undersample = c(forward = num_features_aic_forward_undersample, 
                               backward = num_features_aic_backward_undersample, 
                               both = num_features_aic_both_undersample)
selected_model_name_undersample = names(which.min(feature_counts_undersample))

if (selected_model_name_undersample == "forward") {
  model_aic_final_undersample = aic_model_forward_undersample
} else if (selected_model_name_undersample == "backward") {
  model_aic_final_undersample = aic_model_backward_undersample
} else {
  model_aic_final_undersample = aic_model_both_undersample
}

# Computing and storing predictions on the validation data
aic_model_predictions_undersample = ifelse(predict(model_aic_final_undersample, test_data_scaled_undersample) > 0.5, 1, 0)
confusion_matrix_aic_undersample = table(aic_model_predictions_undersample, test_data_scaled_undersample$Churn)
confusion_matrices[["model_aic_undersample"]] = confusion_matrix_aic_undersample

# Dropping useless variables and models

rm(aic_model_backward_undersample)
rm(aic_model_both_undersample)
rm(aic_model_forward_undersample)
rm(aic_model_predictions_undersample)
rm(selected_model_name_undersample)
rm(selected_variables_aic_both_undersample)
rm(selected_variables_aic_backward_undersample)
rm(selected_variables_aic_forward_undersample)
rm(num_features_aic_both_undersample)
rm(num_features_aic_backward_undersample)
rm(num_features_aic_forward_undersample)
rm(feature_counts_undersample)

#### Oversample
aic_model_forward_oversample = step(glm(Churn ~ 1, 
                      family = "binomial", 
                      data = test_data_scaled_oversample), 
                  scope = formula(model_baseline_logistic_oversample), 
                  direction = "forward")
aic_model_backward_oversample = step(model_baseline_logistic_oversample, direction = "backward")
aic_model_both_oversample = step(model_baseline_logistic_oversample, direction = "both")

# Number of features selected by each model
num_features_aic_forward_oversample = length(coef(aic_model_forward_oversample)) - 1
num_features_aic_backward_oversample = length(coef(aic_model_backward_oversample)) - 1
num_features_aic_both_oversample = length(coef(aic_model_both_oversample)) - 1

# Selected variables
selected_variables_aic_forward_oversample = names(coef(aic_model_forward_oversample))[-1]
selected_variables_aic_backward_oversample = names(coef(aic_model_backward_oversample))[-1]
selected_variables_aic_both_oversample = names(coef(aic_model_both_oversample))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection", num_features_aic_forward_oversample, "features have been selected.")
cat("\n")
for (variable in selected_variables_aic_forward_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With backward selection", num_features_aic_backward_oversample, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_backward_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With both directions selection", num_features_aic_both_oversample, "features have been selected")
cat("\n")
for (variable in selected_variables_aic_both_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

# Select the simplest model, hence the one with the lowest features selected
feature_counts_oversample = c(forward = num_features_aic_forward_oversample, 
                              backward = num_features_aic_backward_oversample, 
                              both = num_features_aic_both_oversample)
selected_model_name_oversample = names(which.min(feature_counts_oversample))

if (selected_model_name_oversample == "forward") {
  model_aic_final_oversample = aic_model_forward_oversample
} else if (selected_model_name_oversample == "backward") {
  model_aic_final_oversample = aic_model_backward_oversample
} else {
  model_aic_final_oversample = aic_model_both_oversample
}

# Computing and storing predictions on the validation data
aic_model_predictions_oversample = ifelse(predict(model_aic_final_oversample, test_data_scaled_oversample, type = "response") > 0.5, 1, 0)
confusion_matrix_aic_oversample = table(aic_model_predictions_oversample, test_data_scaled_oversample$Churn)
confusion_matrices[["aic_oversample"]] = confusion_matrix_aic_oversample

rm(aic_model_backward_oversample)
rm(aic_model_both_oversample)
rm(aic_model_forward_oversample)
rm(aic_model_predictions_oversample)
rm(selected_model_name_oversample)
rm(selected_variables_aic_both_oversample)
rm(selected_variables_aic_backward_oversample)
rm(selected_variables_aic_forward_oversample)
rm(num_features_aic_both_oversample)
rm(num_features_aic_backward_oversample)
rm(num_features_aic_forward_oversample)
rm(feature_counts_oversample)

### BIC


#### Normal

bic_model_forward = step(glm(Churn ~ 1, 
                             family = "binomial", 
                             data = test_data_scaled), 
                         scope = formula(model_baseline_logistic), 
                         direction = "forward",
                         k = log(nrow(test_data_scaled)))
bic_model_backward = step(model_baseline_logistic, direction = "backward", k = log(nrow(test_data_scaled)))
bic_model_both = step(model_baseline_logistic, direction = "both", k = log(nrow(test_data_scaled)))

# Number of features selected by each model
num_features_bic_forward = length(coef(bic_model_forward)) - 1  
num_features_bic_backward = length(coef(bic_model_backward)) - 1
num_features_bic_both = length(coef(bic_model_both)) - 1

# Selected variables
selected_variables_bic_forward = names(coef(bic_model_forward))[-1]  
selected_variables_bic_backward = names(coef(bic_model_backward))[-1]  
selected_variables_bic_both = names(coef(bic_model_both))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection (BIC)", num_features_bic_forward, "features have been selected.")
cat("\n")
for (variable in selected_variables_bic_forward) {
  cat(variable, "has been selected.")
  cat("\n")
}
cat("With backward selection (BIC)", num_features_bic_backward, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_backward) {
  cat(variable, "has been selected.")
  cat("\n")
}
cat("With both directions selection (BIC)", num_features_bic_both, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_both) {
  cat(variable, "has been selected.")
  cat("\n")
}

# Select the simplest model, hence the one with the lowest features selected
feature_counts_bic = c(forward = num_features_bic_forward, 
                       backward = num_features_bic_backward, 
                       both = num_features_bic_both)
selected_model_name_bic = names(which.min(feature_counts_bic))

if (selected_model_name_bic == "forward") {
  model_bic_final = bic_model_forward
} else if (selected_model_name_bic == "backward") {
  model_bic_final = bic_model_backward
} else {
  model_bic_final = bic_model_both
}

# Computing and storing predictions on the test data
bic_model_predictions = ifelse(predict(model_bic_final, test_data_scaled, type = "response") > 0.5, 1, 0)
confusion_matrix_bic = table(bic_model_predictions, test_data_scaled$Churn)
confusion_matrices[["bic_model"]] = confusion_matrix_bic

rm(bic_model_backward)
rm(bic_model_both)
rm(bic_model_forward)
rm(bic_model_predictions)
rm(selected_model_name_bic)
rm(selected_variables_bic_both)
rm(selected_variables_bic_backward)
rm(selected_variables_bic_forward)
rm(num_features_bic_both)
rm(num_features_bic_backward)
rm(num_features_bic_forward)
rm(feature_counts_bic)

#### Undersample

bic_model_forward_undersample = step(glm(Churn ~ 1, 
                                         family = "binomial", 
                                         data = test_data_scaled_undersample), 
                                     scope = formula(model_baseline_logistic_undersample), 
                                     direction = "forward",
                                     k = log(nrow(test_data_scaled_undersample)))
bic_model_backward_undersample = step(model_baseline_logistic_undersample, direction = "backward", k = log(nrow(test_data_scaled_undersample)))
bic_model_both_undersample = step(model_baseline_logistic_undersample, direction = "both", k = log(nrow(test_data_scaled_undersample)))

# Number of features selected by each model
num_features_bic_forward_undersample = length(coef(bic_model_forward_undersample)) - 1
num_features_bic_backward_undersample = length(coef(bic_model_backward_undersample)) - 1
num_features_bic_both_undersample = length(coef(bic_model_both_undersample)) - 1

# Selected variables
selected_variables_bic_forward_undersample = names(coef(bic_model_forward_undersample))[-1]
selected_variables_bic_backward_undersample = names(coef(bic_model_backward_undersample))[-1]
selected_variables_bic_both_undersample = names(coef(bic_model_both_undersample))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection (BIC)", num_features_bic_forward_undersample, "features have been selected.")
cat("\n")
for (variable in selected_variables_bic_forward_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With backward selection (BIC)", num_features_bic_backward_undersample, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_backward_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With both directions selection (BIC)", num_features_bic_both_undersample, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_both_undersample) {
  cat(variable, "has been selected.")
  cat("\n")
}

# Select the simplest model, hence the one with the lowest features selected
feature_counts_bic_undersample = c(forward = num_features_bic_forward_undersample, 
                                   backward = num_features_bic_backward_undersample, 
                                   both = num_features_bic_both_undersample)
selected_model_name_bic_undersample = names(which.min(feature_counts_bic_undersample))

if (selected_model_name_bic_undersample == "forward") {
  model_bic_final_undersample = bic_model_forward_undersample
} else if (selected_model_name_bic_undersample == "backward") {
  model_bic_final_undersample = bic_model_backward_undersample
} else {
  model_bic_final_undersample = bic_model_both_undersample
}

# Computing and storing predictions on the test data
bic_model_predictions_undersample = ifelse(predict(model_bic_final_undersample, test_data_scaled_undersample, type = "response") > 0.5, 1, 0)
confusion_matrix_bic_undersample = table(bic_model_predictions_undersample, test_data_scaled_undersample$Churn)
confusion_matrices[["model_bic_undersample"]] = confusion_matrix_bic_undersample

rm(bic_model_backward_undersample)
rm(bic_model_both_undersample)
rm(bic_model_forward_undersample)
rm(bic_model_predictions_undersample)
rm(selected_model_name_bic_undersample)
rm(selected_variables_bic_both_undersample)
rm(selected_variables_bic_backward_undersample)
rm(selected_variables_bic_forward_undersample)
rm(num_features_bic_both_undersample)
rm(num_features_bic_backward_undersample)
rm(num_features_bic_forward_undersample)
rm(feature_counts_bic_undersample)


#### Oversample

bic_model_forward_oversample = step(glm(Churn ~ 1, 
                                        family = "binomial", 
                                        data = test_data_scaled_oversample), 
                                    scope = formula(model_baseline_logistic_oversample), 
                                    direction = "forward",
                                    k = log(nrow(test_data_scaled_oversample)))
bic_model_backward_oversample = step(model_baseline_logistic_oversample, direction = "backward", k = log(nrow(test_data_scaled_oversample)))
bic_model_both_oversample = step(model_baseline_logistic_oversample, direction = "both", k = log(nrow(test_data_scaled_oversample)))

# Number of features selected by each model
num_features_bic_forward_oversample = length(coef(bic_model_forward_oversample)) - 1
num_features_bic_backward_oversample = length(coef(bic_model_backward_oversample)) - 1
num_features_bic_both_oversample = length(coef(bic_model_both_oversample)) - 1

# Selected variables
selected_variables_bic_forward_oversample = names(coef(bic_model_forward_oversample))[-1]
selected_variables_bic_backward_oversample = names(coef(bic_model_backward_oversample))[-1]
selected_variables_bic_both_oversample = names(coef(bic_model_both_oversample))[-1]

# Print the number of features selected and the selected variables for each method
cat("With forward selection (BIC)", num_features_bic_forward_oversample, "features have been selected.")
cat("\n")
for (variable in selected_variables_bic_forward_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With backward selection (BIC)", num_features_bic_backward_oversample, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_backward_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

cat("With both directions selection (BIC)", num_features_bic_both_oversample, "features have been selected")
cat("\n")
for (variable in selected_variables_bic_both_oversample) {
  cat(variable, "has been selected.")
  cat("\n")
}

# Select the simplest model, hence the one with the lowest features selected
feature_counts_bic_oversample = c(forward = num_features_bic_forward_oversample, 
                                  backward = num_features_bic_backward_oversample, 
                                  both = num_features_bic_both_oversample)
selected_model_name_bic_oversample = names(which.min(feature_counts_bic_oversample))

if (selected_model_name_bic_oversample == "forward") {
  model_bic_final_oversample = bic_model_forward_oversample
} else if (selected_model_name_bic_oversample == "backward") {
  model_bic_final_oversample = bic_model_backward_oversample
} else {
  model_bic_final_oversample = bic_model_both_oversample
}

# Computing and storing predictions on the validation data
bic_model_predictions_oversample = ifelse(predict(model_bic_final_oversample, test_data_scaled_oversample, type = "response") > 0.5, 1, 0)
confusion_matrix_bic_oversample = table(bic_model_predictions_oversample, test_data_scaled_oversample$Churn)
confusion_matrices[["bic_oversample"]] = confusion_matrix_bic_oversample

rm(bic_model_backward_oversample)
rm(bic_model_both_oversample)
rm(bic_model_forward_oversample)
rm(bic_model_predictions_oversample)
rm(selected_model_name_bic_oversample)
rm(selected_variables_bic_both_oversample)
rm(selected_variables_bic_backward_oversample)
rm(selected_variables_bic_forward_oversample)
rm(num_features_bic_both_oversample)
rm(num_features_bic_backward_oversample)
rm(num_features_bic_forward_oversample)
rm(feature_counts_bic_oversample)
rm(variable)

## LASSO REGULARIZATION

set.seed(1)

#### Normal data

# Train the Lasso regression model
X_train = train_data_scaled
Y_train = as.numeric(X_train$Churn)
X_train$Churn = NULL
fit = glmnet(as.matrix(X_train), Y_train, alpha = 1, lambda = seq(0, 0.15, length = 30))

# Plot coefficient values against the (log-)lambda sequence
predictor_names = colnames(as.matrix(X_train))
colors = sample(1:length(predictor_names))
plot(fit, xvar = "lambda", label = TRUE, col = colors)
legend("topright", legend = predictor_names, col = colors, lty = 1, cex = 0.5, text.width = 1.2)

str(train_data_scaled)
sum(is.na(train_data_scaled))

# Train the Lasso regression model with hyperparameter tuning (lamda) and cross validation
model_lasso = train(Churn ~ ., 
                           data = train_data_scaled, 
                           method = "glmnet", 
                           metric = "Accuracy", 
                           trControl = trainControl(method = "cv", number = 10), 
                           tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.15, length = 30)))

# Plot accuracy values against the lambda sequence
plot(model_lasso,
     label = T, 
     xvar = "lambda")

# Retrieve the maximum accuracy and best tuning parameters 
# achieved during cross-validation
best_accuracy_lasso = max(model_lasso$results$Accuracy)
best_parameters_lasso = model_lasso$bestTune$lambda
cat("The highest value of accuracy:", best_accuracy_lasso, "is obtianed with lambda =", best_parameters_lasso)

# Computing and storing predictions
predictions_lasso = predict(model_lasso, test_data_scaled)
confusion_matrix_lasso = table(predictions_lasso, test_data_scaled$Churn)
confusion_matrices[["lasso_model"]] = confusion_matrix_lasso

# Dropping useless variables 
rm(colors)
rm(predictor_names)
rm(fit)
rm(best_accuracy_lasso)
rm(best_parameters_lasso)
rm(predictions_lasso)

#### Undersample

# Train the Lasso regression model on the undersampled data
X_train_undersample = train_data_scaled_undersample
Y_train_undersample = as.numeric(X_train_undersample$Churn)
X_train_undersample$Churn = NULL
fit_undersample = glmnet(as.matrix(X_train_undersample), Y_train_undersample, alpha = 1, lambda = seq(0, 0.15, length = 30))

# Plot coefficient values against the (log-)lambda sequence
predictor_names_undersample = colnames(as.matrix(X_train_undersample))
colors_undersample = sample(1:length(predictor_names_undersample))
plot(fit_undersample, xvar = "lambda", label = TRUE, col = colors_undersample)
legend("topright", legend = predictor_names_undersample, col = colors_undersample, lty = 1, cex = 0.5, text.width = 1.2)

# Train the Lasso regression model on the undersampled data with tuning
model_lasso_undersample = train(Churn ~ ., 
                                data = train_data_scaled_undersample, 
                                method = "glmnet", 
                                metric = "Accuracy", 
                                trControl = trainControl(method = "cv", number = 10), 
                                tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.15, length = 30)))

# Plot accuracy values against the lambda sequence
plot(model_lasso_undersample,
     label = TRUE, 
     xvar = "lambda")

# Retrieve the maximum accuracy and best tuning parameters 
# achieved during cross-validation
best_accuracy_lasso_undersample = max(model_lasso_undersample$results$Accuracy)
best_parameters_lasso_undersample = model_lasso_undersample$bestTune$lambda
cat("The highest value of accuracy:", best_accuracy_lasso_undersample, "is obtained with lambda =", best_parameters_lasso_undersample, "\n")

# Computing and storing predictions
predictions_lasso_undersample = predict(model_lasso_undersample, test_data_scaled_undersample)
confusion_matrix_lasso_undersample = table(predictions_lasso_undersample, test_data_scaled_undersample$Churn)
confusion_matrices[["lasso_model_undersample"]] = confusion_matrix_lasso_undersample
print(confusion_matrices[["lasso_model_undersample"]])

# Clean up variables
rm(colors_undersample)
rm(predictor_names_undersample)
rm(fit_undersample)
rm(best_accuracy_lasso_undersample)
rm(best_parameters_lasso_undersample)
rm(predictions_lasso_undersample)


#### Oversample

# Train the Lasso regression model on the oversampled data
X_train_oversample = train_data_scaled_oversample
Y_train_oversample = as.numeric(X_train_oversample$Churn)
X_train_oversample$Churn = NULL
fit_oversample = glmnet(as.matrix(X_train_oversample), Y_train_oversample, alpha = 1, lambda = seq(0, 0.15, length = 30))

# Plot coefficient values against the (log-)lambda sequence
predictor_names_oversample = colnames(as.matrix(X_train_oversample))
colors_oversample = sample(1:length(predictor_names_oversample))
plot(fit_oversample, xvar = "lambda", label = TRUE, col = colors_oversample)
legend("topright", legend = predictor_names_oversample, col = colors_oversample, lty = 1, cex = 0.5, text.width = 1.2)


# Train the Lasso regression model on the oversampled data with tuning
model_lasso_oversample = train(Churn ~ ., 
                               data = train_data_scaled_oversample, 
                               method = "glmnet", 
                               metric = "Accuracy", 
                               trControl = trainControl(method = "cv", number = 10), 
                               tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.15, length = 30)))

# Plot accuracy values against the lambda sequence
plot(model_lasso_oversample,
     label = TRUE, 
     xvar = "lambda")

# Retrieve the maximum accuracy and best tuning parameters 
# achieved during cross-validation
best_accuracy_lasso_oversample = max(model_lasso_oversample$results$Accuracy)
best_parameters_lasso_oversample = model_lasso_oversample$bestTune$lambda
cat("The highest value of accuracy:", best_accuracy_lasso_oversample, "is obtained with lambda =", best_parameters_lasso_oversample, "\n")

# Computing and storing predictions
predictions_lasso_oversample = predict(model_lasso_oversample, test_data_scaled_oversample)
confusion_matrix_lasso_oversample = table(predictions_lasso_oversample, test_data_scaled_oversample$Churn)
confusion_matrices[["lasso_model_oversample"]] = confusion_matrix_lasso_oversample

rm(colors_oversample)
rm(predictor_names_oversample)
rm(fit_oversample)
rm(best_accuracy_lasso_oversample)
rm(best_parameters_lasso_oversample)
rm(predictions_lasso_oversample)

## RIDGE REGRESSION

set.seed(1)

model_ridge = train(Churn ~ ., 
              data = train_data_scaled, 
              method = "glmnet", 
              metric = "Accuracy", 
              trControl = trainControl(method = "cv", number = 10), 
              tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 0.15, length = 30)))
max(ridge$results$Accuracy)
ridge$bestTune

ridge.plot = ridge %>% 
  ggplot(aes(x = lambda, y = Accuracy)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), check_overlap = TRUE, vjust = -0.5, size = 2.5) + 
  labs(x = TeX("Lambda ($\\lambda$)"), y = "Accuracy", title = "Accuracy vs. Lambda for Ridge Regularization") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ridge.plot


## RANDOM FORESTS

#### Normal Data

set.seed(1)
random_forest_model = randomForest(Churn ~ ., data = train_data, 
                                   mtry = 5, 
                                   ntree = 500, 
                                   importance = T)
print(random_forest_model)
importance(random_forest_model)

# Storing the error rate matrix. The error rate matrix computes the error rate on the OOB, also 
# with respect to the two classes.
error_rate_matrix = random_forest_model$err.rate
# The information is stored in a matrix to enable the data to be plotted
error_rate_matrix = data.frame(
  Trees = rep(1:nrow(error_rate_matrix), times = 3),
  Type = rep(c("OOB", "False", "True"), each = nrow(error_rate_matrix)),
  Error = c(error_rate_matrix[, "OOB"],
            error_rate_matrix[, "False"],
            error_rate_matrix[, "True"])
)
# Printing the result
ggplot(data = error_rate_matrix, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

# Visualizing feature importance
varImpPlot(random_forest_model)


##### Hyper parameter tuning

# Train the model using cross-validation and grid search
rf_grid_search = train(Churn ~ ., data = train_data, 
                       method = "rf", 
                       metric = "Accuracy", 
                       tuneGrid = expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), 
                       trControl = trainControl(method = "cv", number = 5)
                       )
best_mtry = rf_random_search$bestTune$mtry
cat("The optimal value (in terms of accuracy) for mtry is:", best_mtry)

# Make predictions on the test set using the best model
prediction = predict(rf_random_search, newdata = test_data)
# Confusion matrix
confusion_matrix_random_forest = table(prediction, test_data$Churn)

confusion_matrices[["random_forest"]] = confusion_matrix_random_forest

#### Undersample data

# Assuming `train_data_undersample` is your undersampled dataset

set.seed(1)
random_forest_model_undersample = randomForest(Churn ~ ., data = train_data_undersample, 
                                               mtry = 5, 
                                               ntree = 500, 
                                               importance = T)
print(random_forest_model_undersample)
importance(random_forest_model_undersample)

# Storing the error rate matrix
error_rate_matrix_undersample = random_forest_model_undersample$err.rate

# Creating the error rate data frame for plotting
error_rate_matrix_undersample = data.frame(
  Trees = rep(1:nrow(error_rate_matrix_undersample), times = 3),
  Type = rep(c("OOB", "False", "True"), each = nrow(error_rate_matrix_undersample)),
  Error = c(error_rate_matrix_undersample[, "OOB"],
            error_rate_matrix_undersample[, "False"],
            error_rate_matrix_undersample[, "True"])
)

# Plotting the error rate
ggplot(data = error_rate_matrix_undersample, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

# Visualizing feature importance
varImpPlot(random_forest_model_undersample)

##### Hyperparameter Tuning (Undersample)

# Train the model using cross-validation and grid search
rf_grid_search_undersample = train(Churn ~ ., data = train_data_undersample, 
                                   method = "rf", 
                                   metric = "Accuracy", 
                                   tuneGrid = expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), 
                                   trControl = trainControl(method = "cv", number = 5)
)
best_mtry_undersample = rf_grid_search_undersample$bestTune$mtry
cat("The optimal value (in terms of accuracy) for mtry (undersample) is:", best_mtry_undersample)

# Make predictions on the test set using the best model
prediction_undersample = predict(rf_grid_search_undersample, newdata = test_data)
# Confusion matrix
confusion_matrix_random_forest_undersample = table(prediction_undersample, test_data$Churn)

confusion_matrices[["random_forest_undersample"]] = confusion_matrix_random_forest_undersample

#### Oversample data

# Assuming `train_data_oversample` is your oversampled dataset

set.seed(1)
random_forest_model_oversample = randomForest(Churn ~ ., data = train_data_oversample, 
                                              mtry = 5, 
                                              ntree = 500, 
                                              importance = T)
print(random_forest_model_oversample)
importance(random_forest_model_oversample)

# Storing the error rate matrix
error_rate_matrix_oversample = random_forest_model_oversample$err.rate

# Creating the error rate data frame for plotting
error_rate_matrix_oversample = data.frame(
  Trees = rep(1:nrow(error_rate_matrix_oversample), times = 3),
  Type = rep(c("OOB", "False", "True"), each = nrow(error_rate_matrix_oversample)),
  Error = c(error_rate_matrix_oversample[, "OOB"],
            error_rate_matrix_oversample[, "False"],
            error_rate_matrix_oversample[, "True"])
)

# Plotting the error rate
ggplot(data = error_rate_matrix_oversample, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type))

# Visualizing feature importance
varImpPlot(random_forest_model_oversample)

##### Hyperparameter Tuning (Oversample)

# Train the model using cross-validation and grid search
rf_grid_search_oversample = train(Churn ~ ., data = train_data_oversample, 
                                  method = "rf", 
                                  metric = "Accuracy", 
                                  tuneGrid = expand.grid(mtry = c(5)), 
                                  trControl = trainControl(method = "cv", number = 5)
)
best_mtry_oversample = rf_grid_search_oversample$bestTune$mtry
cat("The optimal value (in terms of accuracy) for mtry (oversample) is:", best_mtry_oversample)

# Make predictions on the test set using the best model
prediction_oversample = predict(rf_grid_search_oversample, newdata = test_data)
# Confusion matrix
confusion_matrix_random_forest_oversample = table(prediction_oversample, test_data$Churn)

confusion_matrices[["random_forest_oversample"]] = confusion_matrix_random_forest_oversample

## XGBoosting

# In the first place we train the algorithm without tuning
model_xg = xgboost(as.matrix(X_train), label = Y_train, 
                 nrounds = 50, 
                 objective = "binary:logistic", 
                 eval_metric = "error")

# We then consider its performance
xg.pred <- ifelse(predict(fit.xg, X_test)> 0.5, 1, 0)
mean(xg.pred != Y_test)
print(table(xg.pred, test_data$Churn))
get.metrics(table(xg.pred, test_data$Churn))

# Just to get an insight on the relevance of the parameter ntreelimit, we consider the error
# on the test set. It is important to state that this is not a process to train the model but just
# an analysis on the parameter since models cannot be trained on the test set
train_errors = fit.xg$evaluation_log$train_error
val_errors <- numeric(50)

for (j in 1:50) {
  pred_j <- ifelse(predict(fit.xg, X_test, ntreelimit = j) > 0.5, 1, 0)
  val_errors[j] <- mean(pred_j != Y_test)
}

# Plot the error rates
plot(1:50, val_errors, type = "b", xlab = "Number of trees", ylab = "Error", col = 3, ylim = c(0, 0.3), cex = 0.5)
points(1:50, train_errors, type = "b", cex = 0.5)
legend("topright", legend = c("Train", "Test"), col = c(1, 3), lty = 1, lwd = 2, cex = 0.7)

# Identify the number of trees with the minimum validation error
optimal_trees <- which.min(val_errors)
abline(v = optimal_trees, col = "red")

# Print optimal number of trees
print(paste("Optimal number of trees:", optimal_trees))

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)

# Parallel processing setup
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

tune_grid <- expand.grid(
  nrounds = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  eta = 0.3,
  max_depth = 5,
  subsample = 1,
  colsample_bytree = 1,
  min_child_weight = 5,
  gamma = c(0.1, 0.2, 0.5, 0.75, 1)
)

set.seed(1)
fit_xg_cv <- train(
  Churn ~ ., data = train_data_oversample, 
  method = "xgbTree", 
  trControl = fitControl,
  verbose = FALSE, 
  tuneGrid = tune_grid,
  objective = "binary:logistic", 
  eval_metric = "error"
)

# Stop parallel processing
stopCluster(cl)

# Plot the cross-validation results
trellis.par.set(caretTheme())
plot(fit_xg_cv)

# Predictions on the test set using the best model
pred_xg_cv <- predict(fit_xg_cv, test_data, type = "raw")

# Compute the confusion matrix
confusion_matrix <- table(pred_xg_cv, test_data$Churn)
print(confusion_matrix)

# Calculate the mean error
mean_error <- mean(pred_xg_cv != test_data$Churn)
print(paste("Mean error rate:", mean_error))

library(randomForest)
library(dplyr)

# -------------------------

# Create a random sample of hyperparameter combinations
set.seed(123)
hyperparameter_grid <- expand.grid(
  n_tree = c(400,450,500,550,600),
  m_try = 1:12,
  max_nodes = c(10, 25, 50, 75, 100, 200, 300),
  node_size = 1:10
)

# Sample a smaller grid for random search
sampled_grid = hyperparameter_grid %>% sample_n(50)


# Evaluate all combinations
results_random_forest_tuning = data.frame()
for (i in 1:nrow(sampled_grid)) {
  params = sampled_grid[i, ]
  print(params)
  rf_model = randomForest(
    Churn ~ .,
    data = train_data_oversample,
    ntree = params$n_tree,
    mtry = params$m_try,
    maxnodes = params$max_nodes,
    nodesize = params$node_size,
    importance = TRUE
  )
  # Appending the results in the dataframe
  print(mean(rf_model$err.rate[, 1]))
  results_random_forest_tuning = rbind(results_random_forest_tuning,
                                       cbind(params, mean(rf_model$err.rate[, 1])))
}

# Find the best parameters
best_parameters = results %>%
  filter(oob_error == min(oob_error)) %>%
  slice(1)

# Retrieving best combination of parameters
best_parameters_random_forest = results[1, ]
print(best_parameters_random_forest)


# Train the final model with the best parameters
final_rf_model <- randomForest(
  Churn ~ .,
  data = train_data_oversample,
  ntree = best_parameters_random_forest$n_tree,
  mtry = best_parameters_random_forest$m_try,
  nodesize = best_parameters_random_forest$node_size,
  maxnodes = best_parameters_random_forest$max_nodes,
  importance = TRUE
)

# Make predictions on the test set
pred_rf = predict(final_rf_model, test_data)
confusion_matrix_rf = table(pred_rf, test_data$Churn)
print(confusion_matrix_rf)

# Calculate and print the mean error
mean_error_rf <- mean(pred_rf != test_data$Churn)
print(paste("Mean error rate:", mean_error_rf))


