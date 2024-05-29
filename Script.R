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

# Create vectors to store plots
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
  top = textGrob("Categorical Variables Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)
grid.arrange(
  arrangeGrob(grobs = plot_list_relationship, nrow = 1, ncol = 3),
  top = textGrob("Churn Distribution Across Categorical Variables", gp = gpar(fontsize = 20, fontface = "bold")),
  right = legend,
  left = "Count"
)

# Removing useless variables
rm(binwidth)
rm(legend)
rm(histogram_plot)
rm(piechart_plot)
rm(plot)
rm(plot_list_numerical)



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

## -------------------------------- ALL CORRECT --------------------------------

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
  
  # 
  plot_numerical_relationship = ggplot(Data, aes(x = Churn, y = variable, fill = Churn)) + 
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
  
  
  # Add the plot to the list
  plot_list_numerical[[variable]] = plot_numerical
  plot_list_numerical_relationship[[variable]] = plot_numerical_relationship
}

grid.arrange(
  arrangeGrob(grobs = plot_list_numerical, nrow = 5, ncol = 3),
  top = textGrob("Numerical Variables Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)

# TO ADJUST
grid.arrange(
  arrangeGrob(grobs = plot_list_numerical_relationship, nrow = 5, ncol = 3),
  top = textGrob("Numerical Variables Distribution", gp = gpar(fontsize = 20, fontface = "bold"))
)

# Removing variables that are now useless
rm(means_vec)
rm(median_vec)
rm(sd_vec)


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
         col = colorRampPalette(c("red", "white", "blue"))(200))


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
  cat("Dropped", variable)
  cat("\n")
  numerical_variables = setdiff(numerical_variables, variable)
}

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
Data$region = sapply(Data$State, find_region)

# Converting region in a factor and dropping (but storing) the State variable
Data$region = as.factor(Data$region)
states = Data$State
Data$State = NULL



## DATASET SPLIT

### NORMAL SPLIT

set.seed(1)

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
print(table(test_data$Churn))
cat("Proportions:")
print(prop.table(table(test_data$Churn)))

### UNDERSAMPLING

set.seed(1)

# Get the class counts
class_counts <- table(train_data$Churn)
min_class_count <- min(class_counts)

# Calculate the total number of samples needed for balanced under sampling
# We want each class to have min_class_count samples
target_N <- 2 * min_class_count

# Perform under sampling
train_data_undersample <- ovun.sample(Churn ~ ., data = train_data, method = "under", N = target_N)$data

# Response variable distribution in the train set with under sampling
cat("Distribution of the target variable in the train set with undersampling:\n")
cat("Counts:")
print(table(train_data_undersample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_undersample$Churn)))

# Dropping useless variables
rm(min_class_count)
rm(target_N)

### OVERSAMPLING

set.seed(1)

max_class_count <- max(class_counts)

# Calculate the total number of samples needed for balanced oversampling
# We want each class to have max_class_count samples
target_N <- 2 * max_class_count

# Perform oversampling
train_data_oversample <- ovun.sample(Churn ~ ., data = train_data, method = "over", N = target_N)$data

# Response variable distribution in the train set with over sampling
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(train_data_oversample$Churn))
cat("Proportions:")
print(prop.table(table(train_data_oversample$Churn)))

# Dropping useless variables
rm(max_class_count)
rm(target_N)
rm(class_counts)

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
    panel.grid.major = element_line(color = "gray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  
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

# BEST MODEL SELECTION

## Baseline Logistic Regression Model

model_baseline_logistic = glm(Churn ~ ., data = train_data_oversample_scaled, family = "binomial")
summary(model_baseline_logistic)
baseline_logistic_predictions = ifelse(predict(model_baseline_logistic, newdata = test_data_scaled_oversample) > 0.5, 1, 0)
confusion_matrix = table(baseline_logistic_predictions, test_data_scaled_oversample$Churn)

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

print(get.metrics(confusion_matrix))

#### CONTINUE
