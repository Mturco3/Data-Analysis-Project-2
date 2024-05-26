# Importing libraries

library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(maps)
library(gridExtra)


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




# EDA

colnames(Data)

## TARGET VARIABLE DISTRIBUTION 

ggplot(Data, aes(x = Churn, fill = Churn)) + 
  geom_bar(color = "black", alpha = 0.7) + 
  ggtitle("Distribution of Churn") + 
  xlab("Churn") + 
  ylab("Count") + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = 'bold'),
    axis.text.y = element_text(size = 12, face = 'bold'),
  )

x = table(Data$Churn)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$Churn), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c("blue",
             "red"),
     cex = 1.6)
  mtext("Churn", side = 3, cex = 2)
  legend("topleft", 
         pch = 15, 
         col = c("blue", "red"),
         c("Not churn", "Churn"), cex = 1.2,
         bty = "n")}

## INTERNATIONAL PLAN

x = table(Data$International.plan)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$International.plan), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90,
     col = c("blue",
             "red"),
     cex = 1.5)
  mtext("International Plan", side = 3, cex = 2)
  legend("topleft", 
         pch = 15, 
         col = c("blue", "red"),
         c("Not churn", "Churn"), cex = 1.2,
         bty = "n")}

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



## CATEGORICAL VARIABLES

plot_list <- list()

# Loop through the categorical variables to create individual plots
for (variable in categorical_variables) {
  if (variable == "State") {
    next
  }
  plot <- ggplot(Data, aes_string(x = variable, fill = "Churn")) + 
    geom_bar(position = "dodge", color = "black", alpha = 0.7) + 
    scale_fill_manual(values = c("blue", "red")) + 
    xlab(variable) + 
    ylab("Count") + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  # Add the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange the plots in a grid layout
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 1, ncol = 3))

# Save the combined plot as an image
ggsave("combined_plot.png", combined_plot, width = 10, height = 5)

# Display the combined plot
print(combined_plot)

  

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


states = unique(Data$State)


for (state in states) {
  churn_count = table(Data[Data$State == state, "Churn"])
  proportion_churn <- churn_count["True"] / sum(churn_count)
  cat(state, ":", churn_count, "   Proportion Churn:", proportion_churn, "\n")
}

Data$ChurnNumeric <- ifelse(Data$Churn == "True", 1, 0)

churn_rate <- Data %>%
  group_by(State) %>%
  summarize(ChurnRate = mean(ChurnNumeric))

# Get US state map data
states <- map_data("state")

# Create a reference data frame for state names and abbreviations
state_abbr <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  region = tolower(c("alabama", "alaska", "arizona", "arkansas", "california", 
                     "colorado", "connecticut", "delaware", "florida", "georgia", 
                     "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", 
                     "kentucky", "louisiana", "maine", "maryland", "massachusetts", 
                     "michigan", "minnesota", "mississippi", "missouri", "montana", 
                     "nebraska", "nevada", "new hampshire", "new jersey", 
                     "new mexico", "new york", "north carolina", "north dakota", 
                     "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", 
                     "south carolina", "south dakota", "tennessee", "texas", 
                     "utah", "vermont", "virginia", "washington", "west virginia", 
                     "wisconsin", "wyoming"))
)

churn_rate <- merge(churn_rate, state_abbr, by = "State")

# Merge churn data with geographic data
churn_map <- merge(states, churn_rate, by = "region", all.x = TRUE)

# Create the plot
ggplot(data = churn_map, aes(x = long, y = lat, group = group, fill = ChurnRate)) +
  geom_polygon(color = "black", ) +
  scale_fill_gradient(low = "blue", high = "red", name = "Churn Rate") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Churn Rate Across US States")

## NUMEICAL VARIABLES

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
## PREPROCESSING



## LOW DIMENSIONAL MODEL

#### In the EDA we have seen that International Plan seems to have an impact on the Churn Rate
#### We consider also a simple model trained only with total day minutes

# Fit logistic regression model
set.seed(1)
simple_model <- glm(Churn ~ Total.day.minutes * International.plan, 
                           family = binomial(link = "logit"), 
                           data = Data)

# View model summary
summary(gender_income_model)

predicted_probabilities_logistic <- predict(simple_model, newdata = Data, type="response")

Data$predicted_probabilities <- predict(simple_model, newdata = Data, type="response")

#Plotting computed probabilities
ggplot(Data, aes(x = Total.day.minutes, y = predicted_probabilities, color = International.plan)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("red", "blue"))

Data$predicted_probabilities = NULL # We drop the column of predicted probabilities since it is now useless

