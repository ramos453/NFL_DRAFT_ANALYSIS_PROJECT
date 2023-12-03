

library(dplyr)
library(glmnet)
library(RColorBrewer)
library(tidyr)
library(tidytext)
library(ggplot2)
library(ggridges)
library(textdata)
library(data.table)

setwd("C:/Users/pfram/OneDrive/Desktop/Data for Business Analytics/nfl_project_final/")

# Assuming df_data is your dataframe
df_data <- read.csv('merged_data_1.csv',sep = ',', header= TRUE)

# Remove rows with missing values in the 'position' column
df_data <- df_data[complete.cases(df_data$position), ]


df_data <- na.omit(df_data)

summary(df_data)

# Main Regression
naive_regression <- lm(expected_value ~ round.x + position, data = df_data)

naive_regression_1 <- lm(sum_raw_value_provided ~ round.x + position, data = df_data)

summary(naive_regression_1)

# Additional Regression
additional_regression_value_delta <- lm(value_delta ~ mean_sentiment + Direct_mentions +
                              avg_sentiment_direct_mentions + avg_sentiment_indirect_mentions,
                            data = df_data)

additional_regression_expected_value <- lm(expected_value ~ mean_sentiment + Direct_mentions +
                                          avg_sentiment_direct_mentions + avg_sentiment_indirect_mentions,
                                        data = df_data)


# Generate predictions for both models
naive_predictions <- predict(naive_regression, df_data)  #expected value
additional_predictions_vd <- predict(additional_regression_value_delta, df_data) #value delta
naive_predictions_1 <- predict(naive_regression_1, df_data) #sum_value_provided
additional_predictions_ev <- predict(additional_regression_expected_value, df_data) #expected value

# Expected Value weighted average results
for (weight_naive in seq(.9, 1, by = 0.01)) {
  weight_additional <- 1 - weight_naive
  # Combine predictions with weights
  combined_predictions <- weight_naive * naive_predictions + weight_additional * additional_predictions_ev
  # Calculate R-squared for combined predictions
  r_squared <- 1 - sum((df_data$expected_value - combined_predictions)^2) / sum((df_data$expected_value - mean(df_data$expected_value))^2)
  # Print or store results
  cat("Weight Naive:", weight_naive, "| Weight Additional:", weight_additional, "| R-squared:", r_squared, "\n")
}

# Baseline: Expected value, Additional: Value_Delta --> sum_raw_value_provided
for (weight_naive in seq(.5, 1, by = 0.05)) {
  weight_additional <- 1 - weight_naive
  # Combine predictions with weights
  combined_predictions <- weight_naive * naive_predictions_1 + weight_additional * additional_predictions_vd
  # Calculate R-squared for combined predictions
  r_squared <- 1 - sum((df_data$sum_raw_value_provided - combined_predictions)^2) / sum((df_data$sum_raw_value_provided - mean(df_data$sum_raw_value_provided))^2)
  # Print or store results
  cat("Weight Naive:", weight_naive, "| Weight Additional:", weight_additional, "| R-squared:", r_squared, "\n")
}











