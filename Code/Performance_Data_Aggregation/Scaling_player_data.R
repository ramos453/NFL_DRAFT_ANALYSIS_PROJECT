#Combining Data 

if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}
if (!require("tidytext")) {
  install.packages("tidytext")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("ggridges")) {
  install.packages("ggridges")
}
if (!require("textdata")) {
  install.packages("textdata")
}
if (!require("corrplot")) {
  install.packages("corrplot")
}

library(corrplot)
library(readxl)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(ggridges)
library(textdata)


setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")


filtered_data_2020 <- read.table('filtered_data_2020.txt', header = TRUE, sep = ',') #player performance data
player_name_analysis <- read.csv('player_name_analysis.csv', header = TRUE)
pos_avg_pick <- read.table('pos_avg_pick.txt', header = TRUE, sep = ',')
team_draft_performance <- read.csv('team_draft_performance.txt', header = TRUE) #nicks team sentiment analysis

colnames(filtered_data_2020)
colnames(player_name_analysis)
colnames(pos_avg_pick)
colnames(team_draft_performance)

player_data <- filtered_data_2020 %>%
  left_join(player_name_analysis, by = c("player" = "associated_full_name"))

player_data_1 <- player_data[c(
  'value_delta', 
  'Direct_mentions', 
  'Indirect_mentions', 
  'avg_sentiment_direct_mentions', 
  'avg_sentiment_indirect_mentions', 
  'Direct_mention_binary'
)]

head(player_data_1)

#Scaling Data Attempt 1
response_variable <- player_data_1$value_delta
predictor_variables <- player_data_1[, c(
  'Direct_mentions',
  'Indirect_mentions',
  'avg_sentiment_direct_mentions',
  'avg_sentiment_indirect_mentions'
)]

scaled_predictors <- scale(predictor_variables)

scaled_data <- cbind(scaled_predictors, Direct_mention_binary = player_data_1$Direct_mention_binary, value_delta = response_variable)

scaled_data <- as.data.frame(scaled_data)

head(scaled_data)

#Linear Regression Model 1 
player_lm <-lm(value_delta ~ ., data = scaled_data)

summary(player_lm)

#Scaling Attempt 2: for loop different scaling factors -----------------------------------------------------------
# Assuming the 'player_data_1' is already loaded with the data

# Assuming the 'player_data_1' is already loaded with the data

response_variable <- player_data_1$value_delta
predictor_variables <- player_data_1[, c(
  'Direct_mentions',
  'Indirect_mentions',
  'avg_sentiment_direct_mentions',
  'avg_sentiment_indirect_mentions'
)]
scaling_factors <- seq(.1, 3, by = 0.1)
best_r_squared <- -Inf
best_adjusted_r_squared <- -Inf
best_scaling_factor <- NA

# Iterate over scaling factors
for (scaling_factor in scaling_factors) {
  # Scale only the specified columns
  scaled_avg_sentiment_direct_mentions <- scale(predictor_variables$avg_sentiment_direct_mentions, center = TRUE, scale = scaling_factor)
  scaled_avg_sentiment_indirect_mentions <- scale(predictor_variables$avg_sentiment_indirect_mentions, center = TRUE, scale = scaling_factor)
  
  # Combine scaled columns with the rest of the predictor variables
  scaled_predictors <- cbind(
    Direct_mentions = predictor_variables$Direct_mentions,
    Indirect_mentions = predictor_variables$Indirect_mentions,
    avg_sentiment_direct_mentions = scaled_avg_sentiment_direct_mentions,
    avg_sentiment_indirect_mentions = scaled_avg_sentiment_indirect_mentions
  )
  
  # Combine scaled predictor variables with binary variable and response variable
  scaled_data <- cbind(scaled_predictors, Direct_mention_binary = player_data_1$Direct_mention_binary, value_delta = response_variable)
  scaled_data <- as.data.frame(scaled_data)
  summary(scaled_data)
  # Fit linear regression model
  player_lm <- lm(value_delta ~ ., data = scaled_data)
  
  # Extract R-squared and adjusted R-squared
  current_r_squared <- summary(player_lm)$r.squared
  current_adjusted_r_squared <- summary(player_lm)$adj.r.squared
  
  # Print or store results for each iteration
  cat("Scaling Factor:", scaling_factor, "R-squared:", current_r_squared, "Adjusted R-squared:", current_adjusted_r_squared, "\n")
  
  # Update best values if the current model is better
  if (current_adjusted_r_squared > best_adjusted_r_squared) {
    best_r_squared <- current_r_squared
    best_adjusted_r_squared <- current_adjusted_r_squared
    best_scaling_factor <- scaling_factor
  }
}

# Print the best scaling factor and corresponding R-squared and adjusted R-squared
cat("Best Scaling Factor:", best_scaling_factor, "Best R-squared:", best_r_squared, "Best Adjusted R-squared:", best_adjusted_r_squared, "\n")






#PLotting

#Normality Assumptions ----------------------------------------------------------------------------------------

plot(player_lm)



# Scatter plots for each predictor variable against the response variable---------------------------------------
par(mfrow = c(2, 2))  # Setting up a 2x2 grid for the plots

plot(value_delta ~ Direct_mentions, data = scaled_data, main = "Direct_mentions vs. value_delta", col = "blue", pch = 16)
plot(value_delta ~ Indirect_mentions, data = scaled_data, main = "Indirect_mentions vs. value_delta", col = "green", pch = 16)
plot(value_delta ~ avg_sentiment_direct_mentions, data = scaled_data, main = "avg_sentiment_direct_mentions vs. value_delta", col = "red", pch = 16)
plot(value_delta ~ avg_sentiment_indirect_mentions, data = scaled_data, main = "avg_sentiment_indirect_mentions vs. value_delta", col = "orange", pch = 16)

# Resetting the plot layout to default
par(mfrow = c(1, 1))



#Covariance Matrix----------------------------------------------------------------------------------------------
selected_columns <- c(
  'Direct_mentions',
  'Indirect_mentions',
  'avg_sentiment_direct_mentions',
  'avg_sentiment_indirect_mentions'
)

scaled_data_selected <- scaled_data[complete.cases(scaled_data[, selected_columns]), ]

cov_matrix <- cov(scaled_data_selected[, selected_columns])

corrplot(cov_matrix, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")






















