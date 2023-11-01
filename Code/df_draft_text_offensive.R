library(readxl)
library(dplyr)
library(tidyr)
rm = (list= ls())
setwd("F:\\OMSA\\FA23-MGT6203\\Group Project\\Data")
df_draft_txt <- read.csv("df_draft_associated_v1.csv",header=TRUE)
col_names <- colnames(df_draft_txt)
print(col_names)

# Calculate the summary statistics of the sentiment column
sentiment_summary <- summary(df_draft_txt$sentiment)
# Print the summary statistics
print(sentiment_summary)

# List of offensive words in lowercase
offensive_words <- c(
  "fuck", "shit", "ass", "dumb", "nah", "bae", "baller", "chill", "dank", "dope",
  "extra", "fleek", "gucci", "lit", "salty", "savage", "shook", "tea", "yas",
  "adulting", "amped", "awesome", "basic", "beat", "bet", "bestie", "bounce", "bougie",
  "bro", "bummer", "bussin'", "bust", "cancel", "clapback", "clutch", "cool", "corny",
  "cringe", "drip", "dude", "dunno", "epic fail", "ex", "fake woke", "finesse", "fire",
  "flake", "flex", "fomo", "freebie", "gas", "ghost", "goat", "gucci gang", "hype"
)

# Step 1: Convert all text to lowercase
df_draft_txt$text <- tolower(df_draft_txt$text)

# Step 2: Count the frequency of offensive words in the text
word_freq <- table(unlist(strsplit(df_draft_txt$text, " ")))

# Step 3: Filter the word frequency for offensive words
offensive_word_freq <- word_freq[names(word_freq) %in% offensive_words]

# Step 4: Get the top 20 most frequent offensive words
top_20_offensive_words <- head(sort(offensive_word_freq, decreasing = TRUE), 20)

# Print the result
print(top_20_offensive_words)

# Step 1: Convert all text to lowercase
df_draft_txt$text <- tolower(df_draft_txt$text)

# Create an empty data frame to store results
result_table <- data.frame(Word = character(20), Frequency = integer(20), TotalSentiment = numeric(20))

# Loop through the top 20 offensive words
for (i in 1:20) {
  word <- names(top_20_offensive_words)[i]
  freq <- top_20_offensive_words[i]
  total_sentiment <- 0  # Initialize total sentiment for the word
  
  # Loop through each row in the dataset
  for (row in 1:nrow(df_draft_txt)) {
    text <- df_draft_txt$text[row]
    sentiment_score <- df_draft_txt$sentiment[row]
    
    # Check if the offensive word is in the text of the row
    if (grepl(word, text)) {
      # If the word is found, add its sentiment score to the total_sentiment
      total_sentiment <- total_sentiment + sentiment_score
    }
  }
  
  # Store the result in the result_table
  result_table[i, ] <- c(word, freq, total_sentiment)
}

# Print the result table
print(result_table)

####More info on the top 20 offensive words:
# Calculate additional statistics for sentiment scores
additional_stats <- data.frame(
  Word = result_table$Word,
  MeanSentiment = numeric(20),
  MedianSentiment = numeric(20),
  StdDevSentiment = numeric(20),
  Q1 = numeric(20),
  Q3 = numeric(20)
)

for (i in 1:20) {
  word <- result_table$Word[i]
  
  # Filter sentiment scores for the current word
  sentiment_scores <- df_draft_txt$sentiment[grepl(word, df_draft_txt$text)]
  
  # Calculate statistics
  additional_stats$MeanSentiment[i] <- mean(sentiment_scores, na.rm = TRUE)
  additional_stats$MedianSentiment[i] <- median(sentiment_scores, na.rm = TRUE)
  additional_stats$StdDevSentiment[i] <- sd(sentiment_scores, na.rm = TRUE)
  quantiles <- quantile(sentiment_scores, na.rm = TRUE)
  additional_stats$Q1[i] <- quantiles["25%"]
  additional_stats$Q3[i] <- quantiles["75%"]
}

# Combine the additional statistics with the result_table
result_table <- cbind(result_table, additional_stats[, -1])

# Print the updated result table with additional statistics
print(result_table)
View(result_table)

library(ggplot2)

###Lowest mean sentiment, bottom 10 words
# Sort the result_table by MeanSentiment in ascending order and select the top 10 rows
top_10_words <- head(result_table[order(result_table$MeanSentiment), ], 10)

# Reorder the Word column based on MeanSentiment
top_10_words$Word <- factor(top_10_words$Word, levels = top_10_words$Word)

# Create the bar plot with error bars
ggplot(top_10_words, aes(x = Word, y = MeanSentiment, ymin = MeanSentiment - StdDevSentiment, ymax = MeanSentiment + StdDevSentiment)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_errorbar(width = 0.2) +
  labs(x = "Word", y = "Mean Sentiment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  ggtitle("Top 10 Words by Mean Sentiment with Std Dev Error Bars") +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

# Save the plot as a PDF
ggsave("top_10_mean_sentiment_plot.pdf", width = 12, height = 8)
