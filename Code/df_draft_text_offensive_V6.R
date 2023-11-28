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

#  Convert all text to lowercase
df_draft_txt$text <- tolower(df_draft_txt$text)

#  Count the frequency of offensive words in the text
word_freq <- table(unlist(strsplit(df_draft_txt$text, " ")))

#  Filter the word frequency for offensive words
offensive_word_freq <- word_freq[names(word_freq) %in% offensive_words]

#view the offensive word freq
View(offensive_word_freq)

#### Calculate offensive word counts for each player
player_offensive_counts <- aggregate(text ~ name, data = df_draft_txt, FUN = function(x) sum(sapply(x, function(text) sum(sapply(offensive_words, function(word) sum(grepl(word, text)))))))

# Rename the aggregated column
colnames(player_offensive_counts)[2] <- "offensive_word_count"

# Print the result
View(player_offensive_counts)

#Look at the sum of offensive words for all players
sum(player_offensive_counts$offensive_word_count)

# Calculate the total number of offensive words in the entire dataset
total_offensive_count <- sum(sapply(df_draft_txt$text, function(text) sum(sapply(offensive_words, function(word) sum(grepl(word, text))))))

# Print the total number of offensive words
cat("Total Offensive Words:", total_offensive_count, "\n")