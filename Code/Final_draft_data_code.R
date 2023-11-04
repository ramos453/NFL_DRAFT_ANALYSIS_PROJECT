
rm = (list= ls())


#Install packages and load libraries

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
library(readxl)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(ggridges)
library(textdata)

#Set working directory
setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")


#Read picks and round data
df_picks <- read.csv("picks.csv",header=TRUE)
df_round1 <- read.csv("round_1.csv",header=TRUE)
df_round23 <- read.csv("round_23.csv",header=TRUE)
df_round4567 <- read.csv("round_4567.csv",header=TRUE)

#Consolidating the round data frames into one data frame
stacked_round_df <- bind_rows(
  select(df_round1, -ends_with("_y")),
  select(df_round23, -ends_with("_y")),
  select(df_round4567, -ends_with("_y"))
)

#Collects all unique time stamps across the picks and rounds data frames
all_timestamps <- c(
  unique(df_picks$timestamp),
  unique(df_round1$timestamp),
  unique(df_round23$timestamp),
  unique(df_round4567$timestamp)
)

#Total number of unique time stamps
length(all_timestamps)

# Full join of stacked_round_df with df_picks: represents all timestamps  
combined_df <- data.frame(timestamp = unique(all_timestamps))
combined_df <- combined_df %>%
  full_join(df_picks, by = "timestamp") %>%
  full_join(stacked_round_df, by = "timestamp")

#Grouping and plotting the historgam showing the counts of time stamps in each bucket
num_bins <- 20                                                                  
hist_range <- range(combined_df$timestamp)                                      
timestamp_intervals <- cut(
  combined_df$timestamp,
  breaks = seq(hist_range[1], hist_range[2], length.out = num_bins + 1),
  include.lowest = TRUE
)
timestamp_numeric <- as.numeric(timestamp_intervals)  
histogram <- hist(timestamp_numeric, plot = FALSE)                              
plot(histogram, main = "Timestamp Histogram", xlab = "Timestamp Buckets", ylab = "Count")



#Analysis on image/ video rows
#New data frame with rows that include images / videos 
images_videos_df <- combined_df %>%
  filter(grepl("https", text))

nrow(images_videos_df)

# Create a histogram for positive sentiment (sentiment >= 0)
hist_positive <- hist(images_videos_df[images_videos_df$sentiment >= 0, ]$timestamp, plot = FALSE)
# Create a histogram for negative sentiment (sentiment < 0)
hist_negative <- hist(images_videos_df[images_videos_df$sentiment < 0, ]$timestamp, plot = FALSE)
# Plot the histograms
par(mfrow=c(1, 2))  # Arrange plots side by side
plot(hist_positive, main = "Positive Sentiment Histogram", xlab = "Timestamp Buckets", ylab = "Count")
plot(hist_negative, main = "Negative Sentiment Histogram", xlab = "Timestamp Buckets", ylab = "Count")
par(mfrow=c(1, 1))  # Reset the plotting layout



#The combined data frame without any rows that include images or videos
combined_df_no_images <- anti_join(combined_df, images_videos_df, by = "timestamp")

#Mapping for NFL team names
team_mapping <- c(
  "Chargers" = "LAC", "Colts" = "IND", "Lions" = "DET", "Eagles" = "PHI",
  "Raiders" = "LV", "Jaguars" = "JAX", "Packers" = "GB", "Patriots" = "NE",
  "Bears" = "CHI", "Panthers" = "CAR", "Seahawks" = "SEA", "49ers" = "SF",
  "Saints" = "NO", "Vikings" = "MIN", "Giants" = "NYG", "Browns" = "CLE",
  "Redskins" = "WAS", "Ravens" = "BAL", "Dolphins" = "MIA", "Broncos" = "DEN",
  "Falcons" = "ATL", "Cardinals" = "ARI", "Buccaneers" = "TB", "Jets" = "NYJ",
  "Bills" = "BUF", "Bengals" = "CIN", "Cowboys" = "DAL", "Chiefs" = "KC",
  "Texans" = "HOU", "Rams" = "LA", "Steelers" = "PIT", "Titans" = "TEN"
)

#Maps the abbreviation in to the NFL team name 
#Column Team.Fan represents the Reddit post author's team whose abbreviation is in column team.y
combined_df_no_images$Team.Fan <- team_mapping[combined_df_no_images$team.y]


df_temp <- combined_df_no_images #copy of cleaned data


#Creates a look up table for the picks data, ultimately finding all distinct combinations of the columns below 
lookup_table <- df_picks %>% select(timestamp, round, pick,full_name, team) %>% distinct()

# New columns in df_temp to capture the associated information based on the closest time stamp in the pick data with information relating to round, pick, full name and team 
df_temp <- df_temp %>%
  mutate(associated_timestamp = NA, associated_round = NA, associated_pick = NA, associated_full_name = NA, associated_team = NA)


# Fill in values based on the closest timestamp in the lookup_table
for (i in 1:nrow(df_temp)) {                                                               
  timestamp <- df_temp$timestamp[i]
  closest_match <- lookup_table[which.min(abs(lookup_table$timestamp - timestamp)), ]   #finds the closest pick's data time stamp to the current time stamp
  df_temp$associated_timestamp[i] <- closest_match$timestamp                            # fills in the associated values for the time stamp found 
  df_temp$associated_round[i] <- closest_match$round
  df_temp$associated_pick[i] <- closest_match$pick
  df_temp$associated_full_name[i] <- closest_match$full_name
  df_temp$associated_team[i] <- closest_match$team
}


View(df_temp)

# 9 NA's for sentiment score 
summary(df_temp)

#Dropping those 9 rows with NA's 
df_temp <- df_temp %>%
  filter(!is.na(sentiment))

summary(df_temp)

#Note: To avoid running the for loop above (~10 minutes), the file is written as a csv for further analysis 
write.csv(df_temp, file = "df_draft_associated_v1.csv", row.names = FALSE)



#Sentiment KeyWord Analysis ---------------------------------------------------------------------------------------------------------------------------------------------

# Copy of cleaned data
data <- read.csv("df_draft_associated_v1.csv",header=TRUE)

data <- df_temp

# sentiment lexicons
lexicon <- get_sentiments("afinn")

# Reads custom sentiment lexicon 
custom_lexicon <- read_excel("sentiment_keywords.xlsx")

# Combines lexicons
combined_lexicon <- bind_rows(lexicon, custom_lexicon)

# Renames the "word" column to "bigram"
combined_lexicon <- combined_lexicon %>%
  rename(bigram = word)

# Tokenizing data 
data_tokens <- data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Joining the tokenized data with the combined sentiment lexicon
sentiments <- data_tokens %>%
  inner_join(combined_lexicon, by = "bigram")

# Calculate sentiment scores for each bigram
sentiment_score <- sentiments %>%
  group_by(bigram) %>%
  summarize(sentiment_score = sum(value))

# Calculate word frequency for the bigrams
word_frequency <- data_tokens %>%
  count(bigram, sort = TRUE)

# Combine the sentiment scores with the word frequency
sentiment_score <- sentiment_score %>%
  left_join(word_frequency, by = "bigram")

# Arrange the results in descending order of frequency
sentiment_score <- sentiment_score %>%
  arrange(desc(n))
View(sentiment_score)

# Copy the cleaned data again
data <- df_temp
combined_results <- data.frame()

# Specify different phrase lengths to analyze
phrase_lengths <- c(2, 3, 4, 5, 6)

# Iterate through different phrase lengths
for (n in phrase_lengths) {
  # Tokenize the text into n-grams
  data_tokens <- data %>%
    unnest_tokens(phrase, text, token = "ngrams", n = n)
  
  # Calculate word frequency for the n-grams
  word_frequency <- data_tokens %>%
    count(phrase, sort = TRUE)

  # Append the results to the combined data frame  
  combined_results <- bind_rows(combined_results, word_frequency)
}

# Arrange the results in descending order of frequency
combined_results <- combined_results %>%
  arrange(desc(n))
View(combined_results)

# Write the original cleaned data to a CSV file
write.csv(df_temp, "comments_associated.csv", row.names=FALSE)



#Team Sentiment Analysis -----------------------------------------------------------------------------------------------------------------------------

df_team_sentiment <- read.csv("df_draft_associated_v1.csv",header=TRUE)

#Sentiment distribution by team, all look very similar
ggplot(df_team_sentiment, aes(x = sentiment, y = Team.Fan, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Sentiment", option = "C") +
  labs(title = 'Sentiment Distribution by Team')

ggplot(df_team_sentiment, aes(x = sentiment)) +
  geom_density(fill = "blue") +  # Adjust fill color as needed
  facet_wrap(~Team.Fan)  # Creates separate plots for each team


#Violin plots for teams. Most look similar but some (unsurprisingly PHI who has a notoriously tough fan base) have a more negative skew.
summary_list <- list()
unique_teams <- unique(df_team_sentiment$Team.Fan)
overall_count <- nrow(df_team_sentiment)
for (team in unique_teams) {
  team_data <- df_team_sentiment[df_team_sentiment$Team.Fan == team, ]
  
  team_summary <- summary(team_data$sentiment)
  
  count <- length(team_data$sentiment)
  proportion <- count / overall_count
  
  # Create a violin plot for the current team
  p <- ggplot(team_data, aes(x = Team.Fan, y = sentiment, fill = Team.Fan)) +
    geom_violin() +
    scale_fill_manual(values = setNames(rainbow(length(unique_teams)), unique_teams)) +
    labs(title = paste("Sentiment Violin Plot for", team))
  
  # Print or save the plot (you can customize this part)
  print(p)
  
  # Store the summary statistics and count in the list
  summary_list[[team]] <- list(Summary = team_summary, Count = count, Proportion = proportion)
}


#Summary statistics for each team, including count of comments and overall proportion of comments
for (team in unique_teams) {
  cat("Summary Statistics for", team, " (Count:", summary_list[[team]]$Count, ", Proportion:", summary_list[[team]]$Proportion, "):\n")
  print(summary_list[[team]]$Summary)
  cat("\n") }

#Correlation between proportion of comments and average sentiment
overall_count <- nrow(df_team_sentiment)
unique_teams <- unique(df_team_sentiment$Team.Fan)
team_proportion <- numeric(length(unique_teams))
team_avg_sentiment <- numeric(length(unique_teams))

for (i in 1:length(unique_teams)) {
  team <- unique_teams[i]
  team_data <- df_team_sentiment[df_team_sentiment$Team.Fan == team, ]
  
  # Calculate proportion
  team_count <- nrow(team_data)
  team_proportion[i] <- team_count / overall_count
  
  # Calculate average sentiment
  team_avg_sentiment[i] <- mean(team_data$sentiment, na.rm = TRUE)
}

scatter_data <- data.frame(
  Team = unique_teams,
  Proportion = team_proportion,
  AvgSentiment = team_avg_sentiment
)

ggplot(scatter_data, aes(x = Proportion, y = AvgSentiment, label = Team)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(hjust = -0.2, vjust = 0.3, size = 3) +
  labs(
    title = "Proportion vs. Average Sentiment",
    x = "Proportion of Count",
    y = "Average Sentiment"
  )

team_avg_sentiment

#The same as above but associated with the draft (by associated pick) team

summary_list2 <- list()
unique_teams2 <- unique(df_team_sentiment$associated_team)
#overall_count <- nrow(df_team_sentiment)
for (team in unique_teams2) {
  team_data <- df_team_sentiment[df_team_sentiment$associated_team == team, ]
  team_summary <- summary(team_data$sentiment)
  count <- length(team_data$sentiment)
  proportion <- count / overall_count
  p <- ggplot(team_data, aes(x = associated_team, y = sentiment, fill = associated_team)) +
    geom_violin() +
    scale_fill_manual(values = setNames(rainbow(length(unique_teams)), unique_teams)) +
    labs(title = paste("Sentiment Violin Plot for", team))
  
  print(p)
  
  summary_list2[[team]] <- list(Summary = team_summary, Count = count, Proportion = proportion)
}

for (team in unique_teams2) {
  cat("Summary Statistics for", team, " (Count:", summary_list2[[team]]$Count, ", Proportion:", summary_list2[[team]]$Proportion, "):\n")
  print(summary_list2[[team]]$Summary)
  cat("\n")
}

overall_count <- nrow(df_team_sentiment)
unique_teams <- unique(df_team_sentiment$associated_team)
team_proportion <- numeric(length(unique_teams))
team_avg_sentiment <- numeric(length(unique_teams))
for (i in 1:length(unique_teams)) {
  team <- unique_teams[i]
  team_data <- df_team_sentiment[df_team_sentiment$associated_team == team, ]
  
  # Calculate proportion
  team_count <- nrow(team_data)
  team_proportion[i] <- team_count / overall_count
  
  # Calculate average sentiment
  team_avg_sentiment[i] <- mean(team_data$sentiment, na.rm = TRUE)
}
scatter_data <- data.frame(
  Team = unique_teams,
  Proportion = team_proportion,
  AvgSentiment = team_avg_sentiment
)
ggplot(scatter_data, aes(x = Proportion, y = AvgSentiment, label = Team)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(hjust = -0.2, vjust = 0.3, size = 3) +
  labs(
    title = "Proportion vs. Average Sentiment",
    x = "Proportion of Count",
    y = "Average Sentiment"
  )

#Some notes on the data cleaning and analysis process:
#From initial cleaning, data is time stamped and not necessarily associated directly with a draft pick. Finding other ways, either by associating with the prior pick or with the commenter's team is important.
#There were (proportionally) large differences in both the average sentiments of different teams as well as the number of comments they received. It will be interesting to consider both as factors.
#More comments seems to correlate with a higher average team sentiment when comments are associated with a team by draft pick. When associating by the commenter's team affiliation this correlation reverses. This will be something to keep in mind when deciding how to bring in comment variables.


# Player Name Analysis --------------------------------------------------------------------------------------------

df_player_analysis <- read.csv("df_draft_associated_v1.csv")

# Summarizing how many rows are associated with each pick
summary_df <- df_player_analysis %>%
  group_by(associated_pick) %>%
  summarise(count = n())

as.data.frame(summary_df)

#View summary
View(summary_df)
summary(summary_df)

#Ordering by count 
sorted_summary_df <- summary_df[order(-summary_df$count), ]

# Plotting the distribution of the number of rows per pick number
plot <- ggplot(data = summary_df, aes(x = associated_pick, y = count)) +
  geom_bar(stat = "identity") +
  xlab("Pick Number") +
  ylab("Number of Rows") +
  ggtitle("Distribution # Of Rows by Pick Number")

print(plot)

#number of unque player names
unique(df_player_analysis$full_name)


#splitting the name into first and last name 
name_split <- strsplit(df_player_analysis$associated_full_name, " ")

#new columns with first and last name respectively
df_player_analysis$first_name <- sapply(name_split, function(x) x[1])
df_player_analysis$last_name <- sapply(name_split, function(x) x[2])

#setting text, first name and last name to lowercase 
df_player_analysis$text <- tolower(df_player_analysis$text)
df_player_analysis$first_name <- tolower(df_player_analysis$first_name)
df_player_analysis$last_name <- tolower(df_player_analysis$last_name)

#New column to check if last name is within text column
df_player_analysis$contains_lastname <- sapply(1:nrow(df_player_analysis), function(row) {
  result <- grepl(df_player_analysis$last_name[row], df_player_analysis$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

#New column to check if first name is within text column
df_player_analysis$contains_firstname <- sapply(1:nrow(df_player_analysis), function(row) {
  result <- grepl(df_player_analysis$first_name[row], df_player_analysis$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

#New column to check if either first name or last name is present
df_player_analysis$contains_relevant_info <- rowSums(df_player_analysis[c("contains_lastname", "contains_firstname")]) > 0

#New dataframe with data containing first or last name within the reddit comment
clean_data<- df_player_analysis[df_player_analysis$contains_relevant_info, c("associated_full_name","sentiment", "text")]

View(clean_data)
#~11,000 rows contain either the first name or the last name 

#Visualize
#creating table of counts for each name
name_counts <- table(clean_data$associated_full_name)

name_counts_df <- as.data.frame(name_counts)
names(name_counts_df) <- c("Name", "Count")

#View name count information
View(name_counts_df)
summary(name_counts_df)

#Plotting name counts 
ggplot(name_counts_df, aes(x = Name, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Associated Full Name") +
  ylab("Count") +
  ggtitle("Count of Associated Full Name")


#Distribution
ggplot(name_counts_df, aes(x = Count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  xlab("Count") +
  ylab("Frequency") +
  ggtitle("Distribution of Name Counts")


#Plotting the Top 10 Players based on count 
name_counts_df <- name_counts_df[order(-name_counts_df$Count), ]

top_10_names <- head(name_counts_df, 10)

ggplot(top_10_names, aes(x = reorder(Name, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Associated Full Name") +
  ylab("Count") +
  ggtitle("Top 10 Associated Full Names by Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Calculate the average sentiment for the 10 players with the highest count
average_sentiment <- clean_data %>%
  filter(associated_full_name %in% top_10_names$Name) %>%
  group_by(associated_full_name) %>%
  summarize(Avg_Sentiment = mean(sentiment))

#Plotting the average sentiment for the 10 players with the highest count 
ggplot(average_sentiment, aes(x = reorder(associated_full_name, -Avg_Sentiment), y = Avg_Sentiment)) +
  geom_bar(stat = "identity") +
  xlab("Associated Full Name") +
  ylab("Average Sentiment") +
  ggtitle("Average Sentiment for Top 10 Names") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Offensive Language Analysis ----------------------------------------------------------------------------------------------------

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












