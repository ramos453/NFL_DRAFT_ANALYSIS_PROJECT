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
if (!require("data.table")) {
  install.packages("data.table")
}
library(readxl)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(ggridges)
library(textdata)
library(data.table)

setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")

df_player_analysis <- read.csv("df_draft_associated_m2.csv")

length(unique(df_player_analysis$associated_full_name))

nfl_2020 <- read.csv('nfl_2020_1.csv', header = TRUE, sep = ',')


# Summarizing how many rows are associated with each pick

summary_df <- df_player_analysis %>%
  group_by(associated_pick) %>%
  summarise(count = n())

as.data.frame(summary_df)

#View summary
#View(summary_df)
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

#number of unique player names
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


#Visualize
#creating table of counts for each name
name_counts <- table(clean_data$associated_full_name)

name_counts_df <- as.data.frame(name_counts)
names(name_counts_df) <- c("Name", "Count")

#View name count information
#View(name_counts_df)
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


#Final Player Name Analysis DataFrame
head(df_player_analysis)

df_new <- df_player_analysis %>%
  group_by(associated_full_name) %>%
  summarise(
    pick = first(associated_pick),
    round = first(associated_round),
    Direct_mentions = sum(contains_relevant_info == TRUE),
    Indirect_mentions = sum(contains_relevant_info == FALSE),
    avg_sentiment_direct_mentions = ifelse(sum(contains_relevant_info) > 1, mean(sentiment[contains_relevant_info], na.rm = TRUE),0),
    avg_sentiment_indirect_mentions = ifelse(sum(!contains_relevant_info) > 1, mean(sentiment[!contains_relevant_info], na.rm = TRUE),0),
    Direct_mention_binary = ifelse(any(contains_relevant_info), 1, 0)
  ) %>%
  arrange(pick)  # Sort by pick number


View(df_new)

#write.csv(df_new, file = "player_name_analysis.csv", row.names = FALSE)

