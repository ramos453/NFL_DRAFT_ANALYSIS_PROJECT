
rm = (list= ls())

setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")


library(readxl)
library(dplyr)
library(tidyr)

new_data <- read.csv("df_draft_associated_v1.csv")

tail(new_data)


library(ggplot2)

summary_df <- new_data %>%
  group_by(associated_pick) %>%
  summarise(count = n())

as.data.frame(summary_df)


# Plot the data
plot <- ggplot(data = summary_df, aes(x = associated_pick, y = count)) +
  geom_bar(stat = "identity") +
  xlab("Pick Number") +
  ylab("Number of Rows") +
  ggtitle("Distribution of Rows by Pick Number")

print(plot)


# Name Analysis part one 

unique(new_data$full_name)

sum(is.na(new_data$full_name))

sum(is.na(new_data$associated_full_name))


#Name Analysis part two


name_split <- strsplit(new_data$associated_full_name, " ")

new_data$first_name <- sapply(name_split, function(x) x[1])
new_data$last_name <- sapply(name_split, function(x) x[2])

new_data$text <- tolower(new_data$text)
new_data$first_name <- tolower(new_data$first_name)
new_data$last_name <- tolower(new_data$last_name)

new_data$contains_lastname <- sapply(1:nrow(new_data), function(row) {
  result <- grepl(new_data$last_name[row], new_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

new_data$contains_firstname <- sapply(1:nrow(new_data), function(row) {
  result <- grepl(new_data$first_name[row], new_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

new_data$contains_relevant_info <- rowSums(new_data[c("contains_lastname", "contains_firstname")]) > 0

clean_data<- new_data[new_data$contains_relevant_info, c("associated_full_name","sentiment", "text")]

View(clean_data)

#~11,000 rows contain either the first name or the last name 



#Visualize

name_counts <- table(clean_data$associated_full_name)

name_counts_df <- as.data.frame(name_counts)
names(name_counts_df) <- c("Name", "Count")

View(name_counts_df)

summary(name_counts_df)

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


#Top 10 Players

name_counts_df <- name_counts_df[order(-name_counts_df$Count), ]

top_10_names <- head(name_counts_df, 10)

ggplot(top_10_names, aes(x = reorder(Name, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Associated Full Name") +
  ylab("Count") +
  ggtitle("Top 10 Associated Full Names by Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sentiment

average_sentiment <- clean_data %>%
  filter(associated_full_name %in% top_10_names$Name) %>%
  group_by(associated_full_name) %>%
  summarize(Avg_Sentiment = mean(sentiment))

ggplot(average_sentiment, aes(x = reorder(associated_full_name, -Avg_Sentiment), y = Avg_Sentiment)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  xlab("Associated Full Name") +
  ylab("Average Sentiment") +
  ggtitle("Average Sentiment for Top 10 Names") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
