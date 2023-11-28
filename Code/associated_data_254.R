
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

length(unique(combined_df$full_name))

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

length(unique(combined_df$full_name))


images_videos_df <- combined_df

images_videos_df$sentiment[grepl("https", images_videos_df$text, ignore.case = TRUE)] <- NA

View(images_videos_df)

distinct_count <- images_videos_df %>%
  filter(!is.na(full_name)) %>%
  distinct(full_name)

View(distinct_count)


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
#combined_df_no_images <- anti_join(combined_df, images_videos_df, by = "timestamp")

#Mapping for NFL team names

summary(images_videos_df)

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
images_videos_df$Team.Fan <- team_mapping[images_videos_df$team.y]
df_temp <- images_videos_df #copy of cleaned data

length(unique(images_videos_df$full_name))
length(unique(df_temp$full_name))


#Creates a look up table for the picks data, ultimately finding all distinct combinations of the columns below 
lookup_table <- df_picks %>% select(timestamp, round, pick,full_name, team) %>% distinct()

# New columns in df_temp to capture the associated information based on the closest time stamp in the pick data with information relating to round, pick, full name and team 
df_temp <- df_temp %>%
  mutate(associated_timestamp = NA, associated_round = NA, associated_pick = NA, associated_full_name = NA, associated_team = NA)


# First Method of associating data
  # for (i in 1:nrow(df_temp)) {                                                               
  #   timestamp <- df_temp$timestamp[i]
  #   closest_match <- lookup_table[which.min(abs(lookup_table$timestamp - timestamp)), ]   #finds the closest pick's data time stamp to the current time stamp
  #   df_temp$associated_timestamp[i] <- closest_match$timestamp                            # fills in the associated values for the time stamp found 
  #   df_temp$associated_round[i] <- closest_match$round
  #   df_temp$associated_pick[i] <- closest_match$pick
  #   df_temp$associated_full_name[i] <- closest_match$full_name
  #   df_temp$associated_team[i] <- closest_match$team
  # }

#Second Method of Associating data 
# Fill in values based on the closest previous timestamp in the lookup_table
 for (i in 1:nrow(df_temp)) {                                                               
   timestamp <- df_temp$timestamp[i]
   valid_timestamps <- lookup_table$timestamp[lookup_table$timestamp <= timestamp]
   if (length(valid_timestamps) > 0) {
     closest_match <- lookup_table[which.min(timestamp - valid_timestamps), ]   # finds the closest previous pick's data timestamp
     df_temp$associated_timestamp[i] <- closest_match$timestamp
     df_temp$associated_round[i] <- closest_match$round
     df_temp$associated_pick[i] <- closest_match$pick
     df_temp$associated_full_name[i] <- closest_match$full_name
     df_temp$associated_team[i] <- closest_match$team
   } else {
     # If no previous timestamp is found, you may want to handle this case accordingly
     # For now, setting associated values to NA
     df_temp$associated_timestamp[i] <- NA
     df_temp$associated_round[i] <- NA
     df_temp$associated_pick[i] <- NA
     df_temp$associated_full_name[i] <- NA
     df_temp$associated_team[i] <- NA
   }
 }


summary(df_temp)

length(unique(df_temp$associated_full_name))
length(unique(df_temp$full_name))

# Assuming df_temp is your data frame

missing_players <- setdiff(unique(df_temp$full_name), unique(df_temp$associated_full_name))
print(missing_players)


missing_players <- setdiff(unique(df_temp$full_name), unique(df_temp$associated_full_name))

missing_players_df <- df_temp[df_temp$full_name %in% missing_players, c("full_name", "pick")]
print(missing_players_df)


#Note: To avoid running the for loop above (~10 minutes), the file is written as a csv for further analysis 
write.csv(df_temp, file = "df_draft_associated_m2.csv", row.names = FALSE)


