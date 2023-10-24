install.packages("readxl")
library(readxl)
library(dplyr)
library(tidyr)
rm = (list= ls())
setwd("F:\\OMSA\\FA23-MGT6203\\Group Project\\Data")

df_picks <- read.csv("picks.csv",header=TRUE)
df_round1 <- read.csv("round_1.csv",header=TRUE)
df_round23 <- read.csv("round_23.csv",header=TRUE)
df_round4567 <- read.csv("round_4567.csv",header=TRUE)
nfl_abrev <- read.csv("nlf_teams.csv",header=TRUE)
stacked_round_df <- bind_rows(
  select(df_round1, -ends_with("_y")),
  select(df_round23, -ends_with("_y")),
  select(df_round4567, -ends_with("_y"))
)

head(stacked_round_df)

tail(stacked_round_df)


all_timestamps <- c(
  unique(df_picks$timestamp),
  unique(df_round1$timestamp),
  unique(df_round23$timestamp),
  unique(df_round4567$timestamp)
)

draft_data <- data.frame(timestamp = unique(all_timestamps))
draft_data <- draft_data %>%
  full_join(df_picks, by = "timestamp") %>%
  full_join(stacked_round_df, by = "timestamp")

rows_draftdata<-nrow(draft_data)
rows_draftdata

#convert all text to lower
draft_data$text <- tolower(draft_data$text)

#split the names into first and last
name_split <- strsplit(draft_data$full_name, " ")

# apply the name split function on the draft data
draft_data$first_name <- sapply(name_split, function(x) x[1])
draft_data$last_name <- sapply(name_split, function(x) x[2])

#now convert the names to lower case
draft_data$first_name <- tolower(draft_data$first_name)
draft_data$last_name <- tolower(draft_data$last_name)
draft_data$team.x <- tolower(draft_data$team.x)

# converting the nfl names to lowercase?
nfl_abrev$Abbreviation <- tolower(nfl_abrev$Abbreviation)

head(nfl_abrev)

match_index <- match(draft_data$team.x, nfl_abrev$Abbreviation)

match_index

draft_data$team.x <- nfl_abrev$Name[match_index]

draft_data$City <- tolower(sub(" .*", "", draft_data$team.x))
draft_data$Name <- tolower(sub("^[^ ]+ ", "", draft_data$team.x))

draft_data$contains_lastname <- sapply(1:nrow(draft_data), function(row) {
  result <- grepl(draft_data$last_name[row], draft_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

draft_data$contains_city <- sapply(1:nrow(draft_data), function(row) {
  result <- grepl(draft_data$City[row], draft_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

draft_data$contains_team_name <- sapply(1:nrow(draft_data), function(row) {
  result <- grepl(draft_data$Name[row], draft_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})

bag_of_words <- c(
  "quarterback", " qb ",
  "running back", " rb ",
  "wide receiver", " wr ",
  "tight end", " te ",
  "offensive tackle", " ot ",
  "offensive guard", " og ",
  "center", " c ",
  "defensive tackle", " dt ",
  "defensive end", " de ",
  "linebacker", " lb ",
  "cornerback", " cb ",
  "safety", " s ",
  "kicker", " k ",
  "punter", " p ",
  "fullback", " fb ",
  "return specialist", " rs "
)
head(draft_data, n = 5)

# Get the number of rows in the data frame
draft_rows <- nrow(draft_data)

# Get the number of columns in the data frame
draft_cols <- ncol(draft_data)

# Print the results
cat("Number of rows:", draft_rows, "\n")
cat("Number of columns:", draft_cols, "\n")
View(draft_data)

# Remove rows with NA values in 'name' or 'text'
filtered_data0 <- draft_data[complete.cases(draft_data[c("name", "text")]), ]

# Show the resulting data frame
View(filtered_data0)

#Columns removed with wrong player names in text. 

# Create a new data frame with rows that match the condition
library(stringr)

filtered_data0$contains_player_name <- str_detect(filtered_data0$text, paste(filtered_data0$name, collapse = "|"))
filtered_data1 <- filtered_data0[filtered_data0$contains_player_name & !str_detect(filtered_data0$text, filtered_data0$name), ]


# Get the number of rows in the data frame
filt_rows <- nrow(filtered_data1)

# Get the number of columns in the data frame
filt_cols <- ncol(filtered_data1)

# Print the results
cat("Number of rows:", filt_rows, "\n")
cat("Number of columns:", filt_cols, "\n")

# Show the resulting data frame
View(filtered_data1)

## Pablo code to check relevant info (did not run in my PC)
draft_data$contains_position <- sapply(1:nrow(draft_data), function(row) {
  result <- grepl(paste(bag_of_words, collapse = "|"), draft_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})


draft_data$contains_relevant_info <- rowSums(draft_data[c("contains_lastname", "contains_city", "contains_team_name","contains_firstname","contains_position")]) > 0


clean_data<- draft_data[draft_data$contains_relevant_info, c("last_name","City", "Name", "sentiment", "text")]

head(clean_data, n = 5)

View(clean_data)

rows_filtered<-nrow(clean_data)

rows_filtered / rows_draftdata



