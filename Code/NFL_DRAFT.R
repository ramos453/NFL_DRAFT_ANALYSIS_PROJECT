

rm = (list= ls())


setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")

library(readxl)
library(dplyr)
library(tidyr)

df_picks <- read.csv("picks.csv",header=TRUE)
df_round1 <- read.csv("round_1.csv",header=TRUE)
df_round23 <- read.csv("round_23.csv",header=TRUE)
df_round4567 <- read.csv("round_4567.csv",header=TRUE)

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


draft_data$text <- tolower(draft_data$text)



name_split <- strsplit(draft_data$full_name, " ")


draft_data$first_name <- sapply(name_split, function(x) x[1])
draft_data$last_name <- sapply(name_split, function(x) x[2])



draft_data$first_name <- tolower(draft_data$first_name)
draft_data$last_name <- tolower(draft_data$last_name)
draft_data$team.x <- tolower(draft_data$team.x)


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

draft_data$contains_position <- sapply(1:nrow(draft_data), function(row) {
  result <- grepl(paste(bag_of_words, collapse = "|"), draft_data$text[row], ignore.case = TRUE)
  if (is.na(result)) FALSE else result
})


draft_data$contains_relevant_info <- rowSums(draft_data[c("contains_lastname", "contains_city", "contains_team_name","contains_firstname","contains_position")]) > 0


clean_data<- draft_data[draft_data$contains_relevant_info, c("last_name","City", "Name", "sentiment", "text")]

View(clean_data)

rows_filtered<-nrow(clean_data)

rows_filtered / rows_draftdata




