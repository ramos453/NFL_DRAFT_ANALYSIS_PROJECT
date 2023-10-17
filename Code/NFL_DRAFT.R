

rm = (list= ls())


setwd(r"{E:\OMSA\Semesters\01_Fall_23\MGMT 6203\Group Project\Code}")

picks <- read.csv("picks.csv")

head(picks)

round_1 <- read.csv("round_1.csv")
round_23 <- read.csv("round_23.csv")
round_4567 <- read.csv("round_4567.csv")
nfl_abrev <- read.csv("nfl_teams.csv")

head(picks)
head(round_1)
head(round_23)
head(round_4567)
head(nfl_abrev)

timestamps_rounds <- unique(c(round_1$timestamp, round_23$timestamp, round_4567$timestamp))

picks$in_rounds <- picks$timestamp %in% timestamps_rounds

picks$in_rounds

timestamps_without_matching_info <- picks$timestamp[!picks$in_rounds]
timestamps_without_matching_info


all_rounds <- rbind(round_1, round_23, round_4567)


common_timestamps <- intersect(picks$timestamp, all_rounds$timestamp)

draft_data <- merge(picks[picks$timestamp %in% common_timestamps, ], all_rounds, by = "timestamp")

tail(draft_data$text, 20)

tail(draft_data)
 
sample(draft_data$text, 20)



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





