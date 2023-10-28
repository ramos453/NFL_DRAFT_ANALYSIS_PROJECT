library(tidyverse)
library(ggplot2)

#Load in the data
#Data comes from Stathead draft seasons
draft_2012 <- read.csv('2012Draft.txt')
draft_2013 <- read.csv('2013Draft.txt')
draft_2014 <- read.csv('2014Draft.txt')
draft_2015 <- read.csv('2015Draft.txt')
draft_2016 <- read.csv('2016Draft.txt')
draft_2017 <- read.csv('2017Draft.txt')
draft_2018 <- read.csv('2018Draft.txt')
draft_2019 <- read.csv('2019Draft.txt')
draft_2020 <- read.csv('2020Draft.txt')

#Create a list of all the drafts
df_list <- list(draft_2012, draft_2013, draft_2014, draft_2015, draft_2016, 
                draft_2017, draft_2018, draft_2019, draft_2020)

#Make one final data frame of all picks
final_df <- bind_rows(df_list)

#Find the unique Positions in the data frame
unique_pos <- unique(final_df$Pos)

#Position Function 
position_data <- function(df, pos){
  
  #Separate all of the pos players out
  pos_df <- df[which(df$Pos == pos),]
  
  #Sort by Pick number
  pos_df <- arrange(pos_df, Pick)
  
  #Mean Pick Number
  pos.mean_pick <- round(mean(pos_df$Pick))
  
  #Plot the Density of the Position Picks
  pos_plot <- pos_df %>%
    ggplot(aes(Pick)) + 
    geom_histogram(aes(y = after_stat(density)), fill = '#0014ad', color = '#0014ad',
                   alpha = 0.8, binwidth = 20) + 
    geom_density(fill = '#0014ad', color = '#0014ad', alpha = 0.2) +
    ggtitle(paste(pos, 'Pick Density')) + 
    xlab('Pick Number') + 
    ylab("Density of Picks") + 
    theme(plot.title = element_text(hjust = 0.5))
  show(pos_plot)
  
  list("Position" = pos, "Mean_Pick" = pos.mean_pick, 'Total_Picks' = length(pos_df[,1]))
  
}

#Empty dataframe to present all the data
pos_draft = data.frame(Position=character(), Mean_Pick=integer(), Total_Picks=integer())

#Loop through all of the unique positions and run position_data function
for (i in 1:length(unique_pos)){
  pos <- unique_pos[i]
  out <- position_data(final_df, pos)
  pos_draft[i,] <- c(out$Position, as.integer(out$`Mean_Pick`), as.integer(out$Total_Picks))
}

#Convert all of the draft position data to ints 
pos_draft[,2] <- as.integer(pos_draft$Mean_Pick)
pos_draft[,3] <- as.integer(pos_draft$Total_Picks)
