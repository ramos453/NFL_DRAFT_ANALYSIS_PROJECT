
#Create new df for testing purposes, can be used on the combined_df
images_videos_df <- combined_df

images_videos_df$sentiment[grepl("https", images_videos_df$text, ignore.case = TRUE)] <- NA

View(images_videos_df)

distinct_count <- images_videos_df %>%
  filter(!is.na(full_name)) %>%
  distinct(full_name)

View(distinct_count)

