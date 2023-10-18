```{r pick_matching}
df_temp <- not_in_new_df
df_ts <- not_in_new_df %>%
  select(timestamp, pick, round) %>%
  distinct()

colnames(df_ts)[colnames(df_ts) == "pick"] <- "associated_pick"
colnames(df_ts)[colnames(df_ts) == "round"] <- "associated_round"

n<-10000
associated_timestamps <- rep(NA, nrow(df_temp))

for (i in 1:n) {
  
  current_timestamp <- df_temp$timestamp[i]
  associated_timestamp <- max(df_ts$timestamp[df_ts$timestamp <= current_timestamp])
  associated_timestamps[i] <- associated_timestamp
}
associated_timestamps
df_temp$associated_timestamp <- associated_timestamps

df_temp <- left_join(df_temp,
                     df_ts,
                     by = c("associated_timestamp" = "timestamp"))

View(df_temp)
```
