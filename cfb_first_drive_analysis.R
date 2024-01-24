if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load(dplyr,tidyr)
pacman::p_load_current_gh("sportsdataverse/cfbfastR")

year_vector <- 2022
week_vector <- 1:12
weekly_year_df <- expand.grid(year = year_vector, week = week_vector)
tictoc::tic()
year_split <- split(weekly_year_df, weekly_year_df$year)
for (i in 1:length(year_split)) {
  i <- 1
  progressr::with_progress({
    year_split[[i]] <- year_split[[i]] %>%
      dplyr::mutate(
        pbp = purrr::map2(
          .x = year,
          .y = week,
          cfbd_plays,
          season_type = "both"
        )
      )
    Sys.sleep(1)
  })
}

tictoc::toc()

year_split <- lapply(year_split, function(x) {
  x %>% tidyr::unnest(pbp, names_repair = "minimal")
})

conferences <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12")#, "FBS Independent")
all_years <- dplyr::bind_rows(year_split)
top_10 <- all_years[order(-all_years$yards_gained),]
top_10 <- top_10[top_10$drive_number <= 2,]
top_10 <- top_10[top_10$play_number <= 1,]
top_10 <- top_10[top_10$offense_conference %in% conferences | top_10$offense=="Notre Dame",]
#top_10 <- top_10[top_10$offense=="Notre Dame",]
top_10 <- top_10 %>% group_by(offense)
#top_10 <- top_n(top_10, 20)
avg <- top_10 %>% summarise(avg = mean(yards_gained))
#glimpse(all_years)
top_10 %>% select('year', 'offense', 'yards_gained', 'week', 'defense', 'clock.minutes', 'offense_conference')

avg[order(-avg$avg),]