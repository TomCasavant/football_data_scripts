season <- try(cfbd_stats_season_team(year = 2022, start_week = 1, end_week = 12))

season %>% select("team", "time_of_poss_total")

season %>% summarise( yps = total_yds / time_of_poss_total)

all_yps <- season %>%
    group_by(team) %>%
    mutate(yps = total_yds / time_of_poss_total) %>%
    ungroup()

all_yps <- all_yps[order(-all_yps$yps),]
result <- all_yps %>% select('team', 'yps', 'total_yds', 'time_of_poss_total')
result$rank <- NA
result$rank[order(-result$time_of_poss_total)] <- 1:nrow(result)
