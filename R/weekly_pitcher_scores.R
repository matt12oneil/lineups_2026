library(tidyverse)
library(janitor)
library(baseballr)

#all previous year stats
#set up via Github Actions
#join with mlbplotr
#save png files to computer
#see if we can get final adp and my own exposure
#try to calculate total points by team YTD and see if we can get advance rates by player

days <- mlb_schedule(season = 2025, level_ids = '1') |>
  filter(series_description == 'Regular Season' & date <= '2025-09-14' & date >='2025-03-31') |>
  distinct(date) |>
  mutate(date = as.Date(date)) |>
  head(10)

weeks <- c('2025-03-31'
           ,'2025-04-07'
           ,'2025-04-14'
           , '2025-04-21'
           ,'2025-04-28'
           ,'2025-05-05'
           ,'2025-05-12'
           ,'2025-05-19'
           ,'2025-05-26'
           ,'2025-06-02'
           ,'2025-06-09'
           ,'2025-06-16'
           ,'2025-06-23'
           ,'2025-06-30'
           ,'2025-07-07'
           ,'2025-07-14'
           ,'2025-07-21'
           ,'2025-07-28'
           ,'2025-08-04'
           ,'2025-08-11'
           ,'2025-08-18'
           ,'2025-08-25'
           ,'2025-09-01'
           ,'2025-09-08'
           )

weeks <- as.Date(weeks)


batter_rollup <- function(week_start){
  week_end = week_start + 6
  week_stats <- bref_daily_batter(week_start, week_end) |>
    mutate(week = week_start)
  return(week_stats)
}

#need to add UD positions to be able to rank over

weekly_batter <- future_map_dfr(
  .x = weeks
  ,.f = batter_rollup
)

fg_id <- chadwick_player_lu() |>
  filter(!is.na(key_fangraphs) & mlb_played_last == 2025) |>
  select(key_fangraphs) |>
  distinct()
  
pitcher_rollup <- function(week_start){
  week_end = week_start + 6
  week_stats <- bref_daily_pitcher(week_start, week_end) |>
    mutate(week = week_start)
  return(week_stats)
}

pitcher_gamelogs <- function(fangraph_id){
  game_logs <- fg_pitcher_game_logs(fangraph_id, year = 2025)
  return(game_logs)
}

#need to add UD positions to be able to rank over

weekly_pitcher <- future_map_dfr(
  .x = fg_id$key_fangraphs
  ,.f = pitcher_gamelogs
)

weekly_pitcher_scores <- weekly_pitcher |>
  clean_names() |>
  filter(gs > 0) |>
  select(name = player_name, playerid, date, g, gs, w, so, ip, er) |>
  separate_wider_delim(cols = ip, delim = ".", names = c('full_innings','partial_innings'),cols_remove =  FALSE) |>
  mutate(full_innings = as.numeric(full_innings), partial_innings = as.numeric(partial_innings)) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(outs = full_innings*3 + partial_innings) |>
  mutate(qs = case_when(gs == 1 & g == 1 & ip >= 6 & er <= 3 ~ 1, .default = 0)) |>
  mutate(ud_points = 5*w + 5*qs + 3*so + 1*outs - 3*er) |>
  mutate(week = floor_date(as.Date(date), unit = 'week', week_start = 1)) |>
  group_by(name, playerid, week) |>
  #look to use across here instead of summing each individually
  summarize(w = sum(w), qs = sum(qs), so = sum(so), outs = sum(outs), er = sum(er), ud_points = sum(ud_points)) |>
  ungroup() |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points)) |>
  ungroup() |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'P1'
                                , week_rank <= 24 ~ 'P2'
                                , week_rank <= 36 ~ 'P3'
                                ,.default = 'Unusable')) 


season_pitcher_scores <- weekly_pitcher_scores |>
  group_by(name, playerid, rank_group) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = P1 + P2 + P3) |>
  select(name, playerid, P1, P2, P3, Unusable, usable_points) |>
  arrange(desc(usable_points))

