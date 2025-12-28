library(tidyverse)
library(janitor)
library(baseballr)

#all previous year stats
#set up via Github Actions
#join with mlbplotr
#save png files to computer
#see if we can get final adp and my own exposure
#bring in position labels for IF/OF
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


#need to add UD positions to be able to rank over

weekly_batter_scores <- weekly_batter |>
  clean_names() |>
  select(name, bbref_id, week, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(ud_points = 3*x1b + 6*x2b + 8*x3b + 10*hr + 3*bb + 3*hbp + 2*r + 2*rbi + 4*sb) |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points)) |>
  ungroup() |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'H1'
                                , week_rank <= 24 ~ 'H2'
                                , week_rank <= 36 ~ 'H3'
                                , week_rank <= 48 ~ 'H4'
                                , week_rank <= 60 ~ 'H5'
                                , week_rank <= 72 ~ 'H6'
                                , week_rank <= 84 ~ 'H7'
                                ,.default = 'Unusable')) 


season_batter_scores <- weekly_batter_scores |>
  group_by(name, bbref_id, rank_group) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = H1 + H2 + H3 + H4 + H5 + H6 + H7) |>
  select(name, bbref_id, H1, H2, H3, H4, H5, H6, H7, Unusable, usable_points) |>
  arrange(desc(usable_points))

