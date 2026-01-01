library(tidyverse)
library(janitor)
library(baseballr)
library(furrr)
library(stringi)
library(gt)
library(gtExtras)
library(mlbplotR)

#set up via Github Actions
#join with mlbplotr
#save png files to computer
#see if we can get my own exposure
#try to calculate total points by team YTD and see if we can get advance rates by player
#VORP by usable scores?
#look for weekly consistency
#comparison of players picked within a few picks of each other
#compare where in the draft you get usable points from by position
#comment all code
#set up new files for drag and drop when we get 2026 data

draft_data <- read_csv('https://storage.googleapis.com/underdog-inc/underblog/2025_Dinger/the_dinger_rd1.csv') |>
  mutate(position_name = case_when(position_name %in% c('SP','RP') ~ 'P'
                                   , position_name %in% c('DH','LF','RF','CF') ~ 'OF'
                                   , .default = 'IF')) |>
  arrange(draft_created_time) |>
  group_by(player_id) |>
  mutate(final_adp = last(projection_adp)) |>
  distinct(player_name, position_name, final_adp) |>
  arrange(final_adp) |>
  mutate(player_name = stri_trans_general(str = player_name, 
                                          id = "Latin-ASCII"))

fg_id <- read_csv('https://www.smartfantasybaseball.com/PLAYERIDMAPCSV') |>
  clean_names() |>
  filter(pos != 'P' & active == 'Y') |>
  select(idfangraphs, mlbid)



batter_gamelogs <- function(fangraph_id){
  game_logs <- fg_batter_game_logs(fangraph_id, year = 2025)
  return(game_logs)
}

#need to add UD positions to be able to rank over


weekly_batter <- future_map_dfr(
  .x = fg_id$idfangraphs
  ,.f = batter_gamelogs
)


#need to add UD positions to be able to rank over

batter_weekly_scores <- weekly_batter |>
  clean_names() |>
  select(name = player_name, playerid, date, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(ud_points = 3*x1b + 6*x2b + 8*x3b + 10*hr + 3*bb + 3*hbp + 2*r + 2*rbi + 4*sb) |>
  mutate(week = floor_date(as.Date(date), unit = 'week', week_start = 1)) |>
  group_by(name, playerid, week) |>
  summarize(x1b = sum(x1b), x2b = sum(x2b), x3b = sum(x3b), hr = sum(hr), bb = sum(bb), hbp = sum(hbp), r = sum(r), rbi = sum(rbi), sb = sum(sb), ud_points = sum(ud_points)) |>
  ungroup() |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points, ties.method = 'min')) |>
  ungroup() |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'H1'
                                , week_rank <= 24 ~ 'H2'
                                , week_rank <= 36 ~ 'H3'
                                , week_rank <= 48 ~ 'H4'
                                , week_rank <= 60 ~ 'H5'
                                , week_rank <= 72 ~ 'H6'
                                , week_rank <= 84 ~ 'H7'
                                ,.default = 'Unusable')) 


batter_season_scores <- batter_weekly_scores |>
  group_by(name, playerid, rank_group) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = H1 + H2 + H3 + H4 + H5 + H6 + H7) |>
  select(name, playerid, H1, H2, H3, H4, H5, H6, H7, Unusable, usable_points) |>
  arrange(desc(usable_points))

#weekly scoring with final adp
#can adjust to get rid of columns we don't need
batter_weekly_scoring <- draft_data |>
  filter(position_name != 'P') |>
  inner_join(weekly_batter_scores, by = c('player_name' = 'name'), relationship = 'many-to-many') |>
  select(player_name, playerid, playerid, player_id, week, final_adp, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb, ud_points) |>
  arrange(desc(ud_points)) |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points, ties.method = 'min')) |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'H1'
                                , week_rank <= 24 ~ 'H2'
                                , week_rank <= 36 ~ 'H3'
                                , week_rank <= 48 ~ 'H4'
                                , week_rank <= 60 ~ 'H5'
                                , week_rank <= 72 ~ 'H6'
                                , week_rank <= 84 ~ 'H7'
                                ,.default = 'Unusable')) |>
  ungroup() |>
  mutate(playerid = as.character(playerid)) |>
  left_join(fg_id, by = c('playerid' = 'idfangraphs')) |>
  select(player_name, mlbid, final_adp, week, final_adp, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb, ud_points, week_rank, rank_group)

#season cumulative scoring with final adp
#can adjust to get rid of columns we don't need
batter_season_scoring <- batter_weekly_scoring |>
  group_by(player_name, mlbid, rank_group, final_adp) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = H1 + H2 + H3 + H4 + H5 + H6 + H7) |>
  select(player_name, mlbid, final_adp, H1, H2, H3, H4, H5, H6, H7, Unusable, usable_points) |>
  arrange(desc(usable_points)) |>
  mutate(usable_pct = usable_points/(Unusable + usable_points))

#posting ideas
#add headshots
#can monitor for consistency
#individual pitcher pulls
#comparison of pitchers picked within a few picks
#VORP

data.frame(
  player_name = c(
    "Nathan Eovaldi", "Marcus Semien", "Corey Seager", "Jacob deGrom", "Chris Martin"
  ),
  savant_id1 = c(543135, 543760, 608369, 594798, 455119),
  savant_id2 = c(543135, 543760, 608369, 594798, 455119)
) %>%
  gt::gt() %>%
  gt_fmt_mlb_headshot(columns = "savant_id1") %>%
  gt_fmt_mlb_dot_headshot(columns = "savant_id2")
