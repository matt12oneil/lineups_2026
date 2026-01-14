if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github(repo = "BillPetti/baseballr")

library(tidyverse)
library(janitor)
library(baseballr)
library(furrr)
library(stringi)
library(gt)
library(gtExtras)
library(DT)
library(scales)
library(mlbplotR)

#set up via Github Actions
#join with mlbplotr
#save png files to computer
#see if we can get my own exposure
#try to calculate total points by team YTD and see if we can get advance rates by player
#VORP by usable scores?

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
  filter(pos == 'P' & active == 'Y') |>
  select(idfangraphs, mlbid)


  
pitcher_gamelogs <- function(fangraph_id){
  game_logs <- fg_pitcher_game_logs(fangraph_id, year = 2025)
  return(game_logs)
}

#need to add UD positions to be able to rank over


weekly_pitcher <- future_map_dfr(
  .x = fg_id$idfangraphs
  ,.f = pitcher_gamelogs
)

pitcher_weekly_scores <- weekly_pitcher |>
  clean_names() |>
  filter(gs > 0) |>
  select(name = player_name, playerid, date, g, gs, w, so, ip, er) |>
  #mutate(ip = as.character(ip)) |>
  separate_wider_delim(cols = ip, delim = ".", names = c('full_innings','partial_innings')
                       ,cols_remove =  FALSE, too_few = "align_start") |>
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
  mutate(week_rank = rank(-ud_points, ties.method = 'min')) |>
  ungroup() |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'P1'
                                , week_rank <= 24 ~ 'P2'
                                , week_rank <= 36 ~ 'P3'
                                ,.default = 'Unusable')) 


pitcher_season_scores <- pitcher_weekly_scores |>
  group_by(name, playerid, rank_group) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = P1 + P2 + P3) |>
  select(name, playerid, P1, P2, P3, Unusable, usable_points) |>
  arrange(desc(usable_points))

#weekly scoring with final adp
#can adjust to get rid of columns we don't need
p_weekly_scoring <- draft_data |>
  filter(position_name == 'P') |>
  inner_join(pitcher_weekly_scores, by = c('player_name' = 'name'), relationship = 'many-to-many') |>
  select(player_name, playerid, player_id, week, final_adp, w, qs, so, outs, er, ud_points) |>
  arrange(desc(ud_points)) |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points, ties.method = 'min')) |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'P1'
                                , week_rank <= 24 ~ 'P2'
                                , week_rank <= 36 ~ 'P3'
                                ,.default = 'Unusable')) |>
  ungroup() |>
  mutate(playerid = as.character(playerid)) |>
  left_join(fg_id, by = c('playerid' = 'idfangraphs')) |>
  select(player_name, mlbid, final_adp, week, final_adp, w,qs,outs,so,er, ud_points, week_rank, rank_group)

p_season_scoring <- p_weekly_scoring |>
  group_by(player_name, mlbid, rank_group, final_adp) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = P1 + P2 + P3) |>
  select(player_name, mlbid, final_adp, P1, P2, P3, Unusable, usable_points) |>
  arrange(desc(usable_points)) |>
  mutate(usable_pct = usable_points/(Unusable + usable_points))

#posting ideas
#add headshots
#can monitor for consistency
#individual pitcher pulls
#comparison of pitchers picked within a few picks
#VORP

p_season_scoring |>
  head(10) |>
  gt() |>
  gt_fmt_mlb_headshot(columns = "mlbid") |>
  cols_label(
    player_name = "Name",
    mlbid = " ",
    final_adp = "ADP",
    usable_points = "Usable Points",
    usable_pct = "Usable%"
  ) |>
  opt_row_striping() |>
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(3)),
    locations = cells_body(
      columns = c(mlbid)
    )
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), 
                         color = "black", weight = px(3)),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(3)),
    locations = cells_body(rows = 10)
  ) |>
  tab_header(
    title = "Underdog MLB Scoring: 2024 Pitchers",
    subtitle = "Usable Points is Anything in Weekly Top 36"
  ) |>
  tab_footnote(
    footnote = "Figure by @solvedbywalking | Data from baseballr package | ADP data from Underdog"
  ) |>
  fmt_percent(
    columns = usable_pct,
    rows = everything(),
    decimals = 1
  ) |>
  tab_options(
    heading.background.color = '#54796d'
    , footnotes.background.color = '#54796d'
    , column_labels.background.color = '#D3D3D3'
  ) |>
  gtsave(filename = "Outputs/TopTenP.png")
  
write_csv(p_weekly_scoring, "Outputs/p_weekly_scoring_2025.csv")
