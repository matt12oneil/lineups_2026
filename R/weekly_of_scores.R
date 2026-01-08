library(tidyverse)
library(janitor)
library(baseballr)
library(furrr)
library(stringi)
library(gt)
library(gtExtras)
library(mlbplotR)

#all previous year stats
#set up via Github Actions
#join with mlbplotr
#save png files to computer
#see if we can get final adp and my own exposure
#bring in position labels for IF/OF
#try to calculate total points by team YTD and see if we can get advance rates by player
#VORP by usable scores?
#set up to match pitcher format

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

weekly_batter_scores <- weekly_batter |>
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


season_batter_scores <- weekly_batter_scores |>
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
of_weekly_scoring <- draft_data |>
  filter(position_name == 'OF') |>
  inner_join(weekly_batter_scores, by = c('player_name' = 'name'), relationship = 'many-to-many') |>
  select(player_name, playerid, player_id, week, final_adp, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb, ud_points) |>
  arrange(desc(ud_points)) |>
  group_by(week) |>
  mutate(week_rank = rank(-ud_points, ties.method = 'min')) |>
  mutate(rank_group = case_when(week_rank <= 12 ~ 'OF1'
                                , week_rank <= 24 ~ 'OF2'
                                , week_rank <= 36 ~ 'OF3'
                                , week_rank <= 48 ~ 'OF4'
                                ,.default = 'Unusable')) |>
  ungroup() |>
  mutate(playerid = as.character(playerid)) |>
  left_join(fg_id, by = c('playerid' = 'idfangraphs')) |>
  select(player_name, mlbid, final_adp, week, final_adp, x1b, x2b, x3b, hr, bb, hbp, r, rbi, sb, ud_points, week_rank, rank_group)

of_season_scoring <- of_weekly_scoring |>
  group_by(player_name, mlbid, rank_group, final_adp) |>
  summarize(ud_points = sum(ud_points))  |>
  ungroup() |>
  pivot_wider(names_from = rank_group, values_from = ud_points) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  mutate(usable_points = OF1 + OF2 + OF3 + OF4) |>
  select(player_name, mlbid, final_adp, OF1, OF2, OF3, OF4, Unusable, usable_points) |>
  arrange(desc(usable_points)) |>
  mutate(usable_pct = usable_points/(Unusable + usable_points))


#posting ideas
#add headshots
#can monitor for consistency
#individual pitcher pulls
#comparison of pitchers picked within a few picks
#VORP


of_season_scoring |>
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
    title = "Underdog MLB Scoring: 2024 Outfielders",
    subtitle = "Usable Points is Anything in Weekly Top 48"
  ) |>
  tab_footnote(
    footnote = "Figure by @solvedbywalking | Data from baseballr package | ADP data from Underdog"
  ) |>
  fmt_percent(
    columns = usable_pct,
    rows = everything(),
    decimals = 1
  ) |>
  gtsave(filename = "Outputs/TopTenOF.png")

write_csv(of_weekly_scoring, "Outputs/of_weekly_scoring_2025.csv")