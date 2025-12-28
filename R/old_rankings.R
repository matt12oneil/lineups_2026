library(tidyverse)
library(dplyr)
library(tidytable)
library(janitor)
library(stringi)
library(scales)
library(DT)

ud_adp <- read_csv('Inputs/adp.csv') |>
  clean_names() |>
  mutate(full_name = paste(first_name, last_name)) |>
  mutate(full_name = stri_trans_general(str = full_name, 
                                        id = "Latin-ASCII")) |>
  filter(adp != '-')

player_mapping <- read_csv('https://www.smartfantasybaseball.com/PLAYERIDMAPCSV') |>
  clean_names() |>
  select(idfangraphs, playername) |>
  mutate(playername = case_when(idfangraphs == 20454 ~ 'Jazz Chisholm Jr.' 
                                , idfangraphs == 25931 ~ 'Michael Harris'
                                , idfangraphs == 20043 ~ 'Luis Robert Jr.'
                                , idfangraphs == 26368 ~ 'JJ Bleday'
                                , idfangraphs == 19566 ~ 'Nathaniel Lowe'
                                , idfangraphs == 26365 ~ 'Jonny Deluca'
                                , idfangraphs == 15440 ~ 'Matthew Boyd'
                                , idfangraphs == 18401 ~ 'Ronald Acuna Jr.'
                                , TRUE ~ playername))

batter_projections <- read_csv(file = 'Inputs/steamer_batters.csv') |>
  clean_names() |>
  left_join(player_mapping, by = c('player_id' = 'idfangraphs')) |>
  distinct() |> 
  mutate(ud_points = 3*x1b + 6*x2b + 8*x3b + 10*hr + 3*bb + 3*ibb + 3*hbp + 3*r + 3*rbi + 4*sb) |>
  group_by(player_id) |>
  mutate(multiple = runif(1,min=.95,max = 1.05)) |>
  mutate(ud_points = multiple*ud_points) |>
  ungroup() |>
  select(-adp,-multiple)

pitcher_projections <- read_csv(file = 'Inputs/steamer_pitchers.csv') |>
  clean_names() |>
  left_join(player_mapping, by = c('player_id' = 'idfangraphs')) |>
  distinct() |> 
  mutate(ud_points = 2*w + 3 *qs + so + ip - er) |>
  group_by(player_id) |>
  mutate(multiple = runif(1,min=.95,max = 1.05)) |>
  mutate(ud_points = multiple*ud_points) |>
  ungroup() |>
  select(-adp,-multiple)

batters <- ud_adp |>
  filter(slot_name %in% c('IF','OF')) |>
  left_join(batter_projections, by = c('full_name' = 'playername')) |>
  select(id, full_name, adp, projected_points, ud_points, position_rank, slot_name, g, pa, x1b,x2b,x3b, hr,bb,ibb, hbp, r, rbi, sb, avg, bb_percent, k_percent, obp,slg,ops, w_oba,w_rc) |>
  rename(adp_pos_rank = position_rank) |>
  group_by(slot_name) |>
  mutate(ud_proj_pos_rank = rank(-projected_points)
         , steamer_proj_pos_rank = rank(-ud_points)
         , adp = as.numeric(adp)) |>
  select(id, full_name, adp, projected_points, ud_points, slot_name, adp_pos_rank, ud_proj_pos_rank, steamer_proj_pos_rank) |>
  ungroup() |>
  mutate(ud_pos_rank = rank(as.numeric(adp))
         , steamer_proj_batter_rank = rank(-ud_points)) |>
  mutate(adp_pos_rank = as.numeric(str_sub(adp_pos_rank, 3)))

pitchers <- ud_adp |>
  filter(slot_name %in% c('P')) |>
  left_join(pitcher_projections, by = c('full_name' = 'playername')) |>
  select(id, full_name, adp, projected_points, ud_points, position_rank, slot_name, ip, w, l, qs, er, so, k_9, bb_9, k_bb, era) |>
  filter(!is.na(ip)) |>
  rename(adp_pos_rank = position_rank) |>
  group_by(slot_name) |>
  mutate(ud_proj_pos_rank = rank(-projected_points)
         , steamer_proj_pos_rank = rank(-ud_points)
         , adp = as.numeric(adp)) |>
  select(id, full_name, adp, projected_points, ud_points, slot_name, adp_pos_rank, ud_proj_pos_rank, steamer_proj_pos_rank) |>
  ungroup() |>
  mutate(ud_pos_rank = rank(as.numeric(adp))
         , steamer_proj_pitcher_rank = rank(-ud_points)) |>
  mutate(adp_pos_rank = as.numeric(str_sub(adp_pos_rank, 3)))

batters_steamer_likes_more <- batters |>
  select(id, full_name, slot_name, steamer_proj_batter_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_proj_batter_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  filter(rank_difference >= .15) |>
  arrange(ud_pos_rank)

pitchers_steamer_likes_more <- pitchers |>
  select(id, full_name, slot_name, steamer_proj_pitcher_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_proj_pitcher_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  filter(rank_difference >= .15) |>
  arrange(ud_pos_rank)

batters_steamer_likes_less <- batters |>
  select(id, full_name, slot_name, steamer_proj_batter_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_proj_batter_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  filter(rank_difference <= -.15) |>
  arrange(ud_pos_rank)

pitchers_steamer_likes_less <- pitchers |>
  select(id, full_name, slot_name, steamer_proj_pitcher_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_proj_pitcher_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  filter(rank_difference <= -.15) |>
  arrange(ud_pos_rank)

batter_proj_vs_adp <- batters |>
  select(id, full_name, slot_name, adp, steamer_rank = steamer_proj_batter_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(adp = as.numeric(adp)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  arrange(ud_pos_rank)

pitcher_proj_vs_adp <- pitchers |>
  select(id, full_name, slot_name, adp, steamer_rank = steamer_proj_pitcher_rank, ud_pos_rank) |>
  mutate(ud_pos_rank = as.numeric(ud_pos_rank)) |>
  mutate(adp = as.numeric(adp)) |>
  mutate(rank_difference = (ud_pos_rank - steamer_rank)/ud_pos_rank) |>
  arrange(-rank_difference) |>
  arrange(ud_pos_rank)



all_adp <- pitcher_proj_vs_adp |>
  bind_rows(batter_proj_vs_adp) |>
  arrange(adp) |>
  mutate(rank_pct = scales::percent(round(rank_difference,1)))

color_gradient <- function(dt, column_name, gradient_colors = c("springgreen3",'springgreen2','springgreen1','springgreen','tomato1','tomato2','tomato3','tomato4')) {
  col_func <- colorRampPalette(gradient_colors)
  dt %>% 
    formatStyle(column_name, 
                backgroundColor = styleEqual(
                  sort(unique(dt$x$data[[column_name]]), decreasing = TRUE),
                  col_func(length(unique(dt$x$data[[column_name]])))
                )
    ) 
}


all_adp |>
  select(-id) |>
  #select(rank_difference) |>
  #mutate(rank_difference = rank_pct) |>
  select(-rank_pct) |>
  #select(-steamer_rank, -ud_pos_rank) |>
  DT::datatable(filter = 'top',colnames = c('Name','Position','ADP','Steamer ADP','UD ADP','Value'), options = list(
    pageLength = 24, 
    extensions = list(Scroller=list()),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'black', 'color': 'white'});",
      "}"),
    scrollY=200,
    scrollCollapse = TRUE
  )) |>
  formatStyle(
    'rank_difference',
    backgroundColor = styleInterval(c(-.375,-.225,-.075,.075,.225,.375), c('red','darksalmon', 'lightcoral','white', 'lightgreen','springgreen','green')),
    fontWeight = 'bold'
  ) |>
  formatPercentage('rank_difference', 0) 