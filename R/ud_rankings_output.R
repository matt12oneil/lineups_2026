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

#switch to use position, not just batter rank for hitters
#display points in final table

ud_adp <- read_csv('Inputs/rankings-c9ca3e08-7ceb-4491-8ebd-db1725d95fdd-bf747c72-4183-4d04-90e2-485b14cb077a.csv') |>
  clean_names() |>
  mutate(full_name = paste(first_name, last_name)) |>
  mutate(full_name = stri_trans_general(str = full_name, 
                                        id = "Latin-ASCII")) |>
  #filter(adp != '-') |>
  select(-c(bye_week, lineup_status)) |>
  mutate(adp = as.numeric(adp))

player_mapping <- read_csv('https://www.smartfantasybaseball.com/PLAYERIDMAPCSV') |>
  clean_names() |>
  select(idfangraphs, playername) |>
  mutate(playername = stri_trans_general(str = playername, 
                                         id = "Latin-ASCII")) |>
  mutate(playername = case_when(idfangraphs == 20454 ~ 'Jazz Chisholm Jr.' 
                                , idfangraphs == 18401 ~ 'Ronald Acuna Jr.'
                                , idfangraphs == 20043 ~ 'Luis Robert Jr.'
                                , idfangraphs == 26368 ~ 'JJ Bleday'
                                , idfangraphs == 19566 ~ 'Nathaniel Lowe'
                                #, idfangraphs == 26365 ~ 'Jonny Deluca'
                                , idfangraphs == 15440 ~ 'Matthew Boyd'
                                , idfangraphs == 25931 ~ 'Michael Harris II'
                                , TRUE ~ playername))

steamer_multiple <- runif(1,min=0,max = 1)*2
depth_charts_multiple <- 2-steamer_multiple

steamer_batters <- read_csv(file = 'Inputs/steamer_batters.csv') |>
  clean_names() |>
  filter(pa >= 50) |>
  mutate(full_name = stri_trans_general(str = name, 
                                        id = "Latin-ASCII")) |>
  mutate(ud_points = 3*x1b + 6*x2b + 8*x3b + 10*hr + 3*bb + 3*ibb + 3*hbp + 3*r + 3*rbi + 4*sb) |>
  mutate(ppa = ud_points/pa, ppa_mean = mean(ppa), ppa_sd = sd(ppa)/sqrt(n())) |>
  select(name_ascii, player_id, team, pa, ud_points, ppa, ppa_mean, ppa_sd) |>
  group_by(name_ascii, team) |>
  mutate(ppa_rand = rnorm(1,mean = ppa, sd = ppa_sd), points = ppa_rand*pa) |>
  ungroup() |>
  mutate(proj_rank = rank(-ud_points), rand_rank = rank(-points)) |>
  arrange(desc(ud_points)) |>
  mutate(points = points*steamer_multiple) |>
  rename(name = name_ascii)

depth_charts_batters <- read_csv(file = 'Inputs/depth_charts_batters.csv') |>
  clean_names() |>
  mutate(full_name = stri_trans_general(str = name, 
                                        id = "Latin-ASCII")) |>
  mutate(ud_points = 3*x1b + 6*x2b + 8*x3b + 10*hr + 3*bb + 3*ibb + 3*hbp + 2*r + 2*rbi + 4*sb) |>
  mutate(ppa = ud_points/pa, ppa_mean = mean(ppa), ppa_sd = sd(ppa)) |>
  select(name_ascii, player_id, team, pa, ud_points, ppa, ppa_mean, ppa_sd) |>
  group_by(name_ascii, team) |>
  mutate(ppa_rand = rnorm(1,mean = ppa, sd = ppa_sd), points = ppa_rand*pa) |>
  ungroup() |>
  mutate(proj_rank = rank(-ud_points), rand_rank = rank(-points)) |>
  arrange(desc(ud_points)) |>
  mutate(points = points*depth_charts_multiple) |>
  rename(name = name_ascii)
  



steamer_pitchers <- read_csv(file = 'Inputs/steamer_pitchers.csv') |>
  clean_names() |>
  filter(ip >= 50) |>
  mutate(full_name = stri_trans_general(str = name, 
                                        id = "Latin-ASCII")) |>
  mutate(ud_points = 5*w + 5*qs + 3*so + 3*ip - 3*er) |>
  mutate(ppg = ud_points/g, ppg_mean = mean(ppg), ppg_sd = sd(ppg)/sqrt(n())) |>
  select(name_ascii, player_id, team, g, ud_points, ppg, ppg_mean, ppg_sd) |>
  group_by(name_ascii, team) |>
  mutate(ppg_rand = rnorm(1,mean = ppg, sd = ppg_sd), points = ppg_rand*g) |>
  ungroup() |>
  mutate(proj_rank = rank(-ud_points), rand_rank = rank(-points)) |>
  arrange(desc(ud_points)) |>
  mutate(points = points*steamer_multiple) |>
  rename(name = name_ascii)

depth_charts_pitchers <- read_csv(file = 'Inputs/depth_charts_pitchers.csv') |>
  clean_names() |>
  filter(ip >= 50) |>
  mutate(full_name = stri_trans_general(str = name, 
                                        id = "Latin-ASCII")) |>
  mutate(ud_points = 5*w + 5*qs + 3*so + 3*ip - 3*er) |>
  mutate(ppg = ud_points/g, ppg_mean = mean(ppg), ppg_sd = sd(ppg)/sqrt(n())) |>
  select(name_ascii, player_id, team, g, ud_points, ppg, ppg_mean, ppg_sd) |>
  group_by(name_ascii, team) |>
  mutate(ppg_rand = rnorm(1,mean = ppg, sd = ppg_sd), points = ppg_rand*g) |>
  ungroup() |>
  mutate(proj_rank = rank(-ud_points), rand_rank = rank(-points)) |>
  arrange(desc(ud_points)) |>
  mutate(points = points*depth_charts_multiple) |>
  rename(name = name_ascii)

batter_proj <- steamer_batters |>
  bind_rows(depth_charts_batters) |>
  group_by(name, player_id, team) |>
  summarize(points = sum(points)/2) |>
  arrange(desc(points)) |> 
  left_join(player_mapping, by = c('player_id' = 'idfangraphs')) |>
  distinct() |>
  ungroup()

pitcher_proj <- steamer_pitchers |>
  bind_rows(depth_charts_pitchers) |>
  group_by(name, player_id, team) |>
  summarize(points = sum(points)/2) |>
  arrange(desc(points)) |> 
  left_join(player_mapping, by = c('player_id' = 'idfangraphs')) |>
  distinct() |>
  ungroup()

proj <- batter_proj |>
  bind_rows(pitcher_proj) |>
  filter(points > 100) |>
  dplyr::inner_join(ud_adp, by = join_by(playername == full_name), relationship = "many-to-many") |>
  rename(position = slot_name) |>
  group_by(id, name, player_id, team, adp, position) |>
  summarize(points = round(max(points))) |>
  ungroup() |>
  group_by(position) |>
  mutate(adp_rank = rank(adp)) |>
  mutate(points_rank = round(rank(-points))) |>
  ungroup() |>
  arrange(adp) |>
  mutate(team = if_else(is.na(team),'FA',team)) |>
  select(-player_id) |>
  mutate(value = round(adp_rank/((adp_rank + 2*points_rank)/3),1)) |>
  filter(!is.na(value)) |>
  mutate(value_pct = ((adp_rank + 2*points_rank)/3)) |>
  mutate(value_pct = (adp_rank - value_pct)/adp_rank) |>
  mutate(value = case_when(value <= .7 ~ 1, 
                           value < 1 ~ 2,
                           value == 1 ~ 3,
                           value <= 1.5 ~ 4,
                           value <= 2.5 ~ 5,
                           value > 2.5 ~ 6,
                           .default = 3)) |>
  select(id, name, team, position, value, value_pct, adp_rank, points_rank, adp, points) |>
  group_by(position) |>
  mutate(rp = case_when(points_rank == 36 ~ points, .default = 0)) |>
  mutate(rp =  max(rp)) |>
  ungroup() |>
  mutate(vorp = points - rp) |>
  arrange(vorp) |>
  mutate(vorp = round(rank(-vorp))) |>
  arrange(vorp) |>
  select(id, name, team, position) |>
  filter(!(name == 'Will Smith' & position == 'P') & !(name == 'Luis Garcia') & !(name == 'Luis Garcia Jr.' & position == 'P'))

write_csv(proj, "Outputs/ud_ranks.csv")

