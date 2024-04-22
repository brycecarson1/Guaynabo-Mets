library(gt)
library(gtExtras)
library(dplyr)

gm <- read.csv("Game7.csv")
totals <- read.csv("lineup_totals.csv")
on_off <- read.csv("on_off.csv")

# for one game

gm <- gm |> 
  select("Lineup", 'Plus_Minus', "Poss_off", 'PPP_off', "Poss_def", 'PPP_def') |> 
  mutate("PPP_off" = round(PPP_off, 2),
         "PPP_def" = round(PPP_def, 2))

totals |> 
  arrange(desc(Poss_off)) |> 
  gt() |> 
  gt_theme_guardian() |> 
  cols_label(Poss_off = "Off Poss",
             Poss_def = "Def Poss",
             Plus_Minus = "Plus Minus",
             PPP_off = "Off Points Per Poss",
             PPP_def = "Def Points Per Poss") |> 
  gt_hulk_col_numeric(Plus_Minus) |> 
  gt_hulk_col_numeric(PPP_off) |> 
  gt_hulk_col_numeric(PPP_def, reverse = TRUE) |> 
  tab_header(title = "Guaynabo Mets Game 7 Stats", 
             subtitle = "Minimum 10 possessions (either offensive or defensive)") |> 
  opt_align_table_header(align = "center") |> 
  cols_width(Plus_Minus ~ px(100),
             Lineup ~ px(400),
             PPP_off ~ px(100),
             PPP_def ~ px(100),
             Poss_off ~ px(100),
             Poss_def ~ px(100)) |> 
  gtsave("gm7_stats.png") 


# for cumulative stats

totals <- totals |> 
  filter(Poss_off >= 10 | Poss_def >= 10) |> 
  select("Lineup", 'Plus_Minus', "Poss_off", 'PPP_off', "Poss_def", 'PPP_def') |> 
  mutate("PPP_off" = round(PPP_off, 2),
         "PPP_def" = round(PPP_def, 2))
  
totals |> 
  arrange(desc(Poss_off)) |> 
  gt() |> 
  gt_theme_guardian() |> 
  cols_label(Poss_off = "Off Poss",
             Poss_def = "Def Poss",
             Plus_Minus = "Plus Minus",
             PPP_off = "Off Points Per Poss",
             PPP_def = "Def Points Per Poss") |> 
  gt_hulk_col_numeric(Plus_Minus) |> 
  gt_hulk_col_numeric(PPP_off) |> 
  gt_hulk_col_numeric(PPP_def, reverse = TRUE) |> 
  tab_header(title = "Guaynabo Mets Game 7 Stats", 
             subtitle = "Minimum 10 possessions (either offensive or defensive)") |> 
  opt_align_table_header(align = "center") |> 
  cols_width(Plus_Minus ~ px(100),
             Lineup ~ px(400),
             PPP_off ~ px(100),
             PPP_def ~ px(100),
             Poss_off ~ px(100),
             Poss_def ~ px(100)) |> 
  gtsave("season_stats.png") 


# for on/off stats

on_off |>
  gt() |> 
  gt_theme_espn() |> 
  cols_label(On.Off = "On/Off",
             On.Court = "On Court",
             Off.Court = "Off Court") |> 
  gt_hulk_col_numeric(On.Off) |> 
  tab_header(title = "Player On Off Season Stats") |> 
  opt_align_table_header(align = "center") |> 
  gtsave("on_off.png")


