library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gt)
library(gtExtras)
library(DT)
library(dplyr)
library(bslib)
library(stringr) 
library(tidyverse)

Efficiency <- read.csv("CSVs/Efficiency.csv")

Efficiency$Type[Efficiency$Type == "Catch and Shoot Jumper"] <- "Catch and Shoot"

Qualified <- read.csv("CSVs/All_Qualified.csv")

two_temp <- read.csv("two_man.csv")
three_temp <- read.csv("three_man.csv")
four_temp <- read.csv("four_man.csv")
five_temp <- read.csv("five_man.csv")
Logos <- read.csv("BSN_Logos.csv")



#############################
# CODE FOR 5 MAN DATA FRAME





five_temp <- five_temp |> 
  rename('Player 1' = 'Player.1', 'Player 2' = 'Player.2', 'Player 3' = 'Player.3', 'Player 4' = 'Player.4', 'Player 5' = 'Player.5', 'Total Poss' = 'POSS') |> 
  mutate(PPS = round(PPS, 2),
         PPP = round(PPP, 2),
         pps_percentile = round(100*percent_rank(PPS), 2))


five_off <- five_temp |>
  filter(Possession == "Offense") |>
  mutate(
    PTS = as.numeric(PTS),
    `Total Poss` = as.numeric(`Total Poss`),
    off_rtg = round(100 * (PTS / `Total Poss`), 2),
    ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    off_rtg_perc = round(100 * percent_rank(off_rtg), 2),
    ts_perc = round(100 * percent_rank(ts), 2)) |>
  rename(off_poss = `Total Poss`,
         off_pts = PTS) |> 
  select(Team, `Player 1`, `Player 2`, `Player 3`, `Player 4`, `Player 5`, off_poss, off_rtg,  off_rtg_perc, ts, ts_perc, off_pts) 


five_def <- five_temp |>
  filter(Possession == "Defense") |>
  mutate(
    PTS = as.numeric(PTS),
    `Total Poss` = as.numeric(`Total Poss`),
    def_rtg = round(100 * (PTS / `Total Poss`), 2),
    opp_ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    def_rtg_perc = round(100 * percent_rank(def_rtg), 2),
    opp_ts_perc = round(100 * percent_rank(opp_ts), 2)) |>
  rename(def_poss = `Total Poss`,
         opp_pts = PTS) |> 
  select(Team, `Player 1`, `Player 2`, `Player 3`, `Player 4`, `Player 5`, def_poss, def_rtg, def_rtg_perc, opp_ts, opp_ts_perc, opp_pts)


five <- five_off |> 
  left_join(five_def, by = c("Team", "Player 1", "Player 2", "Player 3", "Player 4", "Player 5")) |> 
  mutate(net_rtg = off_rtg - def_rtg,
         net_rtg_perc = round(100 * percent_rank(net_rtg), 2),
         plus_minus = off_pts - opp_pts) 


five <- five |> 
  left_join(Logos, by = "Team") |> 
  mutate(Lineup = paste(`Player 1`, 
                         `Player 2`,
                         `Player 3`,
                         `Player 4`,
                         `Player 5`, 
                         sep = " - "))

other_totals <- five |> 
  pivot_longer(
    # Reshape data to long format where each row is a player
    
    cols = `Player 1` : `Player 5`,           
    values_to = "Player") |> 
  
  group_by(Player) |>             
  summarise(off_poss = sum(off_poss, na.rm = TRUE),
            def_poss = sum(def_poss, na.rm = TRUE),
            team_off_pts = sum(off_pts, na.rm = TRUE),
            opp_pts = sum(opp_pts, na.rm = TRUE),
            plus_minus = sum(plus_minus, na.rm = TRUE))

col_order <- c('Logo', 'Team', 'Lineup', 'net_rtg', 'net_rtg_perc', 'off_poss', 'off_rtg', 
                      'off_rtg_perc', 'ts', 'ts_perc', 'def_poss', 'def_rtg', 'def_rtg_perc', 'opp_ts', 'opp_ts_perc')

five <- five[, col_order] |> 
  select('Logo', 'Team', 'Lineup', 'net_rtg', 'off_poss', 'off_rtg', 
         'ts', 'def_poss', 'def_rtg', 'opp_ts')


####################################################
# 4-MAN CODE


four_off <- four_temp |>
  filter(Possession == "Offense") |>
  rename(off_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2', 'Player 3' = 'Player.3', 'Player 4' = 'Player.4') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    off_poss = as.numeric(off_poss),
    off_rtg = round(100 * (PTS / off_poss), 2),
    ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    off_rtg_perc = round(100 * percent_rank(off_rtg), 2),
    ts_perc = round(100 * percent_rank(ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, `Player 3`, `Player 4`, off_poss, off_rtg,  off_rtg_perc, ts, ts_perc) 


four_def <- four_temp |>
  filter(Possession == "Defense") |>
  rename(def_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2', 'Player 3' = 'Player.3', 'Player 4' = 'Player.4') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    def_poss = as.numeric(def_poss),
    def_rtg = round(100 * (PTS / def_poss), 2),
    opp_ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    def_rtg_perc = round(100 * percent_rank(def_rtg), 2),
    opp_ts_perc = round(100 * percent_rank(opp_ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, `Player 3`, `Player 4`, def_poss, def_rtg, def_rtg_perc, opp_ts, opp_ts_perc)


four <- four_off |> 
  left_join(four_def, by = c("Team", "Player 1", "Player 2", "Player 3", "Player 4")) |> 
  mutate(net_rtg = off_rtg - def_rtg,
         net_rtg_perc = round(100 * percent_rank(net_rtg), 2)) 


four <- four |> 
  left_join(Logos, by = "Team") |> 
  mutate(Lineup = paste(`Player 1`, 
                        `Player 2`,
                        `Player 3`,
                        `Player 4`,
                        sep = " - "))

four <- four[, col_order] |> 
  select('Logo', 'Team', 'Lineup', 'net_rtg', 'off_poss', 'off_rtg', 'ts', 'def_poss', 'def_rtg', 'opp_ts')


#########################################################
# CODE FOR 3-MAN DF


three_off <- three_temp |>
  filter(Possession == "Offense") |>
  rename(off_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2', 'Player 3' = 'Player.3') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    off_poss = as.numeric(off_poss),
    off_rtg = round(100 * (PTS / off_poss), 2),
    ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    off_rtg_perc = round(100 * percent_rank(off_rtg), 2),
    ts_perc = round(100 * percent_rank(ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, `Player 3`, off_poss, off_rtg,  off_rtg_perc, ts, ts_perc) 


three_def <- three_temp |>
  filter(Possession == "Defense") |>
  rename(def_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2', 'Player 3' = 'Player.3') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    def_poss = as.numeric(def_poss),
    def_rtg = round(100 * (PTS / def_poss), 2),
    opp_ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    def_rtg_perc = round(100 * percent_rank(def_rtg), 2),
    opp_ts_perc = round(100 * percent_rank(opp_ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, `Player 3`, def_poss, def_rtg, def_rtg_perc, opp_ts, opp_ts_perc)


three <- three_off |> 
  left_join(three_def, by = c("Team", "Player 1", "Player 2", "Player 3")) |> 
  mutate(net_rtg = off_rtg - def_rtg,
         net_rtg_perc = round(100 * percent_rank(net_rtg), 2)) 


three <- three |> 
  left_join(Logos, by = "Team") |> 
  mutate(Lineup = paste(`Player 1`, 
                        `Player 2`,
                        `Player 3`,
                        sep = " - "))

three <- three[, col_order] |> 
  select('Logo', 'Team', 'Lineup', 'net_rtg', 'off_poss', 'off_rtg', 'ts', 'def_poss', 'def_rtg', 'opp_ts')


###############################################################
# CODE FOR 2-MAN DF


two_off <- two_temp |>
  filter(Possession == "Offense") |>
  rename(off_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    off_poss = as.numeric(off_poss),
    off_rtg = round(100 * (PTS / off_poss), 2),
    ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    off_rtg_perc = round(100 * percent_rank(off_rtg), 2),
    ts_perc = round(100 * percent_rank(ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, off_poss, off_rtg,  off_rtg_perc, ts, ts_perc) 


two_def <- two_temp |>
  filter(Possession == "Defense") |>
  rename(def_poss = 'POSS', 'Player 1' = 'Player.1', 'Player 2' = 'Player.2') |> 
  
  mutate(
    PTS = as.numeric(PTS),
    def_poss = as.numeric(def_poss),
    def_rtg = round(100 * (PTS / def_poss), 2),
    opp_ts = round(100*(PTS / (2 * (FG.ATT + FT.ATT))), 2),
    def_rtg_perc = round(100 * percent_rank(def_rtg), 2),
    opp_ts_perc = round(100 * percent_rank(opp_ts), 2)) |>
  
  select(Team, `Player 1`, `Player 2`, def_poss, def_rtg, def_rtg_perc, opp_ts, opp_ts_perc)


two <- two_off |> 
  left_join(two_def, by = c("Team", "Player 1", "Player 2")) |> 
  mutate(net_rtg = off_rtg - def_rtg,
         net_rtg_perc = round(100 * percent_rank(net_rtg), 2)) 


two <- two |> 
  left_join(Logos, by = "Team") |> 
  mutate(Lineup = paste(`Player 1`, 
                        `Player 2`,
                        sep = " - "))

two <- two[, col_order] |> 
  select('Logo', 'Team', 'Lineup', 'net_rtg', 'off_poss', 'off_rtg', 'ts', 'def_poss', 'def_rtg', 'opp_ts')


#################################################################


totals <- read.csv("PlayerReportData.csv")

Logos <- Logos |> 
  rename("abbrev" = "Team_abbrev")

totals <- totals |> 
  mutate(Team = if_else(Team == "MET/MET", "MET", Team),
         Team = if_else(Team == "LEO/FAJ/CAP/GRI", "GRI", Team),
         Team = if_else(Team == "MET/GRI", "GRI", Team),
         Team = if_else(Team == "FAJ/CAN", "CAN", Team),
         Team = if_else(Team == "MAN/CAP/CAN", "CAN", Team),
         Team = if_else(Team == "FAJ/CAR", "CAR", Team),
         Team = if_else(Team == "CAN/GRI/FAJ", "FAJ", Team),
         Team = if_else(Team == "GRI/LEO", "LEO", Team),
         Team = if_else(Team == "IND/LEO/GRI", "GRI", Team),
         Team = if_else(Team == "ATL/CAP", "CAP", Team),
         Team = if_else(Team == "MAN/CAR", "CAR", Team),
         Team = if_else(Team == "MAN/IND", "IND", Team),
         Team = if_else(Team == "VAQ/LEO", "LEO", Team),
         Team = if_else(Team == "IND/CAP", "CAP", Team),
         Team = if_else(Team == "FAJ/PIR/CAN", "CAN", Team),
         Team = if_else(Team == "IND/MAN/CAN", "CAN", Team),
         Team = if_else(Team == "IND/GRI", "GRI", Team),
         Team = if_else(Team == "ATL/LEO", "IND", Team))

totals <- totals |> 
  mutate(Import = if_else(Import == 1, "Import", "Domestic")) |> 
  left_join(Logos, by = c("Team" = "abbrev")) |> 
  left_join(other_totals, by = "Player")

totals <- totals |> 
  filter(!is.na(opp_pts), !is.na(def_poss), !is.na(team_off_pts), !is.na(off_poss)) |> 
  mutate(
    def_rtg = round(100*(opp_pts / def_poss), 2),
    off_rtg = round(100*(team_off_pts / off_poss), 2),
    net_rtg = off_rtg - def_rtg,
    net_rtg_perc = round(100 * percent_rank(net_rtg), 2),
    off_rtg_perc = round(100 * percent_rank(off_rtg), 2),
    def_rtg_perc = round(100 * percent_rank(def_rtg), 2),
    plus_minus_perc = round(100 * percent_rank(plus_minus), 2),
    `FGA/G` = round(FGA / GP, 2),
    `3PA/G` = round(X3PA / GP, 2),
    fga_g_perc = round(100 * percent_rank(`FGA/G`), 2),
    `3pa_g_perc` = round(100 * percent_rank(`3PA/G`), 2),
    fg_percentage_perc = round(100*percent_rank(FG.), 2),
    `3p_percentage_perc` = round(100*percent_rank(`X3P.`), 2),
    ft_percentage_perc = round(100*percent_rank(FT.), 2))


convert_percentile <- function(column) {
  as.numeric(gsub("%", "", column))
}

percentile_columns <- c(
  "TS..Percentile", "EFG..Percentile", "SSQ.Percentile", "SSM.Percentile", 
  "PPG.Percentile", "RPG.Percentile", "APG.Percentile", "SPG.Percentile",
  "BPG.Percentile", "TOV.G.Percentile", "net_rtg_perc", "off_rtg_perc",
  "def_rtg_perc", "plus_minus_perc", "PER.Percentile", 
  "Synergy.Sports.GM.Score.Percentile", "FTA.G.Percentile"
)

totals <- totals %>% 
  mutate(across(all_of(percentile_columns), convert_percentile))



players <- unique(unlist(two_temp[c("Player.1", "Player.2")]))



            


unique(Efficiency$Player)

Qualified <- Qualified |> 
  mutate(
    TS = round(100*(as.numeric(TS.))),
    MIN = as.numeric(MIN),
    Game_Score = as.numeric(Synergy_Sports_GM_Score),
    TS_Percentile = as.numeric(TS_Percentile),
    FGA_G_Percentile = as.numeric(FGA_G_Percentile),
    Position_Group = as.character(Position_Group),
    MIN = as.numeric(MIN))

Efficiency <- Efficiency |> 
  group_by(Player) |> 
  mutate(
    Player = as.character(Player),
    Percentage = FGA / (FGA[Type == "All"]))




Bigs <- Qualified |> 
  filter(Position_Group == "Bigs") |> 
  mutate(Position_TS_Percentile = round(100*percent_rank(TS.), 2)) |> 
  mutate(Position_FGA_G_Percentile = round((100*percent_rank(FGA.G)), 2))

Wings <- Qualified |> 
  filter(Position_Group == "Wings") |> 
  mutate(Position_TS_Percentile = round(100*percent_rank(TS.), 2)) |> 
  mutate(Position_FGA_G_Percentile = round((100*percent_rank(FGA.G)), 2))

Guards <- Qualified |> 
  filter(Position_Group == "Guards") |> 
  mutate(Position_TS_Percentile = round(100*percent_rank(TS.), 2)) |> 
  mutate(Position_FGA_G_Percentile = round((100*percent_rank(FGA.G)), 2))

Point_Guards <- Qualified |> 
  filter(Position_Group == "Point_Guards") |> 
  mutate(Position_TS_Percentile = round(100*percent_rank(TS.), 2)) |> 
  mutate(Position_FGA_G_Percentile = round((100*percent_rank(FGA.G)), 2))

Qualified <- bind_rows(Bigs, Wings, Guards, Point_Guards)

Mets <- Qualified |> 
  filter(Team == "MET")



n_rows = nrow(Qualified)
max_minutes = max(Qualified$MIN, na.rm = TRUE)
max_fga_g = max(Qualified$FGA.G)

last_plot <- Qualified |> 
  filter(Team %in% c("ATL", "CAN", "CAP", "CAR", "FAJ", "GRI", "IND", "LEO","MAN", "MET", "PIR", "VAQ")) |> 
  group_by(Team) |> 
  mutate(percent_minutes = (MIN / sum(MIN)) * 100) |> 
  mutate(percent_PER = (PER / sum(PER)) * 100) |> 
  ungroup() |> 
  select(Player, percent_PER, percent_minutes, Team)



all_teams = c("ATL", "CAN", "CAP", "CAR", "FAJ", "GRI", "IND", "LEO","MAN", "MET", "PIR", "VAQ")

tms <- unique(five$Team)




Qualified$Team <- as.factor(Qualified$Team)


# CUSTOM COLOR PALETTES

zz <- seq(0, 100, 1)  
clrs <- colorRampPalette(c("#5703FF", "darkgreen"))(length(zz) + 1)

yy <- seq(0, 15, 1)  
value1 <- colorRampPalette(c("white", "orange"))(length(yy) + 1)

aa <- seq(0, 15, 1)  
value2 <- colorRampPalette(c("white", "darkgreen"))(length(aa) + 1)

custom_colors <- c("orange", "green", "#5733FF", "yellow", "#33FFFF", "pink")

numeric_columns <- names(totals)[sapply(totals, is.numeric)]
numeric_columns


###########################################################
# Start of the Shiny Code


ui <- navbarPage("2023 Guaynabo Mets",
                 theme = bs_theme(bootswatch = "flatly"),
                 
                 tabPanel("Lineup Stats",
                          fluidRow(
                            column(3,
                                   selectInput(
                                     inputId = "lineup",
                                     label = "Combination:",
                                     choices = c("5-Man" = "five",
                                                 "4-Man" = "four",
                                                 "3-Man" = "three",
                                                 "2-Man" = "two"))),
                            
                            column(3,
                                   selectInput(
                                     inputId = "player",
                                     label = "Select Players:",
                                     choices = players,
                                     multiple = TRUE)),
                            
                            column(3,
                                   pickerInput("tms", 
                                               "Select Teams:",
                                               choices = c("All Teams" = "all", tms),
                                               options = list(`actions-box` = TRUE),
                                               multiple = TRUE,
                                               selected = "all")
                            ),
                            
                            column(3,
                                   sliderInput(inputId = "slider", 
                                               label = "Minimum Possessions (Off and Def):", 
                                               min = 0, 
                                               max = max(two_temp$POSS),
                                               value = 50)),
                            
                            
                            mainPanel(
                              fluidRow(
                                align = "center",
                                gt_output(outputId = "lineup_table"),
                              ), width = 12
                            ))
                 ),
                 
                 tabPanel("Player Overview",
                          fluidRow(
                            selectInput(
                              inputId = "report",
                              label = "Select Player:",
                              choices = unique(totals$Player), 
                              selected = "DeMarcus Cousins"
                            )
                         ),
                         mainPanel(
                           fluidRow(
                             align = "center",
                             div(style = "margin-top: -120px;", gt_output(outputId = "basic_info")), 
                             div(style = "margin-top: -40px;", gt_output(outputId = "overall")), 
                             gt_output(outputId = "per_game"),
                             gt_output(outputId = "efficiency"),
                             column(2),
                             column(4, div(style = "margin-top: 20px;", plotOutput(outputId = "totalfga"))), 
                             column(4, div(style = "margin-top: 20px;", plotOutput(outputId = "ts"))),
                             column(2)
                           ), width = 12
                         )
                 ),
                 
                 tabPanel("League-Wide Trends",
                          fluidPage(
                            tags$h5("Click to show a plot:", style = "font-weight: bold; text-align: center;"),
                            div(style = "text-align: center;", 
                                actionButton("age", "Age Distribution", style = "margin: 5px;"), 
                                actionButton("per", "PER Distribution", style = "margin: 5px;"),

                                hr(),
                                plotOutput("plot", width = "1500px", height = "750px")
                            )
                          )
                 ),
                 
                 tabPanel("Player Stat Comparison",
                          fluidRow(
                            
                            
                            
                            column(3,
                                   selectInput(
                                     inputId = "y",
                                     label = "Y-axis:",
                                     choices = c(
                                       "Games Played" = "GP", "Points" = "PTS", "FGM" = "FGM",
                                       "FGA" = "FGA", "FG%" = "FG%", "3PM" = "3PM", "3PA" = "3PA",
                                       "3P%" = "3P%", "FTM" = "FTM", "FTA" = "FTA", "FT%" = "FT%", "Offensive Rebounds" = "ORB",
                                       "Defensive Rebounds" = "DRB", "Total Rebounds" = "REB", "Assists" = "AST",
                                       "Steals" = "STL",
                                       "Points Per Game" = "PPG",
                                       "Blocks" = "BLK",
                                       "Turnovers" = "TOV",
                                       "Personal Fouls" = "PF",
                                       "Minutes Per Game" = "MPG",
                                       "Rebounds Per Game" = "RPG",
                                       "Assists Per Game" = "APG",
                                       "Steals Per Game" = "SPG",
                                       "Blocks Per Game" = "BPG",
                                       "Effective Field Goal %" = "EFG.",
                                       "True Shooting %" = "TS.",
                                       "Player Efficiency Rating" = "PER",
                                       "Net Rating" = "NetRtg",
                                       "Offensive Rating" = "OffRtg",
                                       "Defensive Rating" = "DefRtg",
                                       "Plus/Minus" = "plus_minus",
                                       "FGA / Game" = "FGA.G"
                                     ),
                                     selected = "PPG")
                            ),
                            column(3,
                                   selectInput(
                                     inputId = "x",
                                     label = "X-axis:",
                                     choices = c(
                                       "Games Played" = "GP", "Points" = "PTS", "FGM" = "FGM",
                                       "FGA" = "FGA", "FG%" = "FG%", "3PM" = "3PM", "3PA" = "3PA",
                                       "3P%" = "3P%", "FTM" = "FTM", "FTA" = "FTA", "FT%" = "FT%", "Offensive Rebounds" = "ORB",
                                       "Defensive Rebounds" = "DRB", "Total Rebounds" = "REB", "Assists" = "AST",
                                       "Steals" = "STL",
                                       "Points Per Game" = "PPG",
                                       "Blocks" = "BLK",
                                       "Turnovers" = "TOV",
                                       "Personal Fouls" = "PF",
                                       "Minutes Per Game" = "MPG",
                                       "Rebounds Per Game" = "RPG",
                                       "Assists Per Game" = "APG",
                                       "Steals Per Game" = "SPG",
                                       "Blocks Per Game" = "BPG",
                                       "Effective Field Goal %" = "EFG.",
                                       "True Shooting %" = "TS.",
                                       "Player Efficiency Rating" = "PER",
                                       "Net Rating" = "NetRtg",
                                       "Offensive Rating" = "OffRtg",
                                       "Defensive Rating" = "DefRtg",
                                       "Plus/Minus" = "plus_minus",
                                       "FGA / Game" = "FGA.G"
                                     ),
                                     selected = "APG")
                            ),
                            column(3,
                                   sliderInput(
                                     inputId = "fga",
                                     label = "Minimum FGA/G:",
                                     value = 0,
                                     min = 0, max = max_fga_g,
                                     step = 1)
                            ),
                            
                            
                            column(3,
                                   pickerInput("teams", 
                                               "Display players from these teams:",
                                               choices = c("All Teams" = "all", all_teams),
                                               options = list(`actions-box` = TRUE),
                                               multiple = TRUE,
                                               selected = "all")
                            ),
                            mainPanel(
                              plotOutput(outputId = "scatterplot", height = "600px")
                            )
                          )
                 ))

                            
                            
                 # tabPanel("League Analysis",
                 #           sidebarLayout(
                 #             sidebarPanel(
                 #               titlePanel("Filters:"),
                 #               selectInput(
                 #                 inputId = "left",
                 #                 label = "Left Side:",
                 #                 choices = c("Player Efficiency Rating" = "PER",
                 #                             "Synergy Game Score" = "Game_Score",
                 #                             "Points" = "PTS")),
                 #               
                 #               selectInput(
                 #                 inputId = "right",
                 #                 label = "Right Side:",
                 #                 choices = c("Minutes Played" = "MIN",
                 #                             "Minutes Per Game" = "MPG",
                 #                             "Total FGA" = "FGA"))))
                          
                        #    mainPanel(
                       #       plotOutput(outputId = "league", width = 1250, height = 1000)
                       #     )
                     #     )))

                 
                 
                 
##################################################
# START OF SERVER
                 
                 
server <- function(input, output, session) {
  
  sorted_data <- reactive({
    
    data_sorted <- Qualified |> filter(FGA.G >= input$fga)
    
    if ("all" %in% input$teams) {
      data_sorted
    } else {
      data_sorted |> filter(Team %in% input$teams)
    }  
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data = sorted_data(), aes_string(x = input$x, y = input$y)) +
      ggrepel::geom_text_repel(aes(label = Player)) +
      geom_point(size = 5, color = "navy") +
      geom_smooth(method=lm, se=FALSE, color = "darkgrey", size = .5) +
      theme_bw() + 
      labs(x = input$x, y = input$y) +
      theme(
        axis.title.x = element_text(size = 18, color = "black"),  
        axis.text.x = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 12, color = "black", face = "bold")) 
  }, height = 900, width = 1850)
  
  observeEvent(input$age, {
    
    output$plot <- renderPlot({

      ggplot(totals, aes(x = Age, fill = Team.y)) + 
        geom_density(alpha = 0.5) +
        labs(title = "Age Distribution by Team",
             x = "Age", 
             y = "Frequency") +
        facet_wrap(~ Team.y, ncol = 3) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
              axis.title.x = element_text(size = 18),                 
              axis.title.y = element_text(size = 18),                
              axis.text.x = element_text(size = 12, face = "bold", hjust = .5),                 
              axis.text.y = element_text(size = 14, face = "bold"), 
              strip.text = element_text(size = 16, face = "bold"),
              legend.position = "none")
    })
  })
  
  observeEvent(input$per, {
    
    output$plot <- renderPlot({
      ggplot(totals, aes(x = PER, fill = Team.y)) + 
        geom_density(alpha = 0.5) +
        labs(title = "PER Distribution by Team",
             x = "Player Efficiency Rating (PER)", 
             y = "Frequency") +
        facet_wrap(~ Team.y, ncol = 3) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
              axis.title.x = element_text(size = 18),               
              axis.title.y = element_text(size = 18),               
              axis.text.x = element_text(size = 12, face = "bold", hjust = .5),                 
              axis.text.y = element_text(size = 14, face = "bold"), 
              strip.text = element_text(size = 16, face = "bold"),
              legend.position = "none")
    })
  })
  

  # PLAYER REPORT TAB
  
  basic_info_tbl <- reactive({
    info <- totals |> 
      filter(Player == input$report) |> 
      select(Logo, Team.y, Player, Position, Age, Height, Import) |> 
      gt() |> 
      cols_align(align = "center") |> 
      gt_img_rows(Logo, height = 75) |> 
      cols_width(everything() ~ px(186)) |>
      cols_label(Team.y = "Team",
                 Logo = "") |> 
      gt_theme_espn() |>
      tab_header(title = "General Info") |> 
      opt_align_table_header(align = "center")
  })
  
  output$basic_info <- render_gt({
    basic_info_tbl()
    }, width = "100%",
    align = "center")
  
  
  overall_tbl <- reactive({
    overall <- totals |> 
      filter(Player == input$report) |> 
      select(GP, net_rtg, net_rtg_perc, off_rtg, off_rtg_perc, def_rtg, def_rtg_perc, plus_minus, plus_minus_perc, 
             PER, PER.Percentile, Synergy.Sports.GM.Score, Synergy.Sports.GM.Score.Percentile) |> 
      gt() |> 
      tab_spanner(label = "Net Rating",
                  columns = c(net_rtg, net_rtg_perc)) |> 
      tab_spanner(label = "Offensive Rating",
                  columns = c(off_rtg, off_rtg_perc)) |> 
      tab_spanner(label = "Defensive Rating",
                  columns = c(def_rtg, def_rtg_perc)) |>
      tab_spanner(label = "Plus Minus",
                  columns = c(plus_minus, plus_minus_perc)) |> 
      tab_spanner(label = " PER ",
                  columns = c(PER, PER.Percentile)) |>
      tab_spanner(label = "Synergy Game Score",
                  columns = c(Synergy.Sports.GM.Score, Synergy.Sports.GM.Score.Percentile)) |>
      cols_align(align = "center") |> 
      cols_width(everything() ~ px(100)) |>
      cols_label(
        GP = "Games Played",
        net_rtg = " ", net_rtg_perc = "",
        off_rtg = " ", off_rtg_perc = " ",
        def_rtg = " ", def_rtg_perc = " ",
        plus_minus = " ", plus_minus_perc = " ",
        PER = " ", `PER.Percentile` = " ",
        `Synergy.Sports.GM.Score` = " ", `Synergy.Sports.GM.Score.Percentile` = " "
      ) |>
      gt_theme_espn() |>
      fmt_percent(columns = c(net_rtg_perc, off_rtg_perc, def_rtg_perc, 
                              plus_minus_perc, `PER.Percentile`, `Synergy.Sports.GM.Score.Percentile`),
                  scale_values = FALSE) |> 
      data_color(
        columns = c(net_rtg_perc, off_rtg_perc, def_rtg_perc, plus_minus_perc, PER.Percentile, `Synergy.Sports.GM.Score.Percentile`),
        colors = scales::col_numeric(palette = clrs, domain = range(zz))) |>
      tab_header(title = "Overall",
                 subtitle = "Format is Stat | Percentile Rank in League") |> 
      opt_align_table_header(align = "center")
  })
  
  output$overall <- render_gt({
    overall_tbl()
    }, width = "100%",
    align = "center")
  
  
  per_game_tbl <- reactive({
    per_game <- totals |> 
      filter(Player == input$report) |> 
      select(PPG, RPG, APG, SPG, `TOV.G`, BPG, `FGA/G`, `3PA/G`, `FTA.G`, `PPG.Percentile`, `RPG.Percentile`, `APG.Percentile`, 
             `SPG.Percentile`, `TOV.G.Percentile`, `BPG.Percentile`, `fga_g_perc`, `3pa_g_perc`, `FTA.G.Percentile`) |> 
      mutate(across(ends_with("Percentile"), ~as.numeric(.))) |> 
      gt() |> 
      tab_spanner(label = "Points",
                  columns = c(PPG, `PPG.Percentile`)) |> 
      tab_spanner(label = "Rebounds",
                  columns = c(RPG, `RPG.Percentile`)) |> 
      tab_spanner(label = "Assists",
                  columns = c(APG, `APG.Percentile`)) |> 
      tab_spanner(label = "Steals",
                  columns = c(SPG, `SPG.Percentile`)) |> 
      tab_spanner(label = "Blocks",
                  columns = c(BPG, `BPG.Percentile`)) |> 
      tab_spanner(label = "Turnovers",
                  columns = c(`TOV.G`, `TOV.G.Percentile`)) |> 
      tab_spanner(label = "FGA",
                  columns = c(`FGA/G`, `fga_g_perc`)) |> 
      tab_spanner(label = "3PA",
                  columns = c(`3PA/G`, `3pa_g_perc`)) |> 
      tab_spanner(label = "FTA",
                  columns = c(`FTA.G`, `FTA.G.Percentile`)) |> 
      cols_width(everything() ~ px(72)) |>
      cols_align(align = "center") |> 
      cols_label(
        PPG = " ", RPG = " ", APG = " ", SPG = " ", `TOV.G` = " ", BPG = " ", 
        `FGA/G` = " ", `3PA/G` = " ", `FTA.G` = " ", 
        `PPG.Percentile` = " ", `RPG.Percentile` = " ", `APG.Percentile` = " ",
        `SPG.Percentile` = " ", `TOV.G.Percentile` = " ", `BPG.Percentile` = " ",
        `fga_g_perc` = " ", `3pa_g_perc` = " ", `FTA.G.Percentile` = " "
      ) |>
      gt_theme_espn() |>
      fmt_percent(columns = c(`PPG.Percentile`, 
                  `RPG.Percentile`, `APG.Percentile`, `SPG.Percentile`, `BPG.Percentile`, 
                  `TOV.G.Percentile`, `fga_g_perc`, `3pa_g_perc`, `FTA.G.Percentile`), 
                  scale_values = FALSE) |> 
      data_color(
        columns = c(`PPG.Percentile`, `RPG.Percentile`, `APG.Percentile`, `SPG.Percentile`, `BPG.Percentile`, `TOV.G.Percentile`, `fga_g_perc`, `3pa_g_perc`, `FTA.G.Percentile`),
        colors = scales::col_numeric(palette = clrs, domain = range(zz))) |>
      tab_header(title = "Per Game") |> 
      opt_align_table_header(align = "center")
  })
  
  output$per_game <- render_gt({
    per_game_tbl()}, 
    width = "100%",
    align = "center")
  
  
  efficiency_tbl <- reactive({
    efficiency <- totals |>
      filter(Player == input$report) |>
      select(TS., TS..Percentile, EFG., EFG..Percentile, 
             SSQ, SSQ.Percentile, SSM, SSM.Percentile, 
             FG., fg_percentage_perc, X3P., `3p_percentage_perc`, 
             FT., ft_percentage_perc) |> 
      mutate(TS. = round(100 * TS., 2),
             EFG. = round(100 * EFG., 2),
             FT. = round(100 * FT., 2),
             X3P. = round(100 * X3P., 2),
             FG. = round(100 * FG., 2)) |>
      gt() |>
      cols_width(everything() ~ px(92)) |>
      cols_align(align = "center") |>
      tab_spanner(label = "True Shooting %",
                  columns = c(TS., TS..Percentile)) |>
      tab_spanner(label = "EFG %",
                  columns = c(EFG., EFG..Percentile)) |>
      tab_spanner(label = "Synergy Shot Quality",
                  columns = c(SSQ, SSQ.Percentile)) |>
      tab_spanner(label = "Synergy Shot Making",
                  columns = c(SSM, SSM.Percentile)) |>
      tab_spanner(label = "FG %",
                  columns = c(FG., fg_percentage_perc)) |>
      tab_spanner(label = "3P %",
                  columns = c(X3P., `3p_percentage_perc`)) |>
      tab_spanner(label = "FT %",
                  columns = c(FT., ft_percentage_perc)) |>
      cols_label(
        TS. = "", TS..Percentile = "", 
        EFG. = "", EFG..Percentile = "", 
        SSQ = "", SSQ.Percentile = "", 
        SSM = "", SSM.Percentile = "", 
        FG. = "", fg_percentage_perc = "", 
        X3P. = "", `3p_percentage_perc` = "", 
        FT. = "", ft_percentage_perc = ""
      ) |> 
      gt_theme_espn() |>
      fmt_percent(columns = c(TS..Percentile, EFG..Percentile, SSQ.Percentile, 
                              SSM.Percentile, fg_percentage_perc, `3p_percentage_perc`, ft_percentage_perc),
                  scale_values = FALSE) |> 
      data_color(
        columns = c(TS..Percentile, EFG..Percentile, SSQ.Percentile, SSM.Percentile, fg_percentage_perc, `3p_percentage_perc`, ft_percentage_perc),
        colors = scales::col_numeric(palette = clrs, domain = range(zz))
      ) |>
      tab_header(title = "Shooting") |>
      opt_align_table_header(align = "center")
  })
  
  output$efficiency <- render_gt({
    efficiency_tbl()}, 
    width = "100%",
    align = "center")
  
  
  ######################################
  
  # NEXT TAB
  
  
  sorted_data <- reactive({
    
    data_sorted <- Qualified |> filter(FGA >= input$fga)
    
    if ("all" %in% input$teams) {
      data_sorted
    } else {
      data_sorted |> filter(Team %in% input$teams)
    }  
  })
  
  
  


  
  filtered_data_player <- reactive({
    Efficiency |> 
      filter(Player == input$report)
  })
  
  # filters the data to only the report selected
  
  filtered_data_report <- reactive({
    print(paste("Currently selected player:", input$report))                            
    Qualified |> 
      filter(Player == input$report) 
  })
  
  # filters the data to only the player selected
  
  output$ts <- renderPlot({
    ggplot(filtered_data_player(), aes(x = Type, y = TS, fill = Type)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_fill_viridis_d() +
      labs(
        title = "True Shooting % by Shot Type",
        subtitle = "Note: This data is only included for players on the Mets.",
        x = "Shot Type",
        y = "True Shooting %"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(
        plot.title = element_text(hjust = .5, face = "bold", size = 20),    
        plot.subtitle = element_text(size = 14, hjust = .5),                
        axis.title.x = element_text(size = 18),              
        axis.title.y = element_text(size = 18),              
        axis.text.x = element_text(size = 10, face = "bold", hjust = .5),                  
        axis.text.y = element_text(size = 14, face = "bold"),            
        legend.position = "none"                               
      ) + 
      guides(fill = FALSE)
  })
  

  
  output$totalfga <- renderPlot({
    ggplot(filtered_data_player(), aes(x = Type, y = FGA, fill = Type)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_fill_viridis_d() +
      labs(
        title = "Total FGA for Each Shot Type",
        subtitle = "Note: This data is only included for players on the Mets.",
        x = "Shot Type",
        y = "FGA"
      ) +
      theme(
        plot.title = element_text(hjust = .5, face = "bold", size = 20),   
        plot.subtitle = element_text(size = 14, hjust = .5),                
        axis.title.x = element_text(size = 18),                 
        axis.title.y = element_text(size = 18),                 
        axis.text.x = element_text(size = 12, face = "bold", hjust = .5),                  
        axis.text.y = element_text(size = 14, face = "bold"),                       
        legend.position = "none"                                
      )
  })
  
    
  

  output$report_table <- renderDataTable({
    #print(head(filtered_data_report()))
    DT::datatable(
      data = filtered_data_report() |> 
        select("Player", "Team", "Position", "GP", "PPG", "RPG", "APG"),
      rownames = TRUE,
      options = list(theme = 'bw'),
      colnames = c("Player", "Team", "Position", "Games Played", "Points Per Game", "Rebounds Per Game", "Assists Per Game")
    ) |> 
      formatStyle(c("PPG"), backgroundColor = styleInterval(zz, clrs)) |> 
      formatStyle(c("RPG"), backgroundColor = styleInterval(yy, value1)) |> 
      formatStyle(columns = "APG", 
                  valueColumns = "APG",
                  backgroundColor = styleInterval(zz, clrs))
  })
  
  # output$league <- renderPlot({
  #   ggplot(last_plot, aes(x = -percent_PER, y = reorder(Player, percent_minutes))) +
  #     geom_col(aes(fill = "grey"), color = "black", size = 0.5, position = position_dodge(width = 0.7), show.legend = FALSE) +
  #     geom_col(aes(x = percent_minutes, y = Player, fill = "black"), color = "black", size = 0.5, position = position_dodge(width = -0.7), show.legend = FALSE) +
  #     facet_wrap(~ Team, scales = "free_y", ncol = 2) + 
  #     theme_bw() + 
  #     labs(title = "Percent PER (Left) vs. Percent Minutes (Right)",
  #          x = "",
  #          y = "") + 
  #     theme(plot.title = element_text(hjust = 0.5))
  # })
  
  ##########################################
  # LINEUPS TAB
  
  
  lineup_data <- reactive({
    df <- switch(input$lineup,
                 five = five,
                 four = four,
                 three = three,
                 two = two)
    
    df <- df |> 
      filter(off_poss >= input$slider,
             def_poss >= input$slider)
    
    if (!is.null(input$player) && length(input$player) > 0) {
      for (choice in input$player) {
        df <- df |>
          filter(str_detect(Lineup, fixed(choice)))
      }
    }
    
    if (!"all" %in% input$tms) {
      df <- df |> filter(Team %in% input$tms)
    }
    
    df
  })
  

  output$lineup_table <- render_gt({

    df_filtered <- lineup_data()
    
    gt_table <- df_filtered |>
      gt() |>
      opt_table_outline() |> 
      cols_label(Logo = " ",
                 net_rtg = "Net Rating",
                 off_poss = "Offensive Possessions",
                 off_rtg = "Offensive Rating",
                 ts = "True Shooting %",
                 def_poss = "Defensive Possessions",
                 def_rtg = "Defensive Rating",
                 opp_ts = "Opponent True Shooting %") |> 
      gt_img_rows(Logo, height = 50) |> 
      cols_width(Lineup ~ px(500)) |>
      gt_hulk_col_numeric(net_rtg) |> 
      gt_hulk_col_numeric(off_rtg) |> 
      gt_hulk_col_numeric(def_rtg, reverse = TRUE) |> 
      cols_align(align = "center") |>
      gt_theme_espn() |>
      opt_align_table_header(align = "center") |> 
      opt_interactive(use_sorting = TRUE) |>
      opt_interactive(use_page_size_select = TRUE,
                      page_size_default = 20,
                      use_compact_mode = TRUE,
                      pagination_type = "jump") |> 
      tab_spanner(label = "OFFENSE",
                  columns = c(off_poss, off_rtg, ts)) |> 
      tab_spanner(label = "DEFENSE",
                  columns = c(def_poss, def_rtg, opp_ts))
    
    gt_table

  })
}

                 
shinyApp(ui = ui, server = server)
                 
                 





