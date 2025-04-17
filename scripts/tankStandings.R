require(tidyverse)
require(shlrtools)
require(data.table)

source("scripts/tankStandings/functions.R")

schedule <- gamesLoader(leagueID = 0) %>% 
  filter(
    type == "Regular Season"
  ) %>% 
  mutate(
    date = as_date(date)
  ) %>% 
  arrange(date) %>% 
  filter(
    played == 1
  )

teamInfo <- 
  readAPI("https://index.simulationhockey.com/api/v1/teams", query = list(league = 0)) %>% 
  select(
    conference, 
    division,
    name,
    id
  )

test <- create(schedule, teamInfo = teamInfo)

test %>% 
  select(
    name:P,
    GD:`GP at Elimination`,
    contains("after")
  ) %>% 
  View()




