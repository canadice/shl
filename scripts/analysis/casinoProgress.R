
#################################################################
##                       Casino Progress                       ##
##                                                             ##
##                     Created: 2021-05-01                     ##
##                    Last edit: 2021-05-01                    ##
#################################################################

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")

### Loads team data from index
teams <- 
  teamLoader(0) %>% 
  select(
    id,
    name,
    abbreviation,
    stats
  )

games <- gamesLoader(0)

### Loading casino lines
teamCasino <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1kisvxMASJvX26djRXzDVDxMz1ODqbaaMfuwUXtVwpWw/edit#gid=1074258598",
    sheet = "Teams",
    range = "A:D"
  )

### Analysis
currentProgress <- 
  left_join(
    teamCasino %>% 
      select(
        TeamId, 
        Casino
      ),
    teams,
    by = c("TeamId" = "id")
  ) %>% 
  mutate(
    GP = stats$wins + stats$losses + stats$overtimeLosses + stats$shootoutLosses,
    Record = 
      paste(
        stats$wins,
        stats$losses + stats$overtimeLosses + stats$shootoutLosses,
        sep = "-"
      ),
    XWin = 
      ((stats$wins)/GP * 66) %>% 
      round(2),
    Prediction = 
      case_when(
        XWin > Casino ~ "Over",
        TRUE ~ "Under"
      ),
    RemainingRecordForOver =
      case_when(
        (Casino - (stats$wins)) < 0 ~ "Achieved",
        (Casino - (stats$wins)) > (66 - GP) ~ "Not possible",
        TRUE ~ 
          paste(
            (Casino - (stats$wins)) %>% 
              ceiling(),
            (66 - GP - (Casino - (stats$wins))) %>% 
              floor(),
            sep = "-"
          )
      ),
    PaceVsCasino = XWin - Casino
  ) %>% 
  select(-stats, -abbreviation) %>% 
  relocate(name, .before = Casino) %>% 
  rename(Team = name)


strengthOfSchedule <- 
  games %>% 
  mutate(
    winner = 
      case_when(
        played == 0 ~ NA_integer_,
        homeScore > awayScore ~ homeTeam,
        TRUE ~ awayTeam
      ),
    loser = 
      case_when(
        played == 0 ~ NA_integer_,
        homeScore < awayScore ~ homeTeam,
        TRUE ~ awayTeam
      )
  ) %>% 
  select(
    homeTeam,
    awayTeam,
    played,
    winner,
    loser
  ) %>% 
  left_join(
    currentProgress %>% 
      select(TeamId, XWin),
    by = c("homeTeam" = "TeamId"),
    suffix = c(".x", ".home")
  ) %>% 
  left_join(
    currentProgress %>% 
      select(TeamId, XWin),
    by = c("awayTeam" = "TeamId"),
    suffix = c(".home", ".away")
  ) 

strength <- 
  strengthOfSchedule %>% 
  group_by(played, homeTeam) %>% 
  summarize(
    strength = mean(XWin.away)
  ) %>% 
  left_join(
    strengthOfSchedule %>% 
      group_by(played, awayTeam) %>% 
      summarize(
        strength = mean(XWin.home)
      ),
    by = c("homeTeam" = "awayTeam", "played"),
    suffix = c(".home", ".away")
  ) %>% 
  arrange(homeTeam, -played) %>% 
  rename(team = homeTeam) %>% 
  left_join(
    teams %>% 
      select(id, name),
    by = c("team" = "id")
  ) %>% 
  mutate(
    played = 
      case_when(
        played == 1 ~ "Yes",
        TRUE ~ "No"
      )
  ) %>% 
  select(-team)


# save(
#   strength,
#   currentProgress, 
#   teams,
#   file = "C:/Users/Canadice/Downloads/SHN Issue 1.RData"
#   )  


  
  










