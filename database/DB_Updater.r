
###########################################################################
###########################################################################
###                                                                     ###
###                    SETTING UP A SQLLITE DATABASE                    ###
###                                                                     ###
###########################################################################
###########################################################################

# install.packages(c("DBI", "dbplyr", "RSQLite"))

require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)

source("scripts/API/apiSetup.R")

con <- dbConnect(SQLite(), "database/SHL_Database.db")

dbListTables(con)

#################################################################
##                       Input team data                       ##
#################################################################

teamData <-
  tbl(con, "teamInfo") %>%
  collect()


dbWriteTable(con, "teamInfo", teamData, overwrite = TRUE)

# dbExecute(con, "UPDATE teamInfo SET abbr = 'NA' WHERE team = 'North America'")


##################################################################
##                        Player History                        ##
##################################################################

skaterHistory <-
  tbl(con, "skaterHistory") %>%
  collect()

table(skaterHistory$leagueID, skaterHistory$Season)

historyUpdate <- function(leagueId, season){
  skaterHistory <-
    tbl(con, "skaterHistory") %>%
    collect()
  
  if(skaterHistory %>% filter(leagueID == leagueId & Season == season) %>% nrow() != 0){
    stop("The current season for the league is already present in the data")
  }
  
  data <-
    indStatsLoader(leagueId, season, type = "regular") %>%
    lapply(
      X = .,
      FUN = function(x){
        do.call(what = data.frame, args = x) %>%
          ## Adds indication that stats are regular season
          mutate(
            type = 0
          ) %>%
          relocate(
            type,
            .after = season
          )
      }
    ) %>%
    append(
      indStatsLoader(leagueId, season, type = "playoffs") %>%
        lapply(
          X = .,
          FUN = function(x){
            do.call(what = data.frame, args = x) %>%
              ## Adds indication that stats are playoffs
              mutate(
                type = 1
              ) %>%
              relocate(
                type,
                .after = season
              )
          }
        )
    ) %>%
    do.call(what = plyr::rbind.fill, args = .) %>%
    # mutate(
    #   id = id
    # ) %>%
    dplyr::rename(
      fhmID = id
    ) %>%
    left_join(
      teamInfo %>%
        select(
          franchiseID,
          leagueID,
          newTeamID = teamID,
          abbr
        ),
      by = c("team" = "abbr", "league" = "leagueID")
    ) %>%
    
    ### Removes duplicated rows where the team abbreviation has been used for older teams
    group_by(name, type) %>%
    filter(
      newTeamID == max(newTeamID)
    ) %>%
    
    ### Selects data that is to be kept
    dplyr::select(
      franchiseID,
      skaterID = fhmID,
      Name = name,
      leagueID = league,
      -team,
      newTeamID,
      Season = season,
      isPlayoffs = type,
      GamesPlayed = gamesPlayed,
      Goals = goals,
      Assists = assists,
      Points = points,
      PlusMinus = plusMinus,
      PenaltyMinutes = pim,
      Hits = hits,
      Shots = shotsOnGoal,
      ShotsBlocked = shotsBlocked,
      MinutesPlayed = timeOnIce,
      PPGoals = ppGoals,
      PPAssists = ppAssists,
      PPPoints = ppPoints,
      PPMinutes = ppTimeOnIce,
      PKGoals = shGoals,
      PKAssists = shAssists,
      PKPoints = shPoints,
      PKMinutes = shTimeOnIce,
      FightsWon = fightWins,
      FightsLost = fightLosses,
      Giveaways = giveaways,
      Takeaways = takeaways,
      GR = gameRating,
      OGR = offensiveGameRating,
      DGR = defensiveGameRating,
      contains("advancedStats"),
      -(minutes:savePct)
    ) %>%
    filter(
      !is.na(Goals)
    ) %>%
    dplyr::rename_with(
      stringr::str_remove,
      contains("advancedStats"),
      pattern = "advancedStats."
    )
  
  return(data)
}

temp <- historyUpdate(leagueId = 3, season = 68)

if(any(table(temp$Name, temp$isPlayoffs)>1)){
  # DO NOTHING
} else {
  dbAppendTable(con, "skaterHistory", temp)  
}

# dbWriteTable(con, "skaterHistory", historySkaterSeason, overwrite = TRUE)

## Fixing spelling errors on player names

dbExecute(con, "UPDATE skaterHistory 
          SET franchiseID = 16 
          WHERE newTeamID = 23")

##################################################################
##                        Goalie History                        ##
##################################################################
goalieHistory <-
  tbl(con, "goalieHistory") %>%
  collect() 

table(goalieHistory$leagueID, goalieHistory$Season)

historyUpdate <- function(leagueId, season){
  goalieHistory <-
    tbl(con, "goalieHistory") %>%
    collect()
  
  if(goalieHistory %>% filter(leagueID == leagueId & Season == season) %>% nrow() != 0){
    stop("The current season for the league is already present in the data")
  }
  
  data <-
    indStatsLoader(leagueId, season, type = "regular") %>%
    lapply(
      X = .,
      FUN = function(x){
        do.call(what = data.frame, args = x) %>%
          ## Adds indication that stats are regular season
          mutate(
            type = 0
          ) %>%
          relocate(
            type,
            .after = season
          )
      }
    ) %>%
    append(
      indStatsLoader(leagueId, season, type = "playoffs") %>%
        lapply(
          X = .,
          FUN = function(x){
            do.call(what = data.frame, args = x) %>%
              ## Adds indication that stats are playoffs
              mutate(
                type = 1
              ) %>%
              relocate(
                type,
                .after = season
              )
          }
        )
    ) %>%
    do.call(what = plyr::rbind.fill, args = .) %>%
    # mutate(
    #   id = id
    # ) %>%
    dplyr::rename(
      fhmID = id
    ) %>%
    left_join(
      teamInfo %>%
        select(
          franchiseID,
          leagueID,
          newTeamID = teamID,
          abbr
        ),
      by = c("team" = "abbr", "league" = "leagueID")
    ) %>%
    
    ### Removes duplicated rows where the team abbreviation has been used for older teams
    group_by(name, type) %>%
    filter(
      newTeamID == max(newTeamID)
    ) %>%
    
    filter(
      position == "G"
    ) %>% 
    dplyr::select(
      where(
        function(x) !all(is.na(x))
      )
    ) %>% 
    dplyr::select(
      franchiseID,
      goalieID = fhmID,
      Name = name,
      leagueID = league,
      newTeamID,
      Season = season,
      isPlayoffs = type,
      GamesPlayed = gamesPlayed,
      Wins = wins,
      Losses = losses,
      OvertimeLosses = ot,
      Minutes = minutes,
      Shutouts = shutouts,
      GoalsAgainst = goalsAgainst,
      ShotsAgainst = shotsAgainst,
      GameRating = gameRating,
      GAA = gaa,
      SavePct = savePct
    )
    
  return(data)
}

for(i in 0:3){
  for(j in 68){
    print(paste(i,j))
    
    temp <- historyUpdate(leagueId = i, season = j)
    
    if(any(table(temp$Name, temp$isPlayoffs)>1)){
      # DO NOTHING
    } else {
      dbAppendTable(con, "goalieHistory", temp)  
    }
  }
}

# dbExecute(con, "UPDATE goalieHistory SET leagueID = '2' WHERE leagueID = '3' AND Season < '53'")

# dbWriteTable(con, "goalieHistory", goalieHistory, overwrite = TRUE)

dbDisconnect(con)
