
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

teamData <- 
  teamData %>% 
  add_case(
    franchiseID = 111,
    teamID = 116,
    fhmID = 8,
    team = "Independent Russia",
    Inaugural.Season = 70,
    leagueID = 2,
    league = "IIHF",
    abbr = "RUS",
    primary = "#0083d6",
    secondary = "#ffffff"
      )


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
      teamData %>%
        # group_by(franchiseID) %>% 
        # filter(Inaugural.Season == max(Inaugural.Season)) %>% 
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

for(i in 0:3){
  for(j in 75){
    print(paste(i,j))
    
    temp <- historyUpdate(leagueId = i, season = j)
    
    if(any(table(temp$Name, temp$isPlayoffs)>1)){
      # DO NOTHING
    } else {
      dbAppendTable(con, "skaterHistory", temp)  
    }
  }
}


# dbWriteTable(con, "skaterHistory", historySkaterSeason, overwrite = TRUE)

## Fixing spelling errors on player names

# dbExecute(con, "UPDATE goalieHistory 
#           SET newTeamID = 106
#           WHERE teamID = 60")

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
      teamData %>%
        # group_by(franchiseID) %>% 
        # filter(Inaugural.Season == max(Inaugural.Season)) %>% 
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
  for(j in c(75)){
    print(paste(i,j))
    
    temp <- historyUpdate(leagueId = i, season = j)
    
    if(any(table(temp$Name, temp$isPlayoffs)>1)){
      # DO NOTHING
    } else {
      dbAppendTable(con, "goalieHistory", temp)  
    }
  }
}

# dbExecute(con, 'UPDATE goalieHistory SET Name = "Olli O\'Koivu" WHERE Name = "Kyle Wahlgren"')

# dbWriteTable(con, "goalieHistory", goalieHistory, overwrite = TRUE)

# write.csv(skaterHistory, file = "SHL SKATER CAREER.csv", row.names = FALSE)
# write.csv(goalieHistory, file = "SHL GOALIE CAREER.csv", row.names = FALSE)

dbDisconnect(con)
