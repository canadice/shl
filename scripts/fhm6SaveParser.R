
###########################################################################
###########################################################################
###                                                                     ###
###                      FHM6 EXPORTED DATA PARSER                      ###
###                                                                     ###
###                         CREATED: 2021-01-19                         ###
###                        LAST EDIT: 2021-04-18                        ###
###                                                                     ###
###########################################################################
###########################################################################


### Loads required packages
require(tibble)
require(tidyr)
require(dplyr)

options(scipen = 999)

fhm6Parser <- function(
  ### Add the path to your document folder
  saveFolder,
  ### The name of the save game
  saveGame
  ){
  
  ### The csv-files are always located in import_export/csv
  csvFolder <- "/import_export/csv"
  
  
  ### Game path (taken from function input)
  # saveGame <- "WJC_S57_Line1-Test1.lg"
    
  
  ### Pastes all folder paths and sets working directory where all files are found
  dir <- 
    paste(
      saveFolder,
      saveGame,
      csvFolder,
      sep = ""
    )
  
  t <- try(setwd(dir))
  if("try-error" %in% class(t)) return(NULL)
  
  ##----------------------------------------------------------------
  ##                League and Teams Information                   -
  ##----------------------------------------------------------------
  
  leagueData <- 
    read.csv2("league_data.csv", sep = ";")
  
  conference <- 
    read.csv2("conferences.csv", sep = ";")
  
  divisions <- 
    read.csv2("divisions.csv", sep = ";")
  
  teamsData <- 
    read.csv2("team_data.csv", sep = ";") %>% 
    select(
      !contains("Parent")
    )
  
  teamsRecords <- 
    read.csv2("team_records.csv", sep = ";")
  
  TEAMINFO <- 
    teamsData %>% 
    ### Joins league data (names)
    left_join(
      leagueData %>% 
        rename(
          League = Name,
          LeagueAbbr = Abbr
        ),
      by = c("LeagueId")
    ) %>% 
    ### Joins conference data (names)
    left_join(
      conference %>% 
        select(
          Conference.Id,
          Name
        ) %>% 
        rename(
          Conference = Name
        ),
      by = c("Conference.Id")
    ) %>% 
    ### Joins divisional data (names)
    left_join(
      divisions %>% 
        select(
          Division.Id,
          Conference.Id,
          Name
        ) %>% 
        rename(
          Division = Name
        ),
      by = c("Division.Id", "Conference.Id")
    ) %>% 
    ### Joins team record data
    left_join(
      teamsRecords,
      by = c("TeamId" = "Team.Id")
    ) %>% 
    relocate(
      contains("Id"),
      .before = "Name"
    ) %>% 
    relocate(
      League,
      LeagueAbbr,
      Conference,
      Division,
      .before = "Name"
    ) %>% 
    mutate(
      sim = saveGame
    )
  
  ### Clears workspace
  rm(
    list = 
      c(
        "leagueData",
        "conference",
        "divisions",
        "teamsRecords",
        "teamsData" 
      )
  )
  
  ##----------------------------------------------------------------
  ##                  Parse schedule information                   -
  ##----------------------------------------------------------------
  
  SCHEDULE <- 
    read.csv2("schedules.csv", sep = ";") %>% 
    left_join(
      TEAMINFO %>% 
        select(
          TeamId,
          Abbr
        ),
      by = c("Home" = "TeamId")
    ) %>% 
    rename(
      Team.Home = Abbr
    ) %>% 
    left_join(
      TEAMINFO %>% 
        select(
          TeamId,
          Abbr
        ),
      by = c("Away" = "TeamId")
    ) %>% 
    rename(
      Team.Away = Abbr
    )  %>% 
    mutate(
      sim = saveGame
    )
  
  ##----------------------------------------------------------------
  ##                      Player information                       -
  ##----------------------------------------------------------------
  
  playersInfo <- 
    read.csv2("player_master.csv", sep = ";") 
  
  playerRatings <-
    read.csv2("player_ratings.csv", sep = ";")
  
  playerInfo <- 
    playersInfo %>% 
    left_join(
      playerRatings %>% 
        mutate(
          ## Gets the column value (position) with the highest compatibility, indicating their position
         Pos = 
           playerRatings %>% 
           select(
             G, LD, RD, LW, C, RW
           ) %>% 
           rownames_to_column() %>%
           pivot_longer(
             names_to = "column", 
             values_to = "value", 
             -rowname
             ) %>%
           group_by(rowname) %>% 
           filter(
             rank(
               -value
               ) == 1
             ) %>% 
           ungroup() %>% 
           pull(column)
        ) %>% 
        select(
          -c(G, LD, RD, LW, C, RW)
        ),
      by = c("PlayerId")
    ) %>% 
    relocate(
      Pos,
      .before = Height
    )
  
  currentPlayerStatsPre <- 
    read.csv2("player_skater_stats_ps.csv") %>% 
    mutate(
      Type = "Pre-Season"
    )
  
  currentPlayerStatsReg <- 
    read.csv2("player_skater_stats_rs.csv") %>% 
    mutate(
      Type = "Regular Season"
    )
  
  currentPlayerStatsPost <- 
    read.csv2("player_skater_stats_po.csv") %>% 
    mutate(
      Type = "Post-Season"
    )
  
  PLAYERSTATS <- 
    ### Joins all player stats from the pre, regular and post season
    currentPlayerStatsPre %>% 
    full_join(
      currentPlayerStatsReg
    )
    
  if(nrow(currentPlayerStatsPost) != 0){
    PLAYERSTATS <- 
      PLAYERSTATS %>% 
      full_join(
        currentPlayerStatsPost
      )   
  }
    
  PLAYERSTATS <- 
    PLAYERSTATS %>% 
    ### Adds on player information
    left_join(
      playerInfo
    ) %>% 
    mutate(
      sim = saveGame,
      P = G+A,
      PDO = PDO %>% as.numeric(),
      across(contains("60"), as.numeric),
      across(contains("rel"), as.numeric),
      `CF.` = `CF.` %>% as.numeric(),
      `FF.` = `FF.` %>% as.numeric()
    ) %>% 
    rename(
      `+/-` = `X...`
    ) %>% 
    relocate(
      P,
      .after = A
    ) %>% 
    relocate(
      Last.Name,
      .after = PlayerId
    ) %>% 
    relocate(
      sim,
      .before = PlayerId
    ) %>% 
    select(
      -FranchiseId,
      -First.Name,
      -Nick.Name
    )
  
  
  return(
    list(
      teams = TEAMINFO,
      schedule = SCHEDULE,
      players = PLAYERSTATS
      )
  )
}

fhm6Aggregator <- function(
  ### Add the path to your document folder
  saveFolder,
  ### The BASE name of the save game
  saveGame,
  ### Specifies the number of test sims present with the same base name
  ##  Defaults to 10
  nSims = 10
){
  
  if(saveFolder %>% class() != "character"){
    stop("The saveFolder needs to be a string.")
  }
  
  if(saveGame %>% class() != "character"){
    stop("The saveGame needs to be a string.")
  }
  
  allParsedData <- 
    ### Uses parallell computation to speed up the process
    lapply(
      ### Creates a vector of names using the base name followed by the number
      X = 
        paste(
          saveGame,
          1:nSims %>% as.character(),
          ".lg",
          sep = ""
        ),
      FUN = fhm6Parser,
      saveFolder = saveFolder
    )
  
  return(allParsedData)
}

teamParserRaw <- function(
  ### Adds the parsed fhm6 data
  fhm6Data
){
  
  teamData <- 
    fhm6Data %>%
    ### Filters out the team data
    lapply(
      FUN = function(x){
        x$teams 
      }
    ) %>% 
    do.call(
      rbind,
      args = .
    ) %>% 
    select(
      TeamId,
      Conference.Id,
      Division.Id,
      Name,
      Nickname,
      Abbr,
      Wins,
      Losses,
      Ties,
      OTL,
      contains("Shootout"),
      Points, 
      contains("Goals"),
      sim
    )
  
  return(teamData)
}

casinoLines <- function(){
  teamCasino <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1kisvxMASJvX26djRXzDVDxMz1ODqbaaMfuwUXtVwpWw/edit#gid=1074258598",
      sheet = "Teams",
      range = "A:D"
    )
  return(teamCasino)
}
  
casinoAggregator <- function(
  ### Adds the raw data for each team
  teamRaw,
  ### Adds the casino lines
  teamCasino
){
  
  casinoPredictions<-
    teamCasino %>% 
    full_join(
      teamRaw,
      by = c("TeamId")
    ) %>% 
    arrange(
      Conference.Id,
      Division.Id
    ) %>% 
    group_by(
      Conference.Id,
      Division.Id,
      TeamId,
      Team
    ) %>% 
    summarize(
      casino = mean(Casino),
      nTests = n(),
      over = sum(Wins > Casino),
      under = sum(Wins < Casino),
      meanW = mean(Wins) %>% round(2),
      meanPts = mean(Points) %>% round(2),
      sdW = sd(Wins) %>% round(2),
      medianW = median(Wins),
      lowQuartW = quantile(Wins, probs = 0.25),
      uppQuartW = quantile(Wins, probs = 0.75)
    ) %>% 
    ungroup() %>% 
    select(
      -Conference.Id,
      -Division.Id
    )
  
  return(casinoPredictions)
}

teamAggregator <- function(
  ### Adds the raw data for each team
  teamRaw,
  ### Which function you want to use
  fun = mean
){
  
  teamAverages <-
    teamRaw %>% 
    arrange(
      Conference.Id,
      Division.Id
    ) %>% 
    group_by(
      Conference.Id,
      Division.Id,
      TeamId,
      Name,
      Nickname,
      Abbr
    ) %>% 
    summarize(
      .groups = "keep",
      nTests = n(),
      across(
        where(is.integer),
        .fns = fun  
      )
    ) %>% 
    ungroup() %>% 
    select(
      -Conference.Id,
      -Division.Id
    )
  
  return(teamAverages)
}

rawDataWriter <- function(data, file, sheet){
  googlesheets4::write_sheet(
    data,
    ss = file,
    sheet = sheet)
}

casinoDataWriter <- function(data, file, sheet){
  googlesheets4::write_sheet(
    data,
    ss = file,
    sheet = sheet)
}

