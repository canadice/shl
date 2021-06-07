
##################################################################
##                                                              ##
##                 IIHF Eligibility and Nations                 ##
##                                                              ##
##                                                              ##
##                      Created: 2021-05-18                     ##
##                    Last edit: 2021-05-18                     ##
##                                                              ##
##################################################################

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")

### Loads the data
if(paste("csv/SHL", today(), ".csv", sep = "") %>% file.exists()){
  data <- read.csv2(paste("csv/SHL", today(), ".csv", sep = ""))
} else {
  ## Scrapes the forums (THIS ONE TAKES A LONG TIME)
  source("scripts/forumScrapers/main.R")
}

### Loading the WJC HO Eligibility Sheet
eligible <- 
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1MEi_TGvY_PBUB8I2_gF2oOc1MuXqVZVdzn5JhboEtpk/edit#gid=1987539221",
    range = "Sweden!A:G"
    )

### Loading the Role Rating Matrix sheet
## The Role Rating Matrix is a matrix of weights for each attribute importance for each player role
roleKey <- 
  read_sheet(
    "https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
    sheet = "Role Weight Matrix") %>% 
  column_to_rownames(var = "Role") %>% 
  apply(MARGIN = 1, FUN = function(x)x/sum(x))

### Loading the latest season stats
stats <- 
  indStatsLoader(0)$players %>% 
  add_row(
    indStatsLoader(1)$players
  )

### Loading the latest season attribute ratings
ratings <- 
  playerLoader(0)$players %>% 
  add_row(
    playerLoader(1)$players
  )

### Does the same for goalies
goalieStats <- 
  indStatsLoader(0)$goalies %>% 
  add_row(
    indStatsLoader(1)$goalies
  )

goalieRatings <- 
  playerLoader(0)$goalies %>% 
  add_row(
    playerLoader(1)$goalies
  )

##---------------------------------------------------------------
##                        Data processing                       -
##---------------------------------------------------------------

### Summarizes and combines data for all the players in the leagues
allPlayers <- 
  ## Uses data scraped from the forums
  data %>% 
  
  ## Filters out all goalies
  filter(
    POSITION != "Goalie"
  ) %>% 
  
  ## Adds on stats and ratings from latest season of SHL and SMJHL
  ## Uses a fuzzy join based on the player name with a max difference of 2
  stringdist_left_join(
    stats,
    by = c("clean_name" = "name"),
    max_dist = 2
  ) %>% 
  stringdist_left_join(
    ratings,
    by = c("clean_name" = "name"),
    max_dist = 2
  ) %>% 
  
  ## Checks the WJC eligibility of players
  mutate(
    wjcEligible = 
      case_when(
        (league == 0 & gamesPlayed > 0) ~ "No",
        TRUE ~ "Yes"
      )
  ) %>% 
  
  ## Reorders ratings so as to conform with the Role Matrix
  relocate(
    aggression,
    .after = last_col()
    ) %>% 
  relocate(
    determination,
    .after = last_col()
  ) %>% 
  relocate(
    teamPlayer,
    .after = last_col()
  ) %>% 
  relocate(
    leadership,
    .after = last_col()
  ) %>% 
  relocate(
    professionalism,
    .after = last_col()
  ) %>% 
  relocate(
    bravery,
    .after = last_col()
  ) %>% 
  relocate(
    temperament,
    .after = last_col()
  ) %>% 
  relocate(
    acceleration,
    .after = last_col()
  ) %>% 
  relocate(
    agility,
    .after = last_col()
  ) %>% 
  relocate(
    balance,
    .after = last_col()
  ) %>% 
  relocate(
    speed,
    .after = last_col()
  ) %>% 
  relocate(
    stamina,
    .after = last_col()
  ) %>% 
  relocate(
    strength,
    .after = last_col()
  ) %>% 
  relocate(
    fighting,
    .after = last_col()
  ) %>% 
  relocate(
    screening,
    .after = last_col()
  ) %>% 
  relocate(
    gettingOpen,
    .after = last_col()
  ) %>% 
  relocate(
    passing,
    .after = last_col()
  ) %>% 
  relocate(
    puckHandling,
    .after = last_col()
  ) %>% 
  relocate(
    shootingAccuracy,
    .after = last_col()
  ) %>% 
  relocate(
    shootingRange,
    .after = last_col()
  ) %>% 
  relocate(
    offensiveRead,
    .after = last_col()
  ) %>% 
  relocate(
    checking,
    .after = last_col()
  ) %>% 
  relocate(
    faceoffs,
    .after = last_col()
  ) %>% 
  relocate(
    hitting,
    .after = last_col()
  ) %>% 
  relocate(
    positioning,
    .after = last_col()
  ) %>% 
  relocate(
    shotBlocking,
    .after = last_col()
  ) %>% 
  relocate(
    stickChecking,
    .after = last_col()
  ) %>% 
  relocate(
    defensiveRead,
    .after = last_col()
  ) %>% 
  
  ## Transforms all integer variables to numeric
  mutate(
    across(where(is.integer), as.numeric)
  ) %>% 
  
  ## Removes rows with all NA (should not exist)
  select(
    where(
      ~ sum(!is.na(.x))>0
      )
  ) %>% 
  
  ## Calculates a player's role grade based on Role Matrix and their attributes
  cbind(
    as.matrix(.[, (ncol(.)-27):ncol(.)]) %*% (as.matrix(roleKey)) %>% 
      as.data.frame() %>% 
      apply(MARGIN = 1, FUN = function(x){
        index <- head(order(x, decreasing =TRUE), n = 5)
        
        paste(colnames(.)[index], round(x[index], 3))
      }) %>% 
      t()
  ) %>% 
  
  ## Reorders variables in the data set
  relocate(
    `usedTPE`:`5`, 
    .after = Active
  ) %>% 
  
  ## Sorts the data based on the following variables
  arrange(Active, POSITION, usedTPE) %>% 
  
  ## Selects specific variables from the entire data set 
  ## Does not currently select Advanced stats because of 
  ## problems exporting to Google Sheet. They are a data.frame...
  select(
    NAME:`5`,
    Handedness:IIHF.Nation,
    gamesPlayed:defensiveGameRating
  ) %>% 
  
  ## Filters out nations and if they are eligible for the WJC
  ## THIS IS WHAT YOU WANT TO CHANGE TO GET OTHER NATIONS
  filter(
    IIHF.Nation == "Sweden" & wjcEligible == "Yes"
  ) 

### Does the same for goalies  
goalies <- 
  data %>% 
  filter(
    POSITION == "Goalie"
  ) %>% 
  stringdist_left_join(
    goalieStats,
    by = c("clean_name" = "name"),
    max_dist = 2
  ) %>% 
  stringdist_left_join(
    goalieRatings,
    by = c("clean_name" = "name"),
    max_dist = 2
  ) %>% 
  mutate(
    wjcEligible = 
      case_when(
        (league == 0 & gamesPlayed > 0) ~ "No",
        TRUE ~ "Yes"
      )
  ) %>% 
  arrange(Active) %>% 
  filter(
    IIHF.Nation == "Sweden" & wjcEligible == "Yes"
  )


### Write the eligible players to a specific sheet
googlesheets4::write_sheet(
  allPlayers,
  ss = "https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
  sheet = "Skaters")

googlesheets4::write_sheet(
  goalies,
  ss = "https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
  sheet = "Goalies")





