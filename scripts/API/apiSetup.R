
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
#################################################################

### Packages that are used

options(digits.secs = 3)

print(paste("Start:", Sys.time()))

## tools packages
# devtools::install_github("Canadice/shlrtools")
require(shlrtools)

## API packages
require(httr, quietly = TRUE)
require(jsonlite, quietly = TRUE)

## Packages for reading urls
require(rvest, quietly = TRUE)
require(RCurl, quietly = TRUE)

## Data processing
require(tidyr, quietly = TRUE)

if("plyr" %in% (.packages())){
  # Do nothing
} else {
  require(plyr, quietly = TRUE)  
}

require(dplyr, quietly = TRUE)
require(janitor, quietly = TRUE)
require(tibble, quietly = TRUE)
require(purrr, quietly = TRUE)

## Visualizations
require(ggplot2, quietly = TRUE)
require(ggnewscale, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)
require(cowplot, quietly = TRUE)
require(ggpubr, quietly = TRUE)
require(png, quietly = TRUE)
require(grid, quietly = TRUE)

# Packages for image recognition
# require(tesseract, quietly = TRUE)

# Packages for svg images
require(magick, quietly = TRUE)
require(rsvg, quietly = TRUE)

## Package for handling date and time
require(lubridate, quietly = TRUE)

## Packages for handling strings
require(stringr, quietly = TRUE)
require(stringi, quietly = TRUE)

## Loading package that can talk to Google Sheets
require(googlesheets4, quietly = TRUE)

print(paste("Loading packages done:", Sys.time()))

## Changes when scientific notation is used to never
options(scipen=999)

## GitHub raw url
raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

### Loading data sets
## Current forum scrape data from Google Sheets that is automatically written every day
googlesheets4::gs4_deauth()

## Loads the Forum Data from the shlrtools Github
scraperUrl <- url("https://github.com/canadice/shlrtools/blob/main/data/forumData.RData?raw=true")

load(scraperUrl)

print(paste("Forum Data done:", Sys.time()))
# ## Removed reading from the Google Sheet and instead takes the data from a Github Action process
# forumData <- 
#   googlesheets4::read_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
#     sheet = "Daily Scrape"
#   )
# ## Removed reading from csv and instead take the Google Sheet that is updated outside of the GitHub
# forumData <- 
#   read.csv2(
#     paste(
#       raw, 
#       "csv/SHL_Forum_Scrape_Results.csv", 
#       sep = ""),
#     sep = ";",
#     dec = ",",
#     fileEncoding = "UTF-8"  
#     )

## Loading career data
historySkaterSeason <- 
  # read.csv2(
  #   paste(raw, "csv/history_skaters.csv", sep = ""),
  #   sep = ",",
  #   dec = ".",
  #   fileEncoding = "UTF-8") %>% 
  
  ### Uses the internal history data from the shlrtools package
  shlrtools::historySkaterSeason %>% 
  filter(
    Name != "CPU Player",
    !(str_detect(Name, pattern = "Computer")),
    
    ## Removes records where the player has summed stats for multiple teams in a season
    teamID != ""
  )

historyGoalieSeason <- 
  read.csv2(
    paste(raw, "csv/history_goalies.csv", sep = ""),
    sep = ",",
    dec = ".",
    fileEncoding = "UTF-8") %>% 
  filter(
    Player.Name != "CPU Player",
    !(str_detect(Player.Name, pattern = "Computer")),
    
    ## Removes records where the player has summed stats for multiple teams in a season
    TeamId != ""
  )

print(paste("History done:", Sys.time()))

draftData <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1i6yWbsI3MO036E_4y95xqZFolsUn3oT5Sq6Jju51CSQ/edit#gid=0",
    sheet = "SHL"
  )

print(paste("Draft Data done:", Sys.time()))
# # Don't need this data as the transfers are run into the forum scraper
# iihfTransfer <- 
#   read_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1shZphSBULx7G8hYDtoUqTW6gy445_DDA6NIHqFrayLs/edit#gid=0",
#     sheet = "Transfers"
#   )

## Reads Achievement Data
achievementData <- 
  read.csv(
    paste(raw, "csv/history_achievements.csv", sep = ""),
    sep = ",",
    dec = ".",
    fileEncoding = "UTF-8")

print(paste("Achievements done:", Sys.time()))

## Loading team information
teamInfo <- 
  teamInfo %>% 
  addTeamLogo()

print(paste("Team Information + logos done:", Sys.time()))

## Loads IIHF History
historyIIHF <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1V_S72NZHtO5hxuy1nLOtAqdRmA3zz7ckTPWzUVR2REA/edit#gid=0",
    sheet = "Rankings"
  )

print(paste("IIHF Ranking History done:", Sys.time()))

############################################################################
############################################################################
###                                                                      ###
###                              AUDIT DATA                              ###
###                                                                      ###
############################################################################
############################################################################

## Reads attribute values from SHL Index
indexAttributes <- 
  playerLoader(0) %>% 
  do.call(what = rbind.fill, args = .) %>% 
  rbind(
    playerLoader(1) %>% 
      do.call(what = rbind.fill, args = .)
  ) %>% 
  mutate(
    name = str_trim(name),
    position = 
      factor(
        position,
        levels = 
          c(
            "C", "LW", "RW", "LD", "RD", "G"
          )
      )
  ) %>% 
  relocate(
    usedTPE,
    .after = position
  ) %>% 
  relocate(
    team,
    .before = name
  ) %>% 
  select(
    -id, -league, -season
  ) %>% 
  arrange(
    team, position, name
  ) %>% 
  mutate(
    name = 
      name %>% 
      tolower %>% 
      stringi::stri_trans_general(id = "Latin-ASCII") %>% 
      str_remove_all(pattern = "[[:punct:]]") 
  )


## Reads attribute values from Updater Spreadsheet

forumAttributes <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1dB6tn6fXEy6T-9KGHagoqr5g9WS8JUcBkCuc65-ZQGo/edit#gid=519975432",
    sheet = "Ratings"
  ) %>% 
  filter(
    !is.na(Name)
  ) %>% 
  select(
    -Issue
  )

forumGoalies <- 
  forumAttributes %>% 
  filter(
    Position == "Goalie"
  ) %>% 
  select(
    where(
      ~ sum(is.na(.x)) != length(.x)
    )
  )

colnames(forumGoalies) <- 
  c("Team", "name", "User", "IIHF", "Season", "position", "TPE", "team", 
    "blocker", "glove", "goaliePassing", "pokeCheck", "goaliePositioning", "rebound", "recovery", 
    "goaliePuckhandling", "lowShots", "reflexes", "skating", "aggression", "mentalToughness",
    "determination", "teamPlayer", "leadership", "goalieStamina", "professionalism", "Current Season")

forumPlayers <- 
  forumAttributes %>% 
  filter(
    Position != "Goalie"
  ) %>% 
  select(
    where(
      ~ sum(is.na(.x)) != length(.x)
    )
  )

colnames(forumPlayers) <- 
  c("Team", "name", "User", "IIHF", "Season", "position", "TPE", "team",
    "screening", "gettingOpen", "passing", "puckHandling", "shootingAccuracy", "shootingRange", "offensiveRead", 
    "NA", "checking", "hitting", "positioning", "stickChecking", "shotBlocking", "faceoffs", "defensiveRead",
    "NA2", "NA3", "acceleration", "agility", "balance", "speed", "stamina", "strength", "fighting", "aggression", "bravery")

forumAttributes <- 
  forumGoalies %>% 
  full_join(
    forumPlayers,
    by = c("Team", "name", "User", "IIHF", "Season", "position", "TPE", "team", "aggression")
  ) %>% 
  mutate(
    temperament = 
      case_when(
        position != "Goalie" ~ 15,
        TRUE ~ NaN
      ),
    determination = 15,
    teamPlayer = 15,
    leadership = 15,
    professionalism = 15,
    aggression = 
      case_when(
        position != "Goalie" ~ aggression,
        TRUE ~ 8
      )
  ) %>% 
  mutate(
    name = 
      name %>% 
      tolower %>% 
      stringi::stri_trans_general(id = "Latin-ASCII") %>% 
      str_remove_all(pattern = "[[:punct:]]") 
  )

forumAttributes <- 
  forumAttributes[, 
                  colnames(indexAttributes)[
                    !str_detect(colnames(indexAttributes), "TPE")]
  ]

print(paste("Audit Data done:", Sys.time()))

