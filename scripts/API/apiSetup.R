
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
#################################################################

### Packages that are used

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
require(tesseract, quietly = TRUE)

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

## Changes when scientific notation is used to never
options(scipen=999)

## GitHub raw url
raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

### Loading data sets
## Current forum scrape data from Google Sheets that is automatically written every day
googlesheets4::gs4_deauth()

forumData <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
    sheet = "Daily Scrape"
  )

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

## Loads Attribute Keys
attKey <- read.csv2(paste(raw, "csv/attribute_key.csv", sep = ""))

## Loading cost for TPE
tpeCost <- read.csv2(paste(raw, "csv/tpe_cost.csv", sep = ""))

## Loading career data
historySkaterSeason <- 
  read.csv2(
    paste(raw, "csv/history_skaters.csv", sep = ""),
    sep = ",",
    dec = ".",
    fileEncoding = "UTF-8") %>% 
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

draftData <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1i6yWbsI3MO036E_4y95xqZFolsUn3oT5Sq6Jju51CSQ/edit#gid=0",
    sheet = "SHL"
  )

iihfTransfer <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1shZphSBULx7G8hYDtoUqTW6gy445_DDA6NIHqFrayLs/edit#gid=0",
    sheet = "Transfers"
  )


## Reads Achievement Data
achievementData <- 
  read.csv(
    paste(raw, "csv/history_achievements.csv", sep = ""),
    sep = ",",
    dec = ".",
    fileEncoding = "UTF-8")

## Loading team information
teamInfo <- 
  read.csv2(
    paste(
      raw, 
      "csv/team_information.csv", 
      sep = "")
  ) %>% 
  mutate(
    logoImage = 
      case_when(
        team == "Calgary Dragons" ~ image_read_svg(paste(raw, "graphics/Calgary.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Hamilton Steelhawks" ~ image_read_svg(paste(raw, "graphics/Hamilton.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Manhattan Rage" ~ image_read_svg(paste(raw, "graphics/Manhattan.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Toronto North Stars" ~ image_read_svg(paste(raw, "graphics/Toronto.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Los Angeles Panthers" ~ image_read_svg(paste(raw, "graphics/Los_Angeles.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Edmonton Blizzard" ~ image_read_svg(paste(raw, "graphics/Edmonton.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Texas Renegades" ~ image_read_svg(paste(raw, "graphics/Texas.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "New England Wolfpack" ~ image_read_svg(paste(raw, "graphics/New_England.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Buffalo Stampede" ~ image_read_svg(paste(raw, "graphics/Buffalo.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "San Francisco Pride" ~ image_read_svg(paste(raw, "graphics/San_Francisco.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Chicago Syndicate" ~ image_read_svg(paste(raw, "graphics/Chicago.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "New Orleans Specters" ~ image_read_svg(paste(raw, "graphics/New_Orleans.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Baltimore Platoon" ~ image_read_svg(paste(raw, "graphics/Baltimore.svg", sep = ""), width = 600, height = 600) %>% list(),
        team == "Atlanta Inferno" ~ image_read_svg(paste(raw, "graphics/Atlanta.svg", sep = ""), width = 600, height = 600) %>% list(),
        team == "Minnesota Monarchs" ~ image_read_svg(paste(raw, "graphics/Minnesota.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Seattle Argonauts" ~ image_read_svg(paste(raw, "graphics/Seattle.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Tampa Bay Barracuda" ~ image_read_svg(paste(raw, "graphics/Tampa_Bay.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Winnipeg Aurora" ~ image_read_svg(paste(raw, "graphics/Winnipeg.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Montreal Patriotes" ~ image_read_svg(paste(raw, "graphics/Montreal.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Philadelphia Forge" ~ image_read_svg(paste(raw, "graphics/Philadelphia.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Vancouver Whalers" ~ image_read_svg(paste(raw, "graphics/Vancouver.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Kelowna Knights" ~ image_read_svg(paste(raw, "graphics/Kelowna.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Detroit Falcons" ~ image_read_svg(paste(raw, "graphics/Detroit.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "St. Louis Scarecrows" ~ image_read_svg(paste(raw, "graphics/St_Louis.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Anchorage Armada" ~ image_read_svg(paste(raw, "graphics/Anchorage.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Colorado Raptors" ~ image_read_svg(paste(raw, "graphics/Colorado.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Anaheim Outlaws" & Inaugural.Season == 45 ~ image_read_svg(paste(raw, "graphics/Anaheim.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Carolina Kraken" ~ image_read_svg(paste(raw, "graphics/Carolina.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Newfoundland Berserkers" ~ image_read_svg(paste(raw, "graphics/Newfoundland.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Maine Timber" ~ image_read_svg(paste(raw, "graphics/Maine.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Nevada Battleborn" ~ image_read_svg(paste(raw, "graphics/Nevada.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Quebec City Citadelles" ~ image_read_svg(paste(raw, "graphics/Quebec_City.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Austria" ~ image_read_svg(paste(raw, "graphics/Austria.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Canada" ~ image_read_svg(paste(raw, "graphics/Canada.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Czechia" ~ image_read_svg(paste(raw, "graphics/Czechia.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Finland" ~ image_read_svg(paste(raw, "graphics/Finland.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Germany" ~ image_read_svg(paste(raw, "graphics/Germany.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Great Britain" ~ image_read_svg(paste(raw, "graphics/Great_Britain.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Ireland" ~ image_read_svg(paste(raw, "graphics/Ireland.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Japan" ~ image_read_svg(paste(raw, "graphics/Japan.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Latvia" ~ image_read_svg(paste(raw, "graphics/Latvia.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Norway" ~ image_read_svg(paste(raw, "graphics/Norway.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Russia" ~ image_read_svg(paste(raw, "graphics/Russia.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Sweden" ~ image_read_svg(paste(raw, "graphics/Sweden.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Switzerland" ~ image_read_svg(paste(raw, "graphics/Switzerland.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "USA" ~ image_read_svg(paste(raw, "graphics/United_States.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Unassigned" ~ image_read_svg(paste(raw, "graphics/SHL_old.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "British Isles" ~ image_read_svg(paste(raw, "graphics/British_Isles.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "DACH" ~ image_read_svg(paste(raw, "graphics/DACH.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "North America" ~ image_read_svg(paste(raw, "graphics/North_America.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "UCORCAL" ~ image_read_svg(paste(raw, "graphics/UCORCAL.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "World" ~ image_read_svg(paste(raw, "graphics/World.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Young Guns" ~ image_read_svg(paste(raw, "graphics/Young_Guns.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Canada Red" ~ image_read_svg(paste(raw, "graphics/Canada_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Canada White" ~ image_read_svg(paste(raw, "graphics/Canada_White.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "United States Blue" ~ image_read_svg(paste(raw, "graphics/United_States_Blue.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "United States Red" ~ image_read_svg(paste(raw, "graphics/United_States_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Great Falls Grizzlies" ~ image_read_svg(paste(raw, "graphics/Great_Falls.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Regina Elk" ~ image_read_svg(paste(raw, "graphics/Regina.svg",sep = ""), width = 600, height = 600) %>% list(),
        team == "Yukon Malamutes" ~ image_read_svg(paste(raw, "graphics/Yukon.svg",sep = ""), width = 600, height = 600) %>% list(),
        TRUE ~ NA  %>% list()
      )
  )

## Loads IIHF History
historyIIHF <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1V_S72NZHtO5hxuy1nLOtAqdRmA3zz7ckTPWzUVR2REA/edit#gid=0",
    sheet = "Rankings"
  )

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


