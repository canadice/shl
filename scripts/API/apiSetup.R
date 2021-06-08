
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-06-08                    ##
#################################################################

### Packages that are used
## API packages
require(httr)
require(jsonlite)

## Packages for reading urls
require(rvest)
require(RCurl)

## Data processing
require(tidyr)
require(plyr)
require(dplyr)
require(janitor)
require(tibble)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)
require(ggpubr)
require(png)
require(grid)

# Packages for svg images
require(magick)
require(rsvg)

## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)

## Loading package that can talk to Google Sheets
require(googlesheets4)

## Changes when scientific notation is used to never
options(scipen=999)

## GitHub raw url
raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

### Loading data sets
## Loads Attribute Keys
attKey <- read.csv2(paste(raw, "csv/attribute_key.csv", sep = ""))

## Loading cost for TPE
tpeCost <- read.csv2(paste(raw, "csv/tpe_cost.csv", sep = ""))

## Loading team information
teamInfo <- read.csv2(paste(raw, "csv/team_information.csv", sep = "")) %>% 
  mutate(
    logoImage = 
      list(
        image_read_svg(paste(raw, "graphics/Atlanta.svg", sep = "")),
        image_read_svg(paste(raw, "graphics/Baltimore.svg", sep = "")),
        image_read_svg(paste(raw, "graphics/Buffalo.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Chicago.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Hamilton.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Manhattan.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/New_England.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Tampa_Bay.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Toronto.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Calgary.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Edmonton.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Los_Angeles.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Minnesota.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/New_Orleans.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/San_Francisco.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Seattle.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Texas.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Winnipeg.svg",sep = "")),
        NA, #Old Winnipeg Jets
        image_read_svg(paste(raw, "graphics/Carolina.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Detroit.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Maine.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Newfoundland.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Quebec_City.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/St_Louis.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Anaheim.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Anchorage.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Colorado.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Kelowna.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Nevada.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Vancouver.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Montreal.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Philadelphia.svg",sep = ""))
      )
  )

### Creates functions that are used multiple times 
readAPI <- function(url, ...){
  temp <- 
    url %>% 
    # Gets the API information, the ... allows for specific queries with query = list()
    GET(...) 
  
  temp$content %>% 
    # Extracts the data
    rawToChar() %>% 
    # Converts it from JSON to a data frame
    fromJSON() %>% 
    return()
}

playerLoader <- function(leagueID, season = NULL){
  players <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/ratings",
      query = list(league = leagueID, season = season)
    ) 
  goalies <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/ratings",
      query = list(league = leagueID, season = season)
    ) 
  
  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all players
    players %>% 
    ## Selects only names (for grouping) and attributes
    select(
      name,
      team,
      screening:professionalism
    ) %>% 
    ## Creates a attribute column and value column for each player
    pivot_longer(
      cols = screening:professionalism,
      names_to = "attribute"
    ) %>% 
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>% 
    ## Groups by name for summarizing
    group_by(
      name,
      team
    ) %>% 
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 11 starting Stamina
      usedTPE = sum(TPE) - 62*5 - 16
    ) %>% 
    ungroup() %>% 
    select(name, usedTPE)
  
  players <-
    players %>% 
    left_join(
      usedTPE,
      by = c("name")
    )
  
  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all goalies
    goalies %>% 
    ## Selects only names (for grouping) and attributes
    select(
      name,
      team,
      blocker:professionalism
    ) %>% 
    ## Creates a attribute column and value column for each player
    pivot_longer(
      cols = blocker:professionalism,
      names_to = "attribute"
    ) %>% 
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>% 
    ## Groups by name for summarizing
    group_by(
      name,
      team
    ) %>% 
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 8 Aggression
      usedTPE = sum(TPE) - 62*3 - 4
    ) %>% 
    ungroup() %>% 
    select(name, usedTPE)
  
  goalies <-
    goalies %>% 
    left_join(
      usedTPE,
      by = c("name")
    ) %>% 
    dplyr::rename(
      goaliePassing = passing,
      goaliePuckhandling = puckhandling,
      goaliePositioning = positioning
    )
  
  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>% 
    return()
}


indStatsLoader <- function(leagueID, season = NULL, type = NULL){
  players <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/stats",
      query = list(league = leagueID, season = season, type = type)
    ) 
  goalies <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/stats",
      query = list(league = leagueID, season = season, type = type)
    ) 
  
  players <-
    players %>% 
    mutate(
      ## Using the format with glue grammar that allows for dynamic variable names
      across(
        contains("TimeOnIce"),
        ~ format(
          as.POSIXct(
            .x/gamesPlayed, 
            origin = "1970-01-01"
          ), 
          "%M:%S"
        )
      )
    )
    
  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>% 
    return()
}





teamLoader <-  function(leagueID, season = NULL){
  teams <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/teams",
      query = list(league = leagueID, season = season)
    ) 
  
  return(teams)
}

gamesLoader <- function(leagueID, season = NULL){
  schedule <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/schedule",
      query = list(league = leagueID, season = season)
    ) 
  
  return(schedule)
}

standingsLoader <- function(leagueID, season = NULL){
  standings <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/standings",
      query = list(league = leagueID, season = season)
    ) 
  
  return(standings)
}






