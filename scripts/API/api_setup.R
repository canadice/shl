
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-04-07                    ##
#################################################################

### Packages that are used
## API packages
require(httr)
require(jsonlite)

## Data processing
require(tidyr)
require(dplyr)

### Loads Attribute Keys
attKey <- read.csv2("csv/attribute_key.csv")
tpeCost <- read.csv2("csv/tpe_cost.csv")
teamInfo <- read.csv2("csv/team_information.csv")

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
      query = list(league = leagueID)
    ) 
  goalies <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/ratings",
      query = list(league = leagueID)
    ) 
  
  players <-
    players %>% 
    mutate(
      ## Calculates the used TPE for each player based on their attribute values
      usedTPE = 
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
        summarize(
          ## Removes the fixed attributes to 15 and compensates for 11 starting Stamina
          usedTPE = sum(TPE) - 62*4 - 16
          
          ### TEAM PLAYER ATTRIBRUTE IS MISSING FROM THE API
          
        ) %>% 
        ungroup() %>% 
        select(usedTPE) %>% 
        unlist()
    )
  
  goalies <- 
    goalies %>% 
    mutate(
      ## Calculates the used TPE for each player based on their attribute values
      usedTPE = 
        ## Starts with all players
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
        summarize(
          ## Removes the fixed attributes to 15 and compensates for 8 Aggression
          usedTPE = sum(TPE) - 62*3 - 4
        ) %>% 
        ungroup() %>% 
        select(usedTPE) %>% 
        unlist()
    )
  
  ## Return a list of the loaded data
  list(
    players,
    goalies
  ) %>% 
    return()
}

