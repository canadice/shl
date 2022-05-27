
############################################################################
############################################################################
###                                                                      ###
###                       UPDATING HISTORICAL DATA                       ###
###                                                                      ###
############################################################################
############################################################################

devtools::install_github("Canadice/shlrtools")
require(shlrtools)

require(dplyr)

require(googlesheets4)
# if(getwd() == "F:/GitHubs/shl_API"){
#   #Do Nothing
# } else {
#   setwd("F:/GitHubs/shl_API")
# }
# 
# ## Loads the API Setup
# source("scripts/API/apiSetup.R")

##---------------------------------------------------------------
##            Writing historical data to Google Sheet           -
##---------------------------------------------------------------

updateData <- function(leagueID, season){
  ## Writing data to Google Sheet for easier distribution
  # googlesheets4::gs4_auth(path = ".secrets/client_secret.json")
  
  currentSeasons <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1_Uj6LLdTZnV3wtDNSkdjb35wuztYf0q_lRR0WJ_f3K0/edit#gid=1551240927",
      sheet = leagueID %>% as.character(),
      range = "F:F"
    ) %>% 
    unique()
  
  if(any(currentSeasons == season)){
    stop("The season you want to update is already present in the data.")
  } 
  
  data <- 
    indStatsLoader(leagueID, season, type = "regular") %>%
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
      indStatsLoader(leagueID, season, type = "playoffs") %>% 
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
    do.call(what = rbind.fill, args = .) %>% 
    mutate(
      id = id
    ) %>% 
    rename(
      fhmID = id
    )
  
  googlesheets4::sheet_append(
    data,
    ss = "https://docs.google.com/spreadsheets/d/1_Uj6LLdTZnV3wtDNSkdjb35wuztYf0q_lRR0WJ_f3K0/edit#gid=1551240927",
    sheet = leagueID %>% as.character()
  )
  
}

updateData(leagueID = 0, season = 59)
updateData(leagueID = 1, season = 59)
updateData(leagueID = 2, season = 59)
updateData(leagueID = 3, season = 59)







