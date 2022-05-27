
#################################################################
##                       Player Progress                       ##
##                                                             ##
##                     Created: 2021-05-02                     ##
##                    Last edit: 2021-05-02                    ##
#################################################################

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")

players <- 
  indStatsLoader(0, season = 53)$players
  

playerStats <- 
  lapply(
    X = 53:62,
    FUN = function(x){
      indStatsLoader(0, season = x, type = "rs")$players %>% 
        do.call(data.frame, .)
    }
  ) %>% 
  do.call(
    what = rbind,
    args = .
  )
  
playerAttributes <- 
  lapply(
    X = 53:62,
    FUN = function(x){
      playerLoader(0, season = x)$players
    }
  ) %>% 
  do.call(
    what = rbind,
    args = .
  )

playerData <- 
  playerAttributes %>% 
  full_join(
    playerStats %>% 
      select(
        -league,
        -name,
        -team,
        -position
      ),
    by = c("id", "season")
  )

write.csv2(
  playerData, 
  file = "temp.csv", 
  row.names = FALSE,
  fileEncoding = "UTF-8"
 )


