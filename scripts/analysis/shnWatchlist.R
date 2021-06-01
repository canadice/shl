
############################################################################
############################################################################
###                                                                      ###
###                       SHN THE WATCHLIST SCRIPT                       ###
###                                                                      ###
############################################################################
############################################################################

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")
source("scripts/analysis/casinoProgress.R")
source("scripts/analysis/teamProgress.R")
source("scripts/analysis/playerProgress.R")

save(
  strength,
  currentProgress,
  teams,
  players,
  standings,
  file = "C:/Users/Canadice/Downloads/SHN Issue 2.RData"
)
