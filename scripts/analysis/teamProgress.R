
#################################################################
##                        Team Progress                        ##
##                                                             ##
##                     Created: 2021-05-02                     ##
##                    Last edit: 2021-05-02                    ##
#################################################################

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")

standings <- 
  standingsLoader(0) %>% 
  left_join(
    teamLoader(0),
    by = "id"
  ) %>% 
  arrange(conference, division, position)




