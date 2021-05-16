
#################################################################
##             Data Loader for player page scraper             ##
##                                                             ##
##                     Created: 2021-04-28                     ##
##                    Last edit: 2021-04-28                    ##
#################################################################

### Loads all the required packages
require(XML)
require(ggplot2)
require(stringr)
require(tidyr)
require(plyr)
require(dplyr)
require(RColorBrewer)
require(lubridate)
require(stringi)
require(parallel)
require(fuzzyjoin)
require(rvest)


### Sets the working directory to the current one
setwd("./scripts/forumScrapers")


### Loads the separate scripts containing created scraper functions
## Functions to find links to teams and players from main forum page
source("teamLinkScraper.R")
source("playerLinkScraper.R")

## Functions to scrape and structure information from a player page
source("rvestPlayerPageScraper.R")

### Loads file containing team information
teams <- 
  read.csv2(
    file = "../../csv/team_information.csv"
  )

### Specifies link to forum page containing all the team rosters
smjhl_link <- "https://simulationhockey.com/forumdisplay.php?fid=5"
shl_east <- "https://simulationhockey.com/forumdisplay.php?fid=8"
shl_west <- "https://simulationhockey.com/forumdisplay.php?fid=9"

### Parallell processing to quicken the scraping for all the specified links
##  "Quicken" does not mean quick, scraping this information once takes around 10 minutes
{
  cl <- makeCluster(getOption("cl.cores", 4))
  
  clusterExport(
    cl,
    varlist = 
      c(
        ls(),
        "%>%"
      )
  )
  
  ### Finds all unique player links from each team's roster page on the forum
  playerLinks <-
    c(
      shl_east, 
      shl_west, 
      smjhl_link
    ) %>%
    team_scraper() %>% 
    player_scraper() %>% 
    unique()
  
  ### Puts everything into a structured data.frame
  ##  The function rbind.fill allows for new columns to be created if the headers don't match perfectly
  ##  Also adds NA to the previously created column
  data <- 
    clusterApply(
      cl,
      playerLinks, 
      fun = playerScraper) %>% 
    do.call(
      args = .,
      what = plyr::rbind.fill
    )
  
  stopCluster(cl)
  
}

### Some reformatting of the data
data <- 
  data %>% 
  
  ## Create a "clean name" variable without special characters
  ## This can be used in connection with the index data
  mutate(
    clean_name =
      stringi::stri_trans_general(
        paste(First.Name,
              Last.Name, sep = " "
              ),
        id = "Latin-ASCII"
        )
    ) %>%
  
  ## These players have too long names (or other names) in FHM6 
  mutate(clean_name =
           case_when(
             clean_name == "James \"Jimmy\" Yzerman" ~ "James Yzerman",
             clean_name == "Asclepius Perseus Flitterwind" ~ "Asclepius Perseus Flitter",
             clean_name == "Hennesey-Gallchobhar O'McGuiness" ~ "Hennesey-Gallchobhar O'Mc",
             clean_name == "Terrence \"Big Terry\" Smith" ~ "Terrence Smith",
             clean_name == "Ragnar-Alexandre Ragnarsson-Tremblay" ~ "Ragnar-Alexandre Ragnarss",
             TRUE ~ clean_name
           )
         ) %>%
  
  ## Cleans up the transformation a bit
  mutate(clean_name = str_squish(clean_name)) %>%
  
  ## Uses standard names for positions
  ## Transforms some variables to numeric
  mutate(POSITION =
           case_when(
             POSITION %in% c("G", "Goaltender") ~ "Goalie",
             POSITION %in% c("C", "Centre") ~ "Center",
             POSITION %in% c("D", "Defence", "Defenseman") ~ "Defense",
             POSITION == "LW" ~ "Left Wing",
             POSITION == "RHD" ~ "Right Defense",
             POSITION %in% c("RW", "Right Winger") ~ "Right Wing",
             TRUE ~ POSITION
           ),
         Posts = as.numeric(str_remove_all(Posts, pattern = "[^0-9]")),
         Threads = as.numeric(str_remove_all(Threads, pattern = "[^0-9]")),
         Reputation = as.numeric(str_remove_all(Reputation, pattern = "[^0-9]")),
         Jersey.Nr. = as.numeric(str_remove_all(Jersey.Nr., pattern = "[^0-9]"))
       ) %>%
  
  ## Removes height, weight attributes as they are not used
  ## Removes duplicated position that is taken from the player info
  ## POSITION is taken from the post title
  select(-Weight, -Height, -Position)

### THE RESULTING DATA SET IS data THAT CAN THEN BE EXPORTED VIA NORMAL EXPORT-FUNCTIONS
##  Remember that the working directory is set to the forumScrapers folder
# write.csv2(data, file = "output.csv")