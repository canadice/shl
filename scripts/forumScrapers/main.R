
#################################################################
##             Data Loader for player page scraper             ##
##                                                             ##
##                     Created: 2021-04-28                     ##
##                    Last edit: 2021-05-19                    ##
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

if(getwd() == "F:/GitHubs/shl_API"){
  #Do Nothing
} else {
  setwd("F:/GitHubs/shl_API")
}

### Loads the separate scripts containing created scraper functions
## Functions to find links to teams and players from main forum page
source("scripts/forumScrapers/teamLinkScraper.R")
source("scripts/forumScrapers/playerLinkScraper.R")
source("scripts/forumScrapers/userDataScraper.R")

## Functions to scrape and structure information from a player page
source("scripts/forumScrapers/rvestPlayerPageScraper.R")

### Loads file containing team information
teams <- 
  read.csv2(
    file = "csv/team_information.csv"
  )

iihfTransfer <- 
  read.csv2(
    file = "csv/iihf_transfers.csv",
    encoding = "UTF-8"
  )

### Specifies link to forum page containing all the team rosters
smjhl_link <- "https://simulationhockey.com/forumdisplay.php?fid=5"
shl_east <- "https://simulationhockey.com/forumdisplay.php?fid=8"
shl_west <- "https://simulationhockey.com/forumdisplay.php?fid=9"
prospects <- 
  "https://simulationhockey.com/forumdisplay.php?fid=63" %>% 
  c(
    .,
    paste(., "&page=2", sep = ""),
    paste(., "&page=3", sep = ""),
    paste(., "&page=4", sep = "")
  )
free_agents <- 
  "https://simulationhockey.com/forumdisplay.php?fid=43" %>% 
  c(
    .,
    paste(., "&page=2", sep = ""),
    paste(., "&page=3", sep = ""),
    paste(., "&page=4", sep = ""),
    paste(., "&page=5", sep = "")
  )

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
    c(
      .,
      c(
        prospects,
        free_agents
      ) %>% 
        sapply(FUN = prospectsFAScraper) %>% 
        unlist()
    ) %>% 
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
    ) %>% 
    ## Removes height, weight attributes as they are not used
    ## Removes duplicated position that is taken from the player info
    ## POSITION is taken from the post title
    # select(-Weight, -Height, -Position, -(Player.Type:last_col())) %>%
    left_join(
      iihfTransfer %>% 
        group_by(player) %>% 
        dplyr::filter(
          Transfer.Season == max(Transfer.Season)
        ),
      by = c("NAME" = "player")
    ) %>% 
    mutate(
      IIHF.Nation.x = 
        case_when(
          is.na(IIHF.Nation.y) ~ IIHF.Nation.x,
          TRUE ~ IIHF.Nation.y
        )
    ) %>% 
    rename(
      IIHF.Nation = IIHF.Nation.x
    ) %>% 
    select(
      -IIHF.Nation.y
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
      case_when(
        First.Name %>% is.na() ~ 
          stringi::stri_trans_general(
            NAME,
            id = "Latin-ASCII"
            ),
        TRUE ~ stringi::stri_trans_general(
          paste(First.Name,
                Last.Name, sep = " "
          ),
          id = "Latin-ASCII"
        )
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
  mutate(
    clean_name = 
      str_squish(clean_name)) %>%
  
  ## Uses standard names for positions
  ## Transforms some variables to numeric
  mutate(POSITION =
           case_when(
             POSITION %in% c("G", "Goaltender") ~ "Goalie",
             POSITION %in% c("C", "Centre") ~ "Center",
             POSITION %in% c("D", "Defence", "Defenseman") ~ "Defense",
             POSITION == "LW" ~ "Left Wing",
             POSITION %in% c("RHD", "Right Defence", "Right Defenseman", "Right Defender") ~ "Right Defense",
             POSITION %in% c("LHD", "Left Defence", "Left Defenseman", "Left Defender") ~ "Left Defense",
             POSITION %in% c("RW", "Right Winger") ~ "Right Wing",
             TRUE ~ POSITION
           ) %>% 
           factor(
             levels = 
               c(
                 "Goalie",
                 "Defense",
                 "Left Defense",
                 "Right Defense",
                 "Left Wing",
                 "Center",
                 "Right Wing"
                 )
           ),
         Posts = as.numeric(str_remove_all(Posts, pattern = "[^0-9]")),
         Threads = as.numeric(str_remove_all(Threads, pattern = "[^0-9]")),
         Reputation = as.numeric(str_remove_all(Reputation, pattern = "[^0-9]")),
         Jersey.Nr. = as.numeric(str_remove_all(Jersey.Nr., pattern = "[^0-9]"))
       ) 

### THE RESULTING DATA SET data CAN THEN BE EXPORTED VIA NORMAL EXPORT-FUNCTIONS
write.csv2(data, file = "csv/SHL_Forum_Scrape_Results.csv", row.names = FALSE, fileEncoding = "UTF-8")


# repo <- init()
# 
# git2r::status(repo)
# 
# git2r::add(repo, path = "csv/SHL Forum Scrape Results.csv")
# 
# git2r::commit(repo, message = "Updated Forum Scrape results.")
# 
# git2r::push(repo, credentials = cred)
