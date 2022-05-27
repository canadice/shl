
#################################################################
##             Data Loader for player page scraper             ##
##                                                             ##
##                     Created: 2021-04-28                     ##
#################################################################

print(lubridate::today())

### Loads all the required packages
require(XML, quietly = TRUE)
require(parallel, quietly = TRUE)
require(fuzzyjoin, quietly = TRUE)
require(rvest, quietly = TRUE)

if(getwd() == "D:/GitHubs/shl_API"){
  #Do Nothing
} else {
  setwd("D:/GitHubs/shl_API")
}

### Loads the separate scripts containing created scraper functions
## Functions to find links to teams and players from main forum page
source("scripts/forumScrapers/teamLinkScraper.R")
source("scripts/forumScrapers/playerLinkScraper.R")
source("scripts/forumScrapers/userDataScraper.R")
source("scripts/forumScrapers/rvestProspectScraper.R")

## Functions to scrape and structure information from a player page
source("scripts/forumScrapers/rvestPlayerPageScraper.R")

## Loads the API Setup
source("scripts/API/apiSetup.R")

## Current season
currentSeason <- 
  playerLoader(0)$players %>% select(season) %>% unique() %>% unname()

### Loads file containing team information
teams <- 
  read.csv2(
    file = "csv/team_information.csv"
  )

### Specifies link to forum page containing all the team rosters
smjhl_link <- "https://simulationhockey.com/forumdisplay.php?fid=5"
shl_east <- "https://simulationhockey.com/forumdisplay.php?fid=8"
shl_west <- "https://simulationhockey.com/forumdisplay.php?fid=9"

draftedProspects <- 
  c(
    #ATL
    "https://simulationhockey.com/forumdisplay.php?fid=706",
    #BAP
    "https://simulationhockey.com/forumdisplay.php?fid=601",
    #BUF
    "https://simulationhockey.com/forumdisplay.php?fid=595",
    #HAM
    "https://simulationhockey.com/forumdisplay.php?fid=596",
    #MAN
    "https://simulationhockey.com/forumdisplay.php?fid=597",
    #MTL
    "https://simulationhockey.com/forumdisplay.php?fid=744",
    #NEW
    "https://simulationhockey.com/forumdisplay.php?fid=599",
    #PHI
    "https://simulationhockey.com/forumdisplay.php?fid=743",
    #TBB
    "https://simulationhockey.com/forumdisplay.php?fid=607",
    #TOR
    "https://simulationhockey.com/forumdisplay.php?fid=600",
    #CGY
    "https://simulationhockey.com/forumdisplay.php?fid=603",
    #CHI
    "https://simulationhockey.com/forumdisplay.php?fid=636",
    #EDM
    "https://simulationhockey.com/forumdisplay.php?fid=604",
    #LAP
    "https://simulationhockey.com/forumdisplay.php?fid=605",
    #MIN
    "https://simulationhockey.com/forumdisplay.php?fid=598",
    #NOLA
    "https://simulationhockey.com/forumdisplay.php?fid=641",
    #SFP
    "https://simulationhockey.com/forumdisplay.php?fid=606",
    #SEA
    "https://simulationhockey.com/forumdisplay.php?fid=705",
    #TEX
    "https://simulationhockey.com/forumdisplay.php?fid=608",
    #WPG
    "https://simulationhockey.com/forumdisplay.php?fid=602"
  ) %>% 
  lapply(
    X = .,
    FUN = draftedProspectScraper
  ) %>% 
  do.call(
    what = rbind,
    args = .
  )
  

undraftedProspects <- 
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
    paste(
      .,
      paste("&page=", 2:35, sep = ""),
      sep = ""
    )
  )

smjhl_fa <- 
  "https://simulationhockey.com/forumdisplay.php?fid=64" %>% 
  c(
    .,
    paste(
      .,
      paste("&page=", 2:25, sep = ""),
      sep = ""
    )
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
        undraftedProspects,
        free_agents,
        smjhl_fa
      ) %>% 
        sapply(FUN = prospectsFAScraper) %>% 
        unlist()
    ) %>% 
    unique() %>% 
    stringi::stri_remove_na()
  
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
  
  data <- 
    data %>% 
    ## Removes height, weight attributes as they are not used
    ## Removes duplicated position that is taken from the player info
    ## POSITION is taken from the post title
    # select(-Weight, -Height, -Position, -(Player.Type:last_col())) %>%
    left_join(
      iihfTransfer %>% 
        group_by(player) %>% 
        dplyr::filter(
          `Transfer Season` == max(`Transfer Season`)
        ),
      by = c("NAME" = "player")
    ) %>% 
    mutate(
      IIHF.Nation = 
        case_when(
          is.na(`IIHF Nation`) ~ IIHF.Nation,
          TRUE ~ `IIHF Nation`
        )
    ) %>% 
    select(
      -`IIHF Nation`
    ) %>% 
    left_join(
      draftedProspects,
      by = c("LINK" = "Prospect")
    ) %>% 
    mutate(
      SHL.Team = 
        case_when(
          !is.na(Rights) & SHL.Team != Rights ~ Rights,
          TRUE ~ SHL.Team
        )
    ) %>% 
    select(-Rights)
  
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
             POSITION == "W" ~ "Winger",
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
                 "Right Wing",
                 "Winger"
                 )
           ),
         NAME=str_remove(NAME, pattern = " \\*"),
         Posts = as.numeric(str_remove_all(Posts, pattern = "[^0-9]")),
         Threads = as.numeric(str_remove_all(Threads, pattern = "[^0-9]")),
         Reputation = as.numeric(str_remove_all(Reputation, pattern = "[^0-9]")),
         Jersey.Nr. = as.numeric(str_remove_all(Jersey.Nr., pattern = "[^0-9]"))
       ) 


############################################################################
############################################################################
###                                                                      ###
###                     EXPORTING MULTIPLE DATA SETS                     ###
###                                                                      ###
############################################################################
############################################################################

write.csv2(data, file = "csv/SHL_Forum_Scrape_Results.csv", row.names = FALSE, fileEncoding = "UTF-8")

## Writing data to Google Sheet for easier distribution
googlesheets4::gs4_auth(path = ".secrets/client_secret.json")

# Copies the old data to an earlier sheet to have 1 day backups.
try(
  expr = 
    {
      googlesheets4::sheet_copy(
        from_ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
        from_sheet = "Daily Scrape",
        to_sheet = paste(today()-1, "Scrape"),
        .after = "Daily Scrape"
      )
    },
  silent = TRUE
)

# Tries to delete the 2 day old backup
try(
  expr = 
    {
      googlesheets4::sheet_delete(
        ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
        sheet = paste(today()-2, "Scrape")
      )
    },
  silent = TRUE
)

# Writes the current scrape data to the sheet
googlesheets4::write_sheet(
  data = data,
  ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
  sheet = "Daily Scrape"
)


##----------------------------------------------------------------
##              Writing Index data to Google Sheet               -
##----------------------------------------------------------------

playerAtt <- 
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
  )

googlesheets4::write_sheet(
  data = playerAtt,
  ss = "https://docs.google.com/spreadsheets/d/1X4OUzgO_GYkXkF50GrIpRIEYeY54xgEbPoswgWYTuy8/edit?usp=sharing",
  sheet = "Index Attributes"
)

##---------------------------------------------------------------
##                  Writing TPE to Google Sheet                 -
##---------------------------------------------------------------

## Only updates the sheet on Mondays after all updates have been processed
if(today() %>% wday() == 2){
  oldTPE <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/19ZNK648EIxjKgeHtazhIMzvJEXVlWi7QsQ5Y2lyYWFM/edit#gid=0",
      sheet = "Highest Peak TPE"
    )
  
  TPE <-   
    data %>%  
    select(
      NAME,
      USER,
      TPE
    ) %>% 
    mutate(
      Season = 
        paste(
          "S",
          currentSeason,
          sep = ""
        )
    )
  
  oldTPE %>% 
    full_join(
      TPE,
      by = c("NAME"), 
      suffix = c(".old", ".new")
    ) %>% 
    mutate(
      USER = if_else(TPE.old <= TPE.new, USER.new, USER.old),
      TPE = if_else(TPE.old <= TPE.new, TPE.new, TPE.old),
      Season = if_else(TPE.old <= TPE.new, Season.new, Season.old %>% as.character())
    ) %>% 
    select(
      -contains(c("old", "new"))
    ) %>% 
    filter(
      !is.na(TPE)
    ) %>% 
    arrange(
      -TPE
    ) %>% 
    googlesheets4::write_sheet(
      ss = "https://docs.google.com/spreadsheets/d/19ZNK648EIxjKgeHtazhIMzvJEXVlWi7QsQ5Y2lyYWFM/edit#gid=0",
      sheet = "Highest Peak TPE"
    )
}
















