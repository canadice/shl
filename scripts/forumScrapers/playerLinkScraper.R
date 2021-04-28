### Scrapes all the player pages from the team page
player_scraper <- function(team){
  base_link <- "https://simulationhockey.com/"
  
  ## Reads the information on the team page
  team_page <- XML::htmlTreeParse(RCurl::getURL(team), asText = TRUE, useInternalNodes = TRUE)
  
  ## Scrapes the roster pages from the team page
  team_roster <- XML::xpathApply(doc = team_page, 
                                 path = "//strong/a[contains(., 'Roster')]/@href", 
                                 fun = function(x) as.character(x))
  
  ## As some teams have more than 20 players, the second page of the roster is added
  team_roster2 <- paste(team_roster, "&page=2", sep = "")
  
  ###### OBS! THIS MIGHT MEAN THAT THE SAME PAGE IS INCLUDED TWICE IF THE TEAM HAS AT MOST 20 PLAYERS
  
  ## Summarizes both roster links
  roster_links <- list(team_roster, team_roster2)
  
  ## Scrapes all player links from the roster pages
  player_links <- lapply(roster_links, 
                         FUN = function(roster){
                           XML::htmlTreeParse(
                             RCurl::getURL(
                               paste(base_link, 
                                     roster, sep = "")
                               ),
                             asText = TRUE, 
                             useInternalNodes = TRUE
                           ) %>% 
                             XML::xpathApply(
                               path = "//td[contains(@class,'forumdisplay_regular')]/div/span/a/@href", 
                               fun = function(x)as.character(x)
                             ) %>% 
                             str_remove_all(pattern = "&action=newpost")
                         }
  ) %>% 
    unlist()
  
  return(player_links)
}