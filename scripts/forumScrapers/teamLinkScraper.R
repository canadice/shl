### Function that scrapes all teams present in the league forum link
team_scraper <- function(league){
  base_link <- "https://simulationhockey.com/"
  
  ## Reads the information from the league page
  main <- XML::htmlTreeParse(RCurl::getURL(league), asText = TRUE, useInternalNodes = TRUE)
  
  ## Scrapes the links to all the team pages
  team_forums <- XML::xpathApply(doc = main, 
                                 path = "//span[@style=\"font-size: 14px;\"]/strong/a/@href", 
                                 fun = function(x) as.character(x)) %>% 
    unlist()
  
  ## Pastes the full links to the team pages
  team_links <- paste(base_link, team_forums, sep = "")
  
  return(team_links)
}