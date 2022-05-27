### PROSPECT SCRAPER

draftedProspectScraper <- function(link){
  ### Reads the information
  topic <- xml2::read_html(link)
  
  ###  Reading the player information from topic title             
  SHLRIGHTS <-
    teams %>% 
    dplyr::slice(
      topic %>% 
        rvest::html_nodes(".navigation") %>% 
        rvest::html_text() %>% 
        stringr::str_squish() %>% 
        dplyr::nth(1) %>% 
        stringr::str_detect(
          ## Takes team information from a separate data set 
          pattern = teams$team
        ) %>% 
        which()  
    ) %>% 
    select(team) %>% 
    unlist() %>% 
    unname()
  
  prospectPages <- 
    topic %>% 
    rvest::html_nodes(".tborder2") %>% 
    rvest::html_nodes(".inline_row") %>% 
    rvest::html_nodes(".subject_old") %>%
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") 
  
  data.frame(
    Prospect = paste("https://simulationhockey.com/", prospectPages, sep = ""),
    Rights = SHLRIGHTS
  ) %>% 
  return()
}
