### Scrapes all the player pages from the team page
user_scraper <- function(link){
  base_link <- "https://simulationhockey.com/"
  
  ### Reads the information on the user page
  topic <- xml2::read_html(link)
  
  ### Searches for the date of the user's last post
  lastPost <- 
    topic %>% 
    
    ## Finds link to all posts
    rvest::html_elements("a.button") %>% 
    dplyr::nth(2) %>% 
    rvest::html_attr("href") 
  
  if(lastPost == "#"){
    topic <- xml2::read_html(link)
    
    lastPost <- 
      topic %>% 
      
      ## Finds link to all posts
      rvest::html_elements("a.button") %>% 
      dplyr::nth(2) %>% 
      rvest::html_attr("href") 
  }
  
  lastPost <-
    lastPost %>% 
    paste(
      base_link,
      .,
      sep = ""
    ) %>% 
    
    ## Reads the information from the search results
    xml2::read_html() %>% 
    rvest::html_elements("td.hide span") %>% 
    dplyr::nth(5) %>% 
    
    ## Finds date of the last post and converts it to date format
    rvest::html_text() %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      value = 
        dplyr::case_when(
          stringr::str_detect(value, pattern = "minute") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "hour") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "Today") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "Yesterday") ~ lubridate::today()-1,
          TRUE ~ lubridate::as_date(value, format = "%m-%d-%Y")
        )
    ) 
  
  
  ### Reads table information
  table <- 
    topic %>% 
    
    ## Finds the html element with the tables
    rvest::html_elements("div#two table.tborder") %>%
    
    ## Selects the second table in the list
    dplyr::nth(1) %>% 
    
    ## Converts it (wrong) to a data.frame
    rvest::html_table() %>%
    
    ## Data wrangling to get it to correct format
    dplyr::slice(-1) %>% 
    dplyr::select(X1) %>% 
    unlist() %>% 
    
    ## Splits headers from values, looks for either : followed by info or : at the end of string
    stringr::str_split(pattern = ":[^0-9]|:$") %>% 
    unlist() %>% 
    
    ## Creates a new matrix with the headers 
    matrix(ncol = 2, byrow = TRUE) %>% 
    t() %>% 
    tibble::as_tibble(.name_repair = "minimal") %>% 
    janitor::row_to_names(1) %>% 
    
    ## Removes \n from the text and removes unnecessary white space
    dplyr::mutate_all(
      ~ stringr::str_replace_all(., pattern = "\n", replacement = "") %>%
        stringr::str_squish()
    ) %>% 
    
    ## Converts dates to a date format
    dplyr::mutate(
      `Last Visit` = 
        dplyr::case_when(
          stringr::str_detect(`Last Visit`, pattern = "minute") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "hour") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "Today") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "Yesterday") ~ lubridate::today()-1,
          TRUE ~ lubridate::as_date(`Last Visit`, format = "%m-%d-%Y")
          ),
      Joined = lubridate::as_date(Joined, format = "%m-%d-%Y"),
      `Online For` = 
        sapply(
          X = `Online For`, 
          FUN = function(x){
            if(x == "(Hidden)"){
              return(NA)
            }
            
            x <- 
              x %>% 
              stringr::str_split(",") %>% 
              unlist()
            
            year <- 
              x[stringr::str_detect(x, pattern = "Year")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(year) == 0){
              year <- 0
            }
            
            month <- 
              x[stringr::str_detect(x, pattern = "Month")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric() 
            
            if(length(month) == 0){
              month <- 0
            }
            
            week <- 
              x[stringr::str_detect(x, pattern = "Week")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(week) == 0){
              week <- 0
            }
            
            day <- 
              x[stringr::str_detect(x, pattern = "Day")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(day) == 0){
              day <- 0
            }
            
            hour <- 
              x[stringr::str_detect(x, pattern = "Hour")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(hour) == 0){
              hour <- 0
            }
            
            minute <- 
              x[stringr::str_detect(x, pattern = "Minute")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(minute) == 0){
              minute <- 0
            }
            
            second <- 
              x[stringr::str_detect(x, pattern = "Second")] %>% 
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
              as.numeric()
            
            if(length(second) == 0){
              second <- 0
            }
            
            second + 60 * (minute + 60 * (hour + 24 * (day + 7 * (week + 4.345 * (month + 12*year))))) 
          }
        )
    ) %>%  
    
    ## Checks if a user is considered IA (30 days of not posting)
    dplyr::mutate(
      `Last Post` = lastPost %>% unlist() %>% lubridate::as_date(),
      Active = 
        dplyr::case_when(
          lubridate::today() - (lastPost %>% unlist()) > 30 ~ "IA",
          TRUE ~ "Active"
        ),
      Reputation = 
        stringr::str_extract(
          Reputation, 
          pattern = "[0-9]+"
          ) %>% 
        as.numeric(),
      `Bank Balance` = 
        stringr::str_extract_all(
          `Bank Balance`, 
          pattern = "[0-9]+", 
          simplify = TRUE
          ) %>% 
        paste0(collapse = "") %>% 
        as.numeric()
    )
    
  return(table)
}
