playerScraper <- 
  function(player){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(player, "simulationhockey")){
      
    } else{
      baseLink <- "https://simulationhockey.com/"
      
      player <- paste(baseLink, player, sep = "")
      
    }
    
    ### Reads the information
    topic <- xml2::read_html(player)
    
    ###  Reading the player information from topic title             
    title <- 
      topic %>% 
      rvest::html_nodes("title") %>% 
      rvest::html_text() %>% 
      stringr::str_split(pattern = " - |- ") %>% 
      unlist() %>% 
      stringr::str_squish()
    
    if(length(title) == 2){
      NAME <- 
        title %>% 
        dplyr::nth(2)
      
      CLASS <-
        title %>% 
        stringr::str_extract_all(pattern = "S[0-9]+") %>% 
        unlist() %>% 
        dplyr::nth(1)
      
      if(length(CLASS)==0){
        CLASS <- "Unspecified"
      }
      
      POSITION <-
        title %>% 
        dplyr::nth(1) %>% 
        stringr::str_split(pattern = "]|\\)") %>% 
        unlist() %>% 
        stringr::str_squish() %>% 
        dplyr::nth(2)
      
    } else if(length(title) == 3){
      NAME <- 
        title %>% 
        dplyr::nth(3)
      
      CLASS <-
        title %>% 
        stringr::str_extract_all(pattern = "S[0-9]+") %>% 
        unlist() %>% 
        dplyr::nth(1)
      
      if(length(CLASS)==0){
        CLASS <- "Unspecified"
      }
      
      POSITION <-
        title %>% 
        dplyr::nth(2)
      
    } else {  
      ## If something is wrong with the title splits, return NA for each of these.
      NAME <- NA
      CLASS <- NA
      POSITION <- NA
    }
    
    ### Checks if the name includes a nickname
    if(NAME %>% is.na()){
      #Do nothing
      
      NICKNAME <- NA
    } else if (NAME %>% stringr::str_detect(pattern = "\"")){
      NICKNAME <- 
        NAME %>% 
        stringr::str_extract_all(pattern = "\"[A-z ]+\"", simplify = TRUE)
      
      NAME <- 
        NAME %>% 
        stringr::str_remove_all(pattern = "\"[A-z ]+\"") %>% 
        stringr::str_squish()
    } else {
      NICKNAME <- NA
    }
    
    ### Reading the TPE from the post title
    TPE <- 
      topic %>% 
      rvest::html_nodes("small") %>% 
      dplyr::nth(1) %>% 
      rvest::html_text() %>% 
      stringr::str_extract_all(pattern = "[0-9]+") %>% 
      unlist() %>% 
      as.numeric()
    
    if(length(TPE) == 0){
      TPE = NA
    }
    
    
    ###  Extract user information                                     
    
    ## Specifies the user defined tags that will be removed from the user name
    userTags <- 
      paste0(
        "SMJHL GM|SHL GM|SHL HO|SMJHL HO|Commissioner|IIHF Commissioner|",
        "Registered|Rookie|Historian|",
        "Vancouver Whalers|Quebec City Citadelles|Detroit Falcons|Anchorage Armada|",
        "Chicago Syndicate|Manhattan Rage|Edmonton Blizzard|Los Angeles Panthers|Baltimore Platoon|",
        "Winnipeg Aurora|Tampa Bay Barracuda|Calgary Dragons|Buffalo Stampede|",
        "Head Office|Coach|Budget Director|Graphic Graders|Moderators|Federation Head|",
        "Player Updaters|PGS Grader|",
        "Fantasy League Manager|Simmer|Head Updater|",
        "Owner|Media Graders|Bank Manager|Simmer|Mentor|Comissioner|",
        "Team [A-z ]+|Awards Committee|",
        "All-Star Committee|",
        "Management|HOF Committee"
      )
    
    USER <- 
      topic %>% 
      rvest::html_nodes(".profile-username") %>% 
      dplyr::nth(1) %>% 
      rvest::html_text() %>% 
      stringr::str_split("\n") %>% 
      unlist() %>% 
      ## Usually the user name starts with a \n so the second cell holds the user info
      dplyr::nth(2) %>% 
      stringr::str_remove(
        pattern = userTags
      )
    
    USERLINK <- 
      topic %>% 
      rvest::html_nodes(".profile-username") %>% 
      dplyr::nth(1) %>% 
      rvest::html_nodes(xpath = "./a") %>% 
      rvest::html_attr("href")
    
    USERINFO <- 
      USERLINK %>% 
      user_scraper()
    
    USERDATA <- 
      topic %>% 
      rvest::html_nodes("#mainwidth2") %>% 
      dplyr::nth(1) %>% 
      rvest::html_nodes(".float_right") %>% 
      rvest::html_text() %>% 
      stringr::str_squish() %>% 
      .[1:2]
    
    names(USERDATA) <- c("Posts", "Threads")
    
    ### Extract player information
    
    PLAYERTEAM <-
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
      ) 
      
    
    if((PLAYERTEAM %>% nrow()) == 0){
      PLAYERTEAM <- 
        PLAYERTEAM %>% 
        dplyr::add_row()
    } else {
      PLAYERTEAM <- 
        PLAYERTEAM %>% 
        dplyr::filter(
          Inaugural.Season == max(Inaugural.Season)
        )
    }
    
    postData <- 
      topic %>% 
      rvest::html_nodes("div#two") %>% 
      dplyr::nth(1) %>% 
      rvest::html_nodes(".post_body") 
    
    if((postData %>% rvest::html_nodes(".mycode_font") %>% length()) > 0){
      postData <- 
        postData %>% 
        rvest::html_nodes(".mycode_font") %>% 
        rvest::html_text() %>% 
        paste0(collapse = "\n")
    } else {
      postData <- 
        postData %>% 
        rvest::html_text()  
    }
    
    FIRSTNAME <- 
      postData %>% 
      stringr::str_match(pattern = "First Name:(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    LASTNAME <- 
      postData %>% 
      stringr::str_match(pattern = "Last Name:(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    HANDEDNESS <- 
      postData %>% 
      stringr::str_match(pattern = "(Shoots|Hand[A-z]+):(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::last()
    
    RECRUITEDBY <- 
      postData %>% 
      stringr::str_match(pattern = "Recruited[A-z ]+:(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    RENDER <- 
      postData %>% 
      stringr::str_match(pattern = "Player Render:(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    JERSEYNR <- 
      postData %>% 
      stringr::str_match(pattern = "Jersey[A-z ]+:(.*?)\\n") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    BIRTHPLACE <- 
      postData %>% 
      stringr::str_match(pattern = "Birth[A-z]+:(.*?)(\\n|Player)") %>% 
      unlist() %>% 
      dplyr::nth(2)
    
    PLAYERINFO <- 
      cbind(
        FIRSTNAME,
        LASTNAME,
        HANDEDNESS,
        JERSEYNR,
        RECRUITEDBY,
        RENDER,
        BIRTHPLACE
      ) %>% 
      stringr::str_trim() %>% 
      t() %>% 
      as.data.frame()
      
    colnames(PLAYERINFO) <-
      c(
        "First Name",
        "Last Name",
        "Handedness",
        "Jersey Nr.",
        "Recruited By",
        "Player Render",
        "Birthplace"
      )
    
    # 
    # if(stringr::str_detect(postData, pattern = "Attributes")){
    #   postData <- 
    #     postData %>% 
    #     stringr::str_split(pattern = "Player Attributes|Payer Attributes") %>% 
    #     unlist() %>% 
    #     stringi::stri_remove_empty()
    # } else {
    #   postData <- 
    #     postData %>% 
    #     stringr::str_split(pattern = "Points") %>% 
    #     unlist() %>% 
    #     stringi::stri_remove_empty()
    # }
    # 
    # infoIndex <- 
    #   postData %>% 
    #   stringr::str_detect("First Name") %>% 
    #   which()
    # 
    # if(postData %>% length() > 2){
    #   PLAYERINFO <- 
    #     postData %>%
    #     dplyr::nth(infoIndex) %>% 
    #     stringr::str_split(
    #       pattern = ":|\\n"
    #     ) 
    # } else {
    #   PLAYERINFO <- 
    #     postData %>%
    #     dplyr::nth(1) %>% 
    #     stringr::str_split(
    #       pattern = ":|\\n"
    #     ) 
    # }
    # 
    # ## Checks if a title of Player information is present in the text
    # ## If so then remove first two elements of the vector
    # ## Otherwise only remove first element.
    # if(
    #   stringr::str_detect(
    #     PLAYERINFO %>% paste0(collapse = ""), 
    #     pattern = "Player Information"
    #     )
    #   ){
    #   PLAYERINFO <- 
    #     PLAYERINFO %>% 
    #     unlist() %>% 
    #     .[-(1:2)] %>% 
    #     stringr::str_squish() %>% 
    #     matrix(nrow = 2) %>% 
    #     janitor::row_to_names(1) %>% 
    #     dplyr::as_tibble()
    # } else { 
    #   PLAYERINFO <- 
    #     PLAYERINFO %>% 
    #     unlist() %>% 
    #     .[-1] %>% 
    #     stringr::str_squish() %>% 
    #     matrix(nrow = 2) %>% 
    #     janitor::row_to_names(1) %>% 
    #     dplyr::as_tibble()
    # }
    # 
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Height")] <- "Height"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Weight")] <- "Weight"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Hand")] <- "Handedness"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Shoots")] <- "Handedness"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Recruited")] <- "Recruited"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Jersey")] <- "Jersey Nr."
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Birth[A-z]+")] <- "Birthplace"
    # 
    
    PLAYERINFO <- 
      PLAYERINFO %>%  
      dplyr::mutate(
        `IIHF Nation` =
          if(exists('Birthplace', where = .)){
            dplyr::case_when(
              stringr::str_detect(Birthplace, pattern = "Sweden") ~ "Sweden",
              stringr::str_detect(Birthplace, pattern = "Canada|Ontario") ~ "Canada",
              stringr::str_detect(Birthplace, pattern = "USA|United States|Michigan|NY|N.Y.|Georgia") ~ "USA",
              stringr::str_detect(Birthplace, pattern = "Finland") ~ "Finland",
              stringr::str_detect(Birthplace, pattern = "Russia") ~ "Russia",
              stringr::str_detect(Birthplace, pattern = "Austria") ~ "Austria",
              stringr::str_detect(Birthplace, pattern = "Czechia|Czech Republic|CZE|Czechoslovakia") ~ "Czechia",
              stringr::str_detect(Birthplace, pattern = "Germany") ~ "Germany",
              stringr::str_detect(Birthplace, pattern = "England|Wales|Scotland|Northern Ireland|United Kingdom|Great Britain") ~ "Great Britain",
              stringr::str_detect(Birthplace, pattern = "Ireland") ~ "Ireland",
              stringr::str_detect(Birthplace, pattern = "Japan") ~ "Japan",
              stringr::str_detect(Birthplace, pattern = "Latvia") ~ "Latvia",
              stringr::str_detect(Birthplace, pattern = "Norway") ~ "Norway",
              stringr::str_detect(Birthplace, pattern = "Switzerland") ~ "Switzerland",
              TRUE ~ "Unassigned"
            )  
          } else {
            "Unassigned"
          }
      )
    
    ####### CODE FOR SCRAPING INFORMATION THAT IS NO LONGER USED    ####### 
    # ## Specifies which tags to use for the player information splits
    # playerMeta <- 
    #   paste0(
    #     "First Name:|First Name :|Last Name:|Position:|Born:|",
    #     "Birth Date:|Handedness:|Shoots:|",
    #     "Recruited By:|Recruited by:|Player Render:|Jersey Number:|",
    #     "Height:|Height \\(ft.\\):|Weight:|Weight \\(lbs.\\):|Birthplace:|Player"
    #     )
    # 
    # playerRemove <- 
    #   paste0(
    #     "Offensive Ratings|Defensive Ratings|Mental Ratings|Physical Ratings"
    #   )
    # 
    # ####### Some players have born or birth date as one of the player information, which means 
    # ####### that 12 on row 184 should be 13 to cover Birthplace
    # 
    # PLAYERINFO <- 
    #   postData %>%
    #   dplyr::nth(1) %>% 
    #   stringr::str_split(
    #     pattern = playerMeta
    #   ) %>% 
    #   unlist() %>% 
    #   stringr::str_squish() %>% 
    #   dplyr::as_tibble() %>% 
    #   dplyr::slice((min(nrow(.),12)-9):min(nrow(.),12)) %>% 
    #   unlist() 
    # 
    # ## Some players have their Player Information in a different order...
    # if(PLAYERINFO[1] == "Alexis" && PLAYERINFO[2] == "Saint-Michel"){
    #   PLAYERINFO <- 
    #     PLAYERINFO[c(1, 2, 3, 4, 5, 9, 10, 7, 8, 6)]
    # } else if(PLAYERINFO[1] == "Otis B." && PLAYERINFO[2] == "Driftwood"){
    #   PLAYERINFO <- 
    #     PLAYERINFO[-4]
    #   
    #   PLAYERINFO[6:10] <-
    #     PLAYERINFO[5:9]
    #   
    #   PLAYERINFO[6] <- NA
    # }
    # 
    # ## Sets the named vector to the specified order split from earlier
    # if(length(PLAYERINFO)==10){
    #   names(PLAYERINFO) <- 
    #     c(
    #       "First Name",
    #       "Last Name",
    #       "Position",
    #       "Handedness",
    #       "Recruited",
    #       "Render",
    #       "Jersey Nr.",
    #       "Height",
    #       "Weight",
    #       "Birthplace"
    #     )
    # }
    # 
    ####### CODE FOR SCRAPING ATTRIBUTES THAT IS NO LONGER USED    ####### 
    # 
    # 
    # if(!(PLAYERINFO["Position"] %in% c("Goaltender", "Goalie", "G"))){
    #   playerRatings<- 
    #     paste0(
    #       "Screening:|Getting Open:|Passing:|Puckhandling:|",
    #       "Shooting Accuracy:|Shooting Range:|Offensive Read:|",
    #       "Checking:|Hitting:|Positioning:|Stickchecking:|",
    #       "Shot Blocking:|Faceoffs:|Defensive Read:|Acceleration:|",
    #       "Agility:|Balance:|Speed:|Stamina:|Strength:|Fighting:|",
    #       "Aggression:|Bravery:|\\*"
    #     )
    #   
    #   PLAYERRATINGS<- 
    #     postData %>%
    #     dplyr::nth(2) %>% 
    #     stringr::str_split(
    #       pattern = playerRatings
    #     ) %>% 
    #     unlist() %>% 
    #     stringr::str_remove_all(
    #       pattern = playerRemove
    #     ) %>% 
    #     stringr::str_squish() %>% 
    #     dplyr::as_tibble() %>% 
    #     dplyr::slice(2:24) %>% 
    #     unlist() %>% 
    #     as.numeric()
    #   
    #   if(length(PLAYERRATINGS)==23){
    #     names(PLAYERRATINGS) <- 
    #       c(
    #         "Screening",
    #         "Getting Open",
    #         "Passing",
    #         "Puckhandling",
    #         "Shooting Accuracy",
    #         "Shooting Range",
    #         "Offensive Read",
    #         "Checking",
    #         "Hitting",
    #         "Positioning",
    #         "Stickchecking",
    #         "Shot Blocking",
    #         "Faceoffs",
    #         "Defensive Read",
    #         "Acceleration",
    #         "Agility",
    #         "Balance",
    #         "Speed",
    #         "Stamina",
    #         "Strength",
    #         "Fighting",
    #         "Aggression",
    #         "Bravery"
    #       )
    #   }
    #   
    # } else {
    #   playerRatings<- 
    #     paste0(
    #       "Blocker:|Glove:|Passing:|Poke Check:|",
    #       "Positioning:|Rebound:|Recovery:|",
    #       "Puckhandling:|Low Shots:|Reflexes:|Skating:|",
    #       "Mental Toughness:|Goalie Stamina:|\\*"
    #     )
    #   
    #   PLAYERRATINGS<- 
    #     postData %>%
    #     dplyr::nth(2) %>% 
    #     stringr::str_split(
    #       pattern = playerRatings
    #     ) %>% 
    #     unlist() %>% 
    #     stringr::str_remove_all(
    #       pattern = playerRemove
    #     ) %>% 
    #     stringr::str_squish() %>% 
    #     dplyr::as_tibble() %>% 
    #     dplyr::slice(c(2:12, 14, 18)) %>% 
    #     unlist() %>% 
    #     as.numeric()
    #   
    #   if(length(PLAYERRATINGS)==13){
    #     names(PLAYERRATINGS) <- 
    #       c(
    #         "Blocker",
    #         "Glove",
    #         "Passing",
    #         "Poke Check",
    #         "Positioning",
    #         "Rebound",
    #         "Recovery",
    #         "Puckhandling",
    #         "Low Shots",
    #         "Reflexes",
    #         "Skating",
    #         "Mental Toughness",
    #         "Goalie Stamina"
    #       )
    #   }
    # }
    # 
    # USEDTPE <- 
    #   PLAYERRATINGS %>% 
    #   dplyr::as_tibble() %>% 
    #   dplyr::left_join(
    #     tpeCost,
    #     by = c("value" = "Skill.level")
    #   ) %>% 
    #   summarize(
    #     sum = sum(TPE)
    #   ) %>% 
    #   unlist()
    ####### 
    
    ### Combines and structures the scraped data into a data.frame
    ##  Commented parts are from the discontinued playerRatings scraper that is no longer used
    data <- 
      data.frame(
        NAME,
        NICKNAME,
        CLASS,
        POSITION,
        TPE,
        LINK = player,
        USER,
        USERLINK,
        USERINFO,
        USERDATA %>% t(),
        #USEDTPE, 
        PLAYERTEAM,
        PLAYERINFO #,
        #PLAYERRATINGS %>% t()
      )
    
    return(data)
  }
