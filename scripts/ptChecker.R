require(tidyverse)
require(fuzzyjoin)
require(RecordLinkage)


posts <- read.csv("mybb_posts.csv")

checkSimilarPT <- function(thread){
  oneThread <- posts %>% filter(tid == thread) %>% mutate(
    message = message %>% str_to_lower()
  ) %>% 
    filter(
      message != "",
      message != first(message),
      !(message %>% str_detect(pattern = "pt pass|\\[[^\\]]*\\][A-z ]+\\[/url\\]|i.imgur.com|media.discordapp.net|probaseballexperience.jcink.net|worldsimbasketball.jcink.net|forums.sim-football.com"))
    ) %>% 
    mutate(
      message = str_remove_all(message, 
                               pattern = 
                                 "\\[[^\\]]*\\]")
    ) %>% 
    filter(
      str_length(message) > 75
    )
  
  
  compare <- function(task){
    
    otherMessages <- 
      oneThread %>% 
      filter(
        message != task
      )
    
    similarities <- 
      sapply(
        X = otherMessages$message,
        FUN = function(x){
          levenshteinSim(x, task)
        }
      )
    
    closest <- which(similarities == max(similarities))
    
    closestMatch <- 
      otherMessages[closest,] %>% 
      mutate(similarity = similarities[closest])
    
    oneThread %>% 
      filter(
        message == task
      ) %>% 
      left_join(
        closestMatch %>% 
          select(
            tid, username, message, similarity
          ),
        by = "tid",
        suffix = c(".original", ".match")
      )
  }
  
  temp <- 
    map_df(
      .x = oneThread$message,
      .f = compare
    )
  
  return(temp)
  
}

temp <- 
  map(
    .x = posts$tid %>% unique(),
    .f = checkSimilarPT
  )

similarPTs <- 
  temp %>% 
    lapply(
      X = .,
      FUN = function(x){
        if(nrow(x) > 0){
          x %>% 
            filter(
              similarity > 0.5
            )  
        } else {
          
        }
      }
    ) %>% 
  do.call(
    what = rbind,
    args = .
  ) %>% 
  filter(
    username.original != username.match
  )
# 
# NJ <- similarPTs %>% 
#   filter(username.original == "NJBadApple")

save.image(file = "Similarity Scores PT.RData")



