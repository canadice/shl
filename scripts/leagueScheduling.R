require(Matrix)
require(gtools)
require(tidyr)
require(stringr)
require(stringi)
require(plyr)
require(dplyr)
require(tibble)
teams <- 14
conferences <- 2
divisions <- 4

games <- 66

startDay <- "2022-10-04"
endDay <- "2023-03-31"

nDays <- lubridate::as_date(endDay) - lubridate::as_date(startDay)

nWeeks <- 25

teamID <- 1:14
division <- 
  c(
    rep(1, times = 4),
    rep(2, times = 3),
    rep(3, times = 4),
    rep(4, times = 3)
  )

conference <- rep(1:2, each = 7)

setup <- data.frame(teamID, division, conference)

intraDiv <- matrix(2, nrow = 7, ncol = 7)
interDiv <- matrix(3, nrow = 7, ncol = 7)

matchups <- bdiag(replicate(n = 2, intraDiv, simplify = FALSE))
matchups[1:7, 8:14] <- matchups[8:14, 1:7] <- interDiv
diag(matchups) <- 0

k <- 1
games <- NA
for(i in 1:nrow(matchups)){
  for(j in 1:ncol(matchups)){
    if(matchups[i,j] == 0){
      #DO NOTHING
    } else {
      games[k:(k+matchups[i,j]-1)] <- 
        paste(i, " @ ", j, " (",1:matchups[i,j], ")", sep = "") 
      
      k <- k + matchups[i,j]  
    }
  }
}

dataMatchups <- 
  matchups %>% as.matrix() %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(cols = -1) %>% 
  mutate(
    name = stringr::str_extract_all(name, pattern = "[0-9]+", simplify = TRUE) %>% c()
  ) %>% 
  filter(
    value != 0
  ) %>% 
  rename(
    away = rowname,
    home = name,
    meetings = value
  )


# sampleGames <- 1:length(games)

# checkGames <- games %>% str_split(pattern = " ", simplify = TRUE) %>% .[,c(1,3)]

round <- matrix(nrow = 7, ncol = 66)

# resetSampleGames <- sampleGames

for(i in 1:ncol(round)){
  
  j <- 1
  
  k <- 1
  while(j <= nrow(round)){
    
    if(all(round[,i] %>% is.na())){
      current <- 
        dataMatchups %>% 
        slice_sample(n = 1, weight_by = meetings)
      
      round[j,i] <- paste(current$away, current$home, sep = " @ ")
      
      tempDataMatchups <- 
        dataMatchups %>% 
        mutate(
          meetings = if_else(away == current$away & home == current$home, meetings - 1, meetings)
        )
      
    } else {
      chosen <- round[,i] %>% str_split(pattern = " ", simplify = TRUE) %>% .[,c(1,3)] %>% c() %>% stringi::stri_remove_empty_na()
      
      filteredDataMatchups <- 
        tempDataMatchups %>% 
        filter(
          !(away %in% chosen | home %in% chosen) 
        ) 
      
      if(all(filteredDataMatchups$meetings == 0) | (filteredDataMatchups %>% nrow() == 0)){
        round[j:1, i] <- NA
        
        j <- 0
        k <- k + 1
        print(paste(i, j, k))
      } else {
        current <- 
          filteredDataMatchups %>% 
          slice_sample(n = 1, weight_by = meetings)
        
        round[j,i] <- paste(current$away, current$home, sep = " @ ")
        
        tempDataMatchups <- 
          tempDataMatchups %>% 
          mutate(
            meetings = if_else(away == current$away & home == current$home, meetings - 1, meetings)
          )
      }
    }
    j <- j + 1
  }
  
  ## Overwrites the sampling source when an entire round is finished.
  dataMatchups <- 
    tempDataMatchups
  
}

round






