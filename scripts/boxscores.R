source("./scripts/API/apiSetup.R")

## Function that takes a data set of skaters boxscore and converts the TOI variables to seconds.
convertTOI <- function(data){
  suppressMessages(
    data %>% 
      mutate(
        across(
          contains("OnIce"),
          ~ .x %>% 
            str_split(":", simplify = TRUE) %>% 
            as.numeric() %>% 
            matrix(ncol = 2) %>% 
            as_tibble(.name_repair = "unique") %>% 
            mutate(
              `...1` = `...1` * 60
            ) %>% 
            rowSums()
        )
      ) %>% 
      mutate(
        timeOnIce = timeOnIce - ppTimeOnIce - shTimeOnIce
      ) 
  )
}

## Function that takes the gameid and produces the skater data
skatersGameData <- function(gameid){
  readAPI(
    url = "https://index.simulationhockey.com/api/v2/schedule/game/boxscore/skaters",
    query = list(league = leagueID, gameid = gameid)
  ) %>% 
    lapply(
      X = .,
      FUN = convertTOI
    ) %>% 
    lapply(
      X = .,
      FUN = function(x){
        x %>% 
          left_join(
            indStatsLoader(leagueID = leagueID, season = season)$players %>% 
              select(
                id, position
              ),
            by = "id"
          ) %>% 
          mutate(
            position = factor(x = position, levels = c("LW", "C", "RW", "LD", "RD"))
          )
      }
    ) %>% 
    lapply(
      X = .,
      FUN = function(x){
        x %>% 
          group_by(position) %>% 
          arrange(timeOnIce %>% desc(), .by_group = TRUE) %>% 
          mutate(
            depth = 1:n()
          ) %>% 
          relocate(
            position:depth,
            .after = id
          )
      }
    )
}



leagueID <- 0
season <- 73

schedule <- 
  readAPI(
    url = "https://index.simulationhockey.com/api/v1/schedule", 
    query = 
      list(
        league = leagueID, 
        season = season
      )
    )

temp <- skatersGameData(gameid = 9102)
  
  









