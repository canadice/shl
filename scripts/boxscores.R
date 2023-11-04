source("./scripts/API/apiSetup.R")

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

game <- 
  readAPI(
    url = "https://index.simulationhockey.com/api/v2/schedule/game/boxscore/skaters",
    query = list(league = leagueID, gameid = 8572)
  )

game <- 
  readAPI(
    url = "https://index.simulationhockey.com/api/v2/schedule/game/boxscore/summary",
    query = list(league = leagueID, gameid = 8572)
  )




