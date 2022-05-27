source("scripts/fhm6SaveParser.R")


base <- 
  fhm6Aggregator(
    saveFolder = "C:/Users/hieta/Documents/Out of the Park Developments/Franchise Hockey Manager 6/saved_games/", 
    saveGame = "SHL-S64-Test-", 
    nSims = 10
  )

baseAgg <- 
  teamParserRaw(base) %>% 
  group_by(
    Abbr
  ) %>% 
  summarize(
    across(
      .cols = Wins:Goals.Against,
      ~ mean(.x)
    )
  )

basePlayers <- 
  playerAggFromRaw(base) %>% 
  filter(
    Type == "Regular Season",
    TeamId == 4
  ) %>% 
  transmute(
    Last.Name,
    GF.60,
    GFperSF = GF.60/SF.60*100,
    GAperSA = GA.60/SA.60*100,  
    Diff = GFperSF - GAperSA
  )

alt <- 
  fhm6Aggregator(
    saveFolder = "C:/Users/hieta/Documents/Out of the Park Developments/Franchise Hockey Manager 6/saved_games/", 
    saveGame = "SMJHL-S62-Counter", 
    nSims = 10
  )

altAgg <- 
  teamParserRaw(alt) 

altPlayers <- 
  playerAggFromRaw(alt) %>% 
  filter(
    Type == "Regular Season",
    TeamId == 9,
    Last.Name == "Roderic Banes"
  ) %>% 
  transmute(
    Last.Name,
    GFperSF = GF.60/SF.60*100,
    GAperSA = GA.60/SA.60*100,
    Diff = GFperSF - GAperSA
  )

var <- 
  fhm6Aggregator(
    saveFolder = "C:/Users/hieta/Documents/Out of the Park Developments/Franchise Hockey Manager 6/saved_games/", 
    saveGame = "SMJHL-S62-Day10Var", 
    nSims = 15
  )

varAgg <- 
  teamParserRaw(var) 

varPlayers <- 
  playerAggFromRaw(var) %>% 
  filter(
    Type == "Regular Season",
    TeamId == 9
  ) %>% 
  transmute(
    Last.Name,
    GFperSF = GF.60/SF.60*100,
    GAperSA = GA.60/SA.60*100,
    Diff = GFperSF - GAperSA
  )


comparison <- 
  rbind(
    cbind("base", basePlayers),
    cbind("counter", altPlayers)
  )

comparison <- 
  altPlayers %>% 
  left_join(
    basePlayers,
    by = c("Last.Name"),
    suffix = c(".Alt", ".Base")
  ) %>% 
  left_join(
    varPlayers,
    by = "Last.Name"
  )

baseAgg %>% teamAggregator()

altAgg %>% teamAggregator()

varAgg %>% teamAggregator()


















# medal1 <- 
#   day1 %>% 
#   lapply(
#     X = .,
#     FUN = function(x){
#       x$schedule %>% 
#         filter(Type == "Playoffs") %>% 
#         slice_tail() %>% 
#         mutate(
#           Gold = if_else(Score.Home > Score.Away, Team.Home, Team.Away),
#           Silver = if_else(Score.Home < Score.Away, Team.Home, Team.Away)
#         ) %>% 
#         pivot_longer(
#           Gold:Silver
#         ) %>% 
#         select(name, value)
#     } 
#   ) %>% 
#   do.call(what = rbind, args = .) %>% 
#   table()

