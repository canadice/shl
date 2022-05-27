
###########################################################################
###########################################################################
###                                                                     ###
###                      AWARDS COMMITTEE ANALYSIS                      ###
###                                                                     ###
###                         CREATED: 2021-05-26                         ###
###                        LAST EDIT: 2021-05-26                        ###
###                                                                     ###
###########################################################################
###########################################################################


##---------------------------------------------------------------
##              Loading relevant data and functions             -
##---------------------------------------------------------------

### Loading API functions and loads relevant data sets
source("scripts/API/apiSetup.R")

shlData <- dataLoader(0, 61)

shlData2 <- dataLoader(0, 60)

index <- which(shlData$skaters$name %in% shlData2$skaters$name)

shlRookies <- shlData$skaters[-index,]

write.csv2(shlRookies, file = "S61Rookies.csv", row.names = FALSE)



playerStatistics <- 
  indStatsLoader(0, 61)$players %>% 
  do.call(what = data.frame, args = .) %>% 
  mutate(
    offEffectiveness = advancedStats.GF60/advancedStats.SF60,
    defEffectiveness = 1 - (advancedStats.GA60/advancedStats.SA60)
  ) %>% 
  select(
    name, 
    position,
    team,
    goals,
    assists,
    points,
    contains("advancedStats"),
    offEffectiveness,
    defEffectiveness
  ) %>% 
  filter(
    name %in% 
      c(
        "Aaron Wilson",
        "Mitchell van der Heijden",
        "Theo Morgan",
        "Rikard Hammarberg",
        "Karl Krashwagen",
        "Cal Labovitch",
        "Kriss Darzins",
        "Martijn Westbroek",
        "Steve Harrington",
        "Henrik Lekberg Osterman",
        "Aron Hernadivic",
        "Alexander Roach",
        "Sven Svenson",
        "Ryan Shepard",
        "Alexander Wachter",
        "Joseph Weston",
        "Matt Kholin",
        "Ethan Price",
        "Bobby Sharp"
      )
  )







data <- indStatsLoader(0, 61)$goalies
data2 <- indStatsLoader(0, 60)$goalies

index <- which(data$name %in% data2$name)

shlGRookies <- data[-index, ]

playerCard("Friedensreich Hundertwasser", shlData)
