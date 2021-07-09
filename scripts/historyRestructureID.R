require(dplyr)

history <- read.csv("csv/history_skaters.csv", sep = ",", dec = ".")

history <- 
  history %>% 
  mutate(
    newTeamID = 
      case_when(
        TeamId == 9 ~ 1,  #Calgary Dragons
        TeamId == 7 ~ 2,  #Hamilton Steelhawks
        TeamId == 10 & Season >= 1 ~ 3, #Toronto Stars
        TeamId == 10 & Season >= 2 ~ 9, #Toronto North Stars
        
        TeamId == 6 ~ 4,  #West Kendall Platoon (BAP)
        TeamId == 61 ~ 27,#Baltimore Platoon
        
        TeamId == 14 ~ 5, #Winnipeg Jets (WIN)
        TeamId == 14 & Season >= 59 ~ 31, #Winnipeg Aurora 
        
        TeamId == 37 ~ 6, #Wisconsin Monarchs (TBB)
        TeamId == 37 & Season >= 3 ~ 10, #Vancouver Ice Wolves (TBB)
        TeamId == 39 ~ 13,#Vancouver Nightmare (TBB)
        TeamId == 40 ~ 14,#Las Vegas Kings (TBB)
        TeamId == 5 ~ 20,#Seattle Riot (TBB)
        TeamId == 44 ~ 26,#Tampa Bay Barracuda
        
        TeamId == 38 ~ 7,#Edmonton Comets (EDM)
        TeamId == 3 ~ 15, #Edmonton Blizzard
        
        TeamId == 4 ~ 8,#Manhattan Rage
        
        TeamId == 11 ~ 11,#Los Angeles Panthers
        
        TeamId == 2 ~ 12, #Minnesota Chiefs (MIN)
        TeamId == 64 ~ 29,#Minnesota Monarchs
        
        TeamId == 41 & Season >= 10 ~ 16,#Tampa Bay Hydras (NEW)
        TeamId == 41 & Season >= 11 ~ 18,#Hartford Hydras (NEW)
        TeamId == 12 ~ 19,#New England Wolfpack
        
        TeamId == 1 ~ 17, #Texas Renegades
        
        TeamId == 13 ~ 21,#Buffalo Stampede
        
        TeamId == 42 ~ 22,#Portland Admirals (SFP)
        TeamId == 8 & Season >= 37 ~ 23, #San Francisco Pride
        
        TeamId == 43 ~ 24,#Chicago Syndicate
        
        TeamId == 45 ~ 25,#New Orleans Specters
        
        TeamId == 63 ~ 28,#Atlanta Inferno
        
        TeamId == 62 ~ 30,#Seattle Argonauts
        TRUE ~ NaN
      )
  ) %>% 
  relocate(
    newTeamID,
    .before = TeamId
  )

write.csv(history, file = "csv/history_skaters.csv", row.names = FALSE)


