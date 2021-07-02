
library(tidyverse)  
library(dplyr) 
library(readr)
library(lubridate)
schedules <- read_delim("schedules.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)


schedules <- schedules %>%
  filter(Type=="Regular Season")

schedules$Date <- as.Date(schedules$Date, format = "%Y-%m-%d") 


schedules$year <- year(ymd(schedules$Date))
schedules$month <- month(ymd(schedules$Date))
schedules$day <- day(ymd(schedules$Date))
schedules$IRLDay <- 0
schedules$gameNumber <- 0
gameNum <- 1
#getting the games right
for (x in 1:length(schedules$day)){
  schedules$gameNumber[x] <- gameNum
  gameNum <- gameNum +1
}

IRLDay <- 1
ingameDay <- schedules$day[1] #First day of RS
daysInSeason <- 25 #How many days in the season
targetGamePerDay <- (length(schedules$day) / daysInSeason) - 1
iterator <- 0
#start with IRLDay 1, iterate the schedule until it reaches 22 games, then have it stop at the end of the day

for (i in 1:length(schedules$day)){
  schedules$IRLDay[i] <- IRLDay
  #print((iterator/targetGamePerDay)* 100)
  if ((iterator/targetGamePerDay)* 100 > 91 ) { #check to see if the "target" day is over 91% of the way there
    if (schedules$day[i] == schedules$day[i+1]){
      #check to see if the next day lands on the same sim day
      if (schedules$day[i] == schedules$day[i-1]){ 
        #check to make sure that the sim day lands on the day before
        schedules$IRLDay[i] <- IRLDay
      } else {
        #if it isnt, then increase the IRL day and make that sim day the next irl day
        IRLDay <- IRLDay + 1
        schedules$IRLDay[i] <- IRLDay #Make the IRL the next day
        iterator <- 0
      }
    }
  } else {
    schedules$IRLDay[i] <- IRLDay
  }
  i <- i+1
  iterator <- iterator +1
  

}


GPerD <- schedules %>%
  count(IRLDay)

####################################################
#This gets the team abbreviations for the teamID's
#####################################################

library(readr)
team_data <- read_delim("team_data.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

team_data <- team_data %>%
  select(TeamId, Abbr)

schedulesNew <- merge(schedules, team_data, by.x = "Home", by.y = "TeamId")

schedulesNew <- merge(schedulesNew, team_data, by.x = "Away", by.y = "TeamId")

schedulesNew <- schedulesNew[with(schedulesNew, order(Date)), ] %>%
  arrange(gameNumber, decreasing=TRUE)

####################################################################
#After this it could be a lot better
####################################################################



IRLStartDate <- as.Date("2021/06/24")
weekdays(as.Date(IRLStartDate))


test <- list()

for (x in 1:(length(schedules$day) +1 )) {
  #print(x)
  if(weekdays(as.Date(IRLStartDate)) == "Sunday"){
    #print(weekdays(as.Date(IRLStartDate)))
    IRLStartDate <- IRLStartDate + 1
    
  } else {
    test[[x]] <- paste(IRLStartDate, schedulesNew$Abbr.x[x], "@", schedulesNew$Abbr.y[x])
    print(paste(schedulesNew$Abbr.x[x], "@", schedulesNew$Abbr.y[x]))
    if(schedulesNew$IRLDay[x] == schedulesNew$IRLDay[x+1]){
      
      x <- x + 1
    } else{
      IRLStartDate <-IRLStartDate + 1
      #test[[x]] <- paste(IRLStartDate)
      x <- x + 1
    }
      
    
  }
  
  
  
}



finalSchedule <- do.call("rbind", test)


write.csv(finalSchedule,"C:\\Users\\Luke\\Desktop\\SHL\\SHLSchedule.csv", row.names = FALSE)


