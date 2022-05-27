
###########################################################################
###########################################################################
###                                                                     ###
###                    SETTING UP A SQLLITE DATABASE                    ###
###                                                                     ###
###########################################################################
###########################################################################

# install.packages(c("DBI", "dbplyr", "RSQLite"))

require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)

source("scripts/API/apiSetup.R")

### Creating the DB strucutre
myDB <- 
  src_sqlite(
    "database/analyticsDB.sqlite", 
    create = TRUE
  )

myDB


# con <- dbConnect(SQLite(), "database/shl.db")
# 
# teams <- read.csv2(file = "csv/team_information.csv")


#################################################################
##                       Input team data                       ##
#################################################################

teamData <- 
  teams %>% 
  select(
    teamID,
    fhmID,
    franchiseID,
    leagueID,
    name = team,
    abbr,
    primaryCol = primary,
    secondaryCol = secondary,
    alt1Col = alt1,
    alt2Col = alt2,
    inaugural = Inaugural.Season
  )

copy_to(myDB, teamData)

#################################################################
##                       Input user data                       ##
#################################################################

userData <- 
  forumData %>% 
  select(
    user = USER,
    userPage = USERLINK,
    joined = Joined,
    lastVisit = Last.Visit,
    timeOnline = Online.For,
    bank = Bank.Balance,
    posts = Posts,
    threads = Threads,
    reputation = Reputation
  ) %>% 
  unique() %>% 
  group_by(user) %>% 
  slice_head()

  
# createCommand <- 
#   sprintf(
#     "create table %s(%s, primary key(%s))", 
#     "userData",
#     paste(names(userData), collapse = ", "),
#     names(userData)[1]
#   )
# 
# dbExecute(con, createCommand)
# 
# dbWriteTable(
#   con, 
#   "userData", 
#   value = 
#     userData,
#   append = TRUE,
#   row.names = FALSE
# )

#################################################################
##                      Input player data                      ##
#################################################################
playerData <- 
  forumData %>% 
  select(
    ## The thread ID of the player page is used as a playerID
    playerID = LINK,          
    currentTeam = teamID,
    user = USER,
    name = NAME,
    nickname = NICKNAME,
    fhmName = clean_name,
    draftClass = CLASS,
    position = POSITION,
    activeStatus = Active,
    currentTPE = TPE,
    shoots = Handedness,
    jerseyNr = Jersey.Nr.,
    recruitedBy = Recruited.By,
    playerRender = Player.Render,
    birthplace = Birthplace
  ) %>% 
  mutate(
    playerID = 
      playerID %>% 
      str_split(pattern = "=", simplify = TRUE) %>% 
      .[,2] %>% 
      as.numeric(),
    retired = 0
  )

# createCommand <- 
#   sprintf(
#     "create table %s(%s, primary key(%s), %s)", 
#     "playerData",
#     paste(names(playerData), collapse = ", "),
#     names(playerData)[1],
#     "FOREIGN KEY (user) REFERENCES userData(user), FOREIGN KEY (currentTeam) REFERENCES teamData(teamID)"
#   )
# 
# dbExecute(con, createCommand)
# 
# dbWriteTable(
#   con, 
#   "playerData", 
#   value = 
#     playerData,
#   append = TRUE,
#   row.names = FALSE
# )

##################################################################
##                       Input draft data                       ##
##################################################################
shlDraftData <- 
  draftData %>%
  left_join(
    teams %>% 
      select(
        abbr,
        teamID,
        Inaugural.Season
      ) ,
    by = c("Team" = "abbr")
  ) %>% 
  group_by(
    Season,
    Drafted,
    Player
  ) %>% 
  filter(
    Season >= Inaugural.Season 
  ) %>% 
  filter(
    Inaugural.Season == max(Inaugural.Season)
  ) %>% 
  ungroup() %>% 
  left_join(
    playerData %>% 
      select(
        name, 
        playerID,
        draftClass
      ),
    by = c("Player" = "name")
  ) %>% 
  mutate(
    draftClass = 
      str_extract_all(
        draftClass,
        pattern = "[0-9]+", 
        simplify = TRUE
      ) %>% as.numeric()
  ) %>% 
  mutate(
    playerID = 
      case_when(
        Season < draftClass ~ NA_real_,
        TRUE ~ playerID
      )
  ) %>% 
  mutate(
    playerID = 
      case_when(
        is.na(playerID) ~ paste0(Season, Drafted, teamID, Player %>% as.factor() %>% as.numeric()) %>% as.numeric(),
        TRUE ~ playerID
      )
  ) %>% 
  select(
    playerID,
    player = Player,
    teamID,
    season = Season,
    pick = Drafted
  ) 
  
# createCommand <- 
#   sprintf(
#     "create table %s(%s, primary key(%s), %s)", 
#     "shlDraft",
#     paste(names(shlDraftData), collapse = ", "),
#     names(shlDraftData)[1],
#     "FOREIGN KEY (playerID) REFERENCES players(playerID), FOREIGN KEY (teamID) REFERENCES teamData(teamID)"
#   )
# 
# dbExecute(con, createCommand)
# 
# dbWriteTable(
#   con, 
#   "shlDraft", 
#   value = 
#     shlDraftData,
#   append = TRUE,
#   row.names = FALSE
# )




##################################################################
##                       Input draft data                       ##
##################################################################
iihfData <- 
  forumData %>% 
  select(
    ## The thread ID of the player page is used as a playerID
    playerID = LINK,          
    IIHF.Nation,
    Original,
    transferred = Transfer.Season
  ) %>% 
  mutate(
    playerID = 
      playerID %>% 
      str_split(pattern = "=", simplify = TRUE) %>% 
      .[,2] %>% 
      as.numeric()
  ) %>% 
  left_join(
    teams %>% 
      filter(
        league == "IIHF"
      ) %>% 
      select(
        team,
        teamID
      ),
    by = c("IIHF.Nation" = "team")
  ) %>% 
  rename(
    iihfID = teamID
  ) %>% 
  left_join(
    teams %>% 
      filter(
        league == "IIHF"
      ) %>%  
      select(
        team,
        teamID
      ),
    by = c("Original" = "team")
  ) %>% 
  rename(
    originalID = teamID
  ) %>% 
  select(
    playerID,
    iihfID,
    originalID,
    transferred
  )

createCommand <- 
  sprintf(
    "create table %s(%s, primary key(%s), %s)", 
    "iihfData",
    paste(names(iihfData), collapse = ", "),
    names(iihfData)[1],
    "FOREIGN KEY (playerID) REFERENCES players(playerID), FOREIGN KEY (iihfID) REFERENCES teamData(teamID), FOREIGN KEY (originalID) REFERENCES teamData(teamID)"
  )

dbExecute(con, createCommand)

dbWriteTable(
  con, 
  "iihfData", 
  value = 
    iihfData,
  append = TRUE,
  row.names = FALSE
)



dbDisconnect(con)
