
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
#################################################################


##----------------------------------------------------------------
##                        Loading packages                       -
##----------------------------------------------------------------

options(digits.secs = 3)
## Changes when scientific notation is used to never
options(scipen=999)

start <- Sys.time()
print(paste("Start:", Sys.time()))

## tools packages
# devtools::install_github("Canadice/shlrtools")
require(shlrtools)

## API packages
require(httr, quietly = TRUE)
require(jsonlite, quietly = TRUE)

## Packages for reading urls
require(rvest, quietly = TRUE)
require(RCurl, quietly = TRUE)

## Data processing
require(tidyr, quietly = TRUE)

if("plyr" %in% (.packages())){
  # Do nothing
} else {
  require(plyr, quietly = TRUE)  
}

require(dplyr, quietly = TRUE)
require(janitor, quietly = TRUE)
require(tibble, quietly = TRUE)
require(purrr, quietly = TRUE)

## Visualizations
require(ggplot2, quietly = TRUE)
require(ggnewscale, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)
require(cowplot, quietly = TRUE)
require(ggpubr, quietly = TRUE)
require(png, quietly = TRUE)
require(grid, quietly = TRUE)

## Loading database related packages
require(DBI)
require(dbplyr)
require(RSQLite)

# Packages for image recognition
# require(tesseract, quietly = TRUE)

# Packages for svg images
require(magick, quietly = TRUE)
require(rsvg, quietly = TRUE)

## Package for handling date and time
require(lubridate, quietly = TRUE)

## Packages for handling strings
require(stringr, quietly = TRUE)
require(stringi, quietly = TRUE)

## Loading package that can talk to Google Sheets
require(googlesheets4, quietly = TRUE)

print(paste("Loading packages done:", Sys.time()))


##----------------------------------------------------------------
##                    Sets up the data loading                   -
##----------------------------------------------------------------

## GitHub raw url
raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

## Removes the need to authenticate loading from public Google Sheets
googlesheets4::gs4_deauth()

## Loads the Forum Data from the shlrtools Github
scraperUrl <- url("https://github.com/canadice/shlrtools/blob/main/data/forumData.RData?raw=true")

load(scraperUrl)

# forumData <- 
#   forumData %>% 
#   rename_with(toupper)

print(paste("Forum Data done:", Sys.time()))

## Downloads a local file for the database
dbFile <- tempfile(fileext = ".db")

dbUrl <- ("https://github.com/canadice/shl/blob/main/database/SHL_Database.db?raw=true")

download.file(dbUrl, destfile = dbFile, mode = "wb")

con <- 
  dbConnect(
    SQLite(), 
    dbFile
  )

## Loading career data
historySkaterSeason <- 
  tbl(con, "skaterHistory") %>%
  filter(
    Name != "CPU Player",
    !(Name %like% "%Computer%")
  ) %>% 
  collect() 
  
historyGoalieSeason <- 
  tbl(con, "goalieHistory") %>%
  filter(
    Name != "CPU Player",
    Name != "CPU",
    !(Name %like% "%Computer%")
  ) %>% 
  collect() 

print(paste("History done:", Sys.time()))

## Loading team information
teamData <-
  tbl(con, "teamInfo") %>%
  collect() %>% 
  unique()

print(paste("Team data done:", Sys.time()))

## Loads IIHF History
historyIIHF <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1V_S72NZHtO5hxuy1nLOtAqdRmA3zz7ckTPWzUVR2REA/edit#gid=0",
    sheet = "Rankings"
  )

print(paste("IIHF Ranking History done:", Sys.time()))

############################################################################
############################################################################
###                                                                      ###
###                              AUDIT DATA                              ###
###                                                                      ###
############################################################################
############################################################################



print(paste("Total loading time: ", Sys.time() - start))

dbDisconnect(con)
