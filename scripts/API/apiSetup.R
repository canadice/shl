
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-06-08                    ##
#################################################################

### Packages that are used
## API packages
require(httr)
require(jsonlite)

## Packages for reading urls
require(rvest)
require(RCurl)

## Data processing
require(tidyr)

if("plyr" %in% (.packages())){
  # Do nothing
} else {
  require(plyr)  
}

require(dplyr)
require(janitor)
require(tibble)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)
require(ggpubr)
require(png)
require(grid)

# Packages for svg images
require(magick)
require(rsvg)

## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)

## Loading package that can talk to Google Sheets
require(googlesheets4)

## Changes when scientific notation is used to never
options(scipen=999)

## GitHub raw url
raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

### Loading data sets
## Current forum scrape data
forumData <- 
  read.csv2(
    paste(
      raw, 
      "csv/SHL_Forum_Scrape_Results.csv", 
      sep = ""),
    sep = ";",
    dec = ","
    )

## Loads Attribute Keys
attKey <- read.csv2(paste(raw, "csv/attribute_key.csv", sep = ""))

## Loading cost for TPE
tpeCost <- read.csv2(paste(raw, "csv/tpe_cost.csv", sep = ""))

## Loading team information
teamInfo <- read.csv2(paste(raw, "csv/team_information.csv", sep = "")) %>% 
  mutate(
    logoImage = 
      list(
        image_read_svg(paste(raw, "graphics/Atlanta.svg", sep = "")),
        image_read_svg(paste(raw, "graphics/Baltimore.svg", sep = "")),
        image_read_svg(paste(raw, "graphics/Buffalo.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Chicago.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Hamilton.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Manhattan.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/New_England.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Tampa_Bay.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Toronto.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Calgary.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Edmonton.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Los_Angeles.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Minnesota.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/New_Orleans.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/San_Francisco.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Seattle.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Texas.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Winnipeg.svg",sep = "")),
        NA, #Old Winnipeg Jets
        image_read_svg(paste(raw, "graphics/Carolina.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Detroit.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Maine.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Newfoundland.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Quebec_City.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/St_Louis.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Anaheim.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Anchorage.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Colorado.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Kelowna.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Nevada.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Vancouver.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Montreal.svg",sep = "")),
        image_read_svg(paste(raw, "graphics/Philadelphia.svg",sep = ""))
      )
  )


#################################################################
##           Creates function for different analyses           ##
#################################################################


##---------------------------------------------------------------
##          Function that reads from API based on url           -
##---------------------------------------------------------------

readAPI <- function(url, ...){
  temp <- 
    url %>% 
    # Gets the API information, the ... allows for specific queries with query = list()
    GET(...) 
  
  temp$content %>% 
    # Extracts the data
    rawToChar() %>% 
    # Converts it from JSON to a data frame
    fromJSON() %>% 
    return()
}


##---------------------------------------------------------------
##        Function that loads player ratings (attributes)       -
##---------------------------------------------------------------

playerLoader <- function(leagueID, season = NULL){
  players <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/ratings",
      query = list(league = leagueID, season = season)
    ) 
  goalies <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/ratings",
      query = list(league = leagueID, season = season)
    ) 
  
  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all players
    players %>% 
    ## Selects only names (for grouping) and attributes
    select(
      name,
      team,
      screening:professionalism
    ) %>% 
    ## Creates a attribute column and value column for each player
    pivot_longer(
      cols = screening:professionalism,
      names_to = "attribute"
    ) %>% 
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>% 
    ## Groups by name for summarizing
    group_by(
      name,
      team
    ) %>% 
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 11 starting Stamina
      usedTPE = sum(TPE) - 62*5 - 16
    ) %>% 
    ungroup() %>% 
    select(name, usedTPE)
  
  players <-
    players %>% 
    left_join(
      usedTPE,
      by = c("name")
    )
  
  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all goalies
    goalies %>% 
    ## Selects only names (for grouping) and attributes
    select(
      name,
      team,
      blocker:professionalism
    ) %>% 
    ## Creates a attribute column and value column for each player
    pivot_longer(
      cols = blocker:professionalism,
      names_to = "attribute"
    ) %>% 
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>% 
    ## Groups by name for summarizing
    group_by(
      name,
      team
    ) %>% 
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 8 Aggression
      usedTPE = sum(TPE) - 62*3 - 4
    ) %>% 
    ungroup() %>% 
    select(name, usedTPE)
  
  goalies <-
    goalies %>% 
    left_join(
      usedTPE,
      by = c("name")
    ) %>% 
    dplyr::rename(
      goaliePassing = passing,
      goaliePuckhandling = puckhandling,
      goaliePositioning = positioning
    )
  
  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>% 
    return()
}


##---------------------------------------------------------------
##            Function that loads player statistics             -
##---------------------------------------------------------------

indStatsLoader <- function(leagueID, season = NULL, type = NULL){
  players <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/stats",
      query = list(league = leagueID, season = season, type = type)
    ) 
  goalies <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/stats",
      query = list(league = leagueID, season = season, type = type)
    ) 
  
  players <-
    players %>% 
    mutate(
      ## Using the format with glue grammar that allows for dynamic variable names
      across(
        contains("TimeOnIce"),
        ~ format(
          as.POSIXct(
            .x/gamesPlayed, 
            origin = "1970-01-01"
          ), 
          "%M:%S"
        )
      )
    )
    
  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>% 
    return()
}


##---------------------------------------------------------------
##      Function that loads team statistics and information     -
##---------------------------------------------------------------

teamLoader <-  function(leagueID, season = NULL){
  teams <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/teams",
      query = list(league = leagueID, season = season)
    ) 
  
  return(teams)
}


##----------------------------------------------------------------
##                  Function that loads schedule                 -
##----------------------------------------------------------------

gamesLoader <- function(leagueID, season = NULL){
  schedule <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/schedule",
      query = list(league = leagueID, season = season)
    ) 
  
  return(schedule)
}


##---------------------------------------------------------------
##                Function that loads standings                 -
##---------------------------------------------------------------

standingsLoader <- function(leagueID, season = NULL){
  standings <- 
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/standings",
      query = list(league = leagueID, season = season)
    ) 
  
  return(standings)
}



##-----------------------------------------------------------------------
##  Adding function that rescales a variable that has been normalized   -
##-----------------------------------------------------------------------

rescale <- function(x){
  x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
}


##----------------------------------------------------------------------
##  Creates a function that loads the data used for the player cards   -
##----------------------------------------------------------------------

dataLoader <- function(league, season = NULL){
  ### Reads all players from the index
  players <- indStatsLoader(league, season = season, type = "regular")
  
  ### Reads team information from the index
  teams <- 
    teamLoader(league, season = season) %>% 
    left_join(
      teamInfo %>% 
        select(
          team,
          logoImage
        ),
      by = c("name" = "team")
    )
  
  
  ### Splits the data sets ### MOVE THIS ONE TO LOWER WHEN BUILDING GOALIE FUNCTIONS
  goalies <- 
    players$goalies %>%
    do.call(data.frame, .)
  
  ##----------------------------------------------------------------
  ##                      Adding new variables                     -
  ##----------------------------------------------------------------
  
  skaters <- 
    players$players %>% 
    do.call(data.frame, .) %>% 
    
    ## Creates takeaway to giveaway ratio
    ## Creates group for forwards and defencemen
    mutate(
      `TA_GA` = takeaways/giveaways,
      posGroup = 
        case_when(
          position %in% c("LD", "RD") ~ "Def",
          TRUE ~ "For"
        )
    ) %>% 
    relocate(
      `TA_GA`,
      .after = "takeaways"
    ) %>% 
    ## Converts time on ice variables to numeric values in seconds
    mutate(
      across(
        contains("timeOnIce"), 
        ~ .x %>% 
          ms() %>% 
          as.numeric()
      ),
      ## Creates Even Strength Time On Ice
      esTimeOnIce = timeOnIce - ppTimeOnIce - shTimeOnIce
    ) %>% 
    select(
      -id, -league, -season
    ) %>% 
    
    ## Calculates the Time On Ice rank within the team and skater group
    group_by(team, posGroup) %>% 
    mutate(
      esRank = rank(-esTimeOnIce, ties.method = "first"),
      ppRank = rank(-ppTimeOnIce, ties.method = "first"),
      shRank = rank(-shTimeOnIce, ties.method = "first")
    ) %>% 
    ## Adds the nth text to the rank
    mutate(
      across(
        contains("Rank"),
        ~ case_when(
          .x %in% c(11,12,13) ~ paste(.x, "th", sep = ""),
          .x %% 10 == 1 ~ paste(.x, "st", sep = ""),
          .x %% 10 == 2 ~ paste(.x, "nd", sep = ""),
          .x %% 10 == 3 ~ paste(.x, "rd", sep = ""),
          TRUE ~ paste(.x, "th", sep = "")
        )
      )
    ) %>% 
    ungroup() %>% 
    ## Relocates variables
    relocate(
      esRank:shRank,
      .after = team
    ) %>% 
    relocate(
      esTimeOnIce,
      .after = timeOnIce
    )
  
  ##---------------------------------------------------------------
  ##                  Aggregating team statistics                 -
  ##---------------------------------------------------------------
  
  teamSkater <- 
    skaters %>% 
    group_by(team) %>% 
    ## Calculates the sum and mean where applicable ### SHOULD MOVE GAME RATINGS TO MEAN
    summarize(
      across(
        timeOnIce:defensiveGameRating,
        sum
      ),
      across(
        contains("advancedStats"),
        mean
      )
    ) 
  
  ##----------------------------------------------------------------
  ##    Transforming statistics in units of standard deviation     -
  ##----------------------------------------------------------------
  
  teamSkaterZ <- 
    teamSkater %>% 
    mutate(
      across(
        timeOnIce:advancedStats.FFPctRel,
        ~ .x %>% 
          as.numeric() %>% 
          scale()
      )
    ) %>% 
    
    ## Changes direction of AGAINST stats, larger values are worse so they become negative 
    mutate(
      across(
        contains(
          "advancedStats.GA60"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.SA60"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.CA"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.FA"
        ),
        ~ -.x
      )
    ) 
  
  ### Standardizing skaters
  skatersZ <- 
    skaters %>% 
    ## Calculates the normalized value (z-score) of all players' variables
    ## The z-score is the distance in units of standard deviation from the league mean
    mutate(
      across(
        timeOnIce:advancedStats.FFPctRel,
        ~ .x %>% 
          as.numeric() %>% 
          scale()
      )
    ) %>% 
    ## Changes direction of AGAINST stats, larger values are worse so they become negative 
    mutate(
      across(
        contains(
          "advancedStats.GA60"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.SA60"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.CA"
        ),
        ~ -.x
      ),
      across(
        contains(
          "advancedStats.FA"
        ),
        ~ -.x
      )
    ) 
  
  ### Calculates the percentile of the standardized values
  skatersZRank <- 
    skatersZ %>% 
    mutate(
      across(
        timeOnIce:advancedStats.FFPctRel,
        ~ percent_rank(.x)
      )
    )
  
  ### Adds percentile data to the standardized stats
  skatersZ <- 
    skatersZ %>% 
    left_join(
      skatersZRank,
      by = 
        c("name", "position", 
          "team", "esRank", 
          "ppRank", "shRank", 
          "gamesPlayed", "posGroup"),
      suffix = c(".ind", ".rank")
    )
  
  ### Removes text from the colnames
  colnames(skatersZ) <- 
    colnames(skatersZ) %>% 
    str_remove_all("advancedStats.")
  
  colnames(teamSkaterZ) <- 
    colnames(teamSkaterZ) %>% 
    str_remove_all("advancedStats.")
  
  ### Returns list of standardized skater and team stats as well as merged teamInfo with index data
  list(
    skaters = skatersZ,
    team = teamSkaterZ,
    teamInfo = teams,
    season = players$players$season %>% unique()
  ) %>% 
    return()
  ##----------------------------------------------------------------
}


##--------------------------------------------------------------------------
##  Creates a function that outputs a specific player card from a league   -
##--------------------------------------------------------------------------

playerCard <- function(chosen, leagueData){
  ## The league data used as an input is output from dataLoader()
  
  skatersZ <- leagueData$skaters
  
  teamSkaterZ <- leagueData$team
  
  teams <- leagueData$teamInfo
  
  ## Checks if the chosen player exists in the league data
  if(!any(skatersZ$name == chosen)){
    stop(
      c(
        "The player does not exist in the data. ",
        paste(
          "You might have meant:",
          agrep(
            pattern = chosen, 
            x = skatersZ$name,
            value = TRUE
          )
        )
      )
    )
  }
  
  ## Selects the statistics that will be used for the player card
  selectedStats <- 
    c(
      "goals",
      "assists",
      "points",
      "ppPoints",
      "hits",
      "TA_GA",
      "shotsBlocked",
      "CFPct",
      "FFPct"
    )
  
  ## Joins individual and team standardized values
  skatersZ <- 
    skatersZ %>% 
    left_join(
      ## The team statistic is added with a suffix to distinguish the two statistics
      ## This is required as the individual statistics already has .ind so no duplicates will be joined
      teamSkaterZ %>% 
        rename_with(
          .cols = -team,
          ~ paste0(.x,".teams")
        ),
      by = c("team")
    )
  
  
  ##----------------------------------------------------------------
  ##              Creates the initial visualization                -
  ##----------------------------------------------------------------
  
  visData <- 
    skatersZ %>% 
    
    ## Filters the chosen player
    filter(
      name == chosen
    ) %>% 
    
    ## Joins team information, specifically the team colors
    left_join(
      teams %>% 
        select(
          abbreviation,
          colors
        ),
      by = c("team" = "abbreviation")
    ) %>% 
    relocate(
      c(colors),
      .after = team
    ) %>% 
    
    ## Rescales time on ice statistics to original scale
    mutate(
      across(
        contains("timeOnIce.ind"),
        ~ rescale(.x) %>%
          ## Converts the value to a lubridate format
          seconds_to_period()
      ),
      
      ## Adds a 0 to values that are less than 10 in order to create a good output format
      ## 1:1 or 01:01 for 1 minute and 1 second 
      across(
        contains("timeOnIce.ind"),
        ~ paste(
          if_else(
            minute(.x) > 9,
            minute(.x) %>% round(0) %>% as.character(),
            paste(0, minute(.x) %>% round(0), sep = "")
          ),
          if_else(
            second(.x) > 9,
            second(.x) %>% round(0) %>% as.character(),
            paste(0, second(.x) %>% round(0), sep = "")
          ), 
          sep = ":")
      )
    ) %>% 
    
    ## Unnests the data frame to one level
    do.call(
      what = data.frame,
      args = .
    ) %>% 
    
    ## Adds team information again but this time only the logo image
    left_join(
      teams %>% 
        select(
          abbreviation,
          logoImage
        ),
      by = c("team" = "abbreviation") 
    ) %>% 
    relocate(
      logoImage,
      .after = team
    )
  
  ## Checks if the player name is too long for the default size (14)
  if(nchar(chosen) > 12){
    mainTextSize <- 10
  } else {
    mainTextSize <- 14
  }
  
  ### Creates the initial base for the visualization
  ##  Uses the visualization data on a plain background colored by the team colors
  main <- 
    ggplot(visData) + theme_minimal() +
    
    ## Creates the title 
    annotate(
      "text", 
      x = 4.25, 
      y = 29.5, 
      label = paste(visData$name, " (", visData$position,")", sep = ""), 
      color = "white", # visData$colors.text was used but dark on dark occurred
      size = mainTextSize,
      family = "monospace",
      hjust = 0.5
    ) +
    
    ## Sets the dimensions of the plot
    coord_cartesian(ylim = c(0, 30), xlim = c(0, 10)) +
    labs(x = "", y = "") +
    theme(
      panel.background = element_rect(fill = visData$colors.primary),
      panel.grid = element_blank(),
      plot.margin = unit(c(0,0,-0.75,-0.75), "cm")
    ) + 
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL)
  
  
  ##----------------------------------------------------------------
  ##        Creates the data for the statistic visualization       -
  ##----------------------------------------------------------------
  
  ### Further processes the visualization data to value, group and id columns (pivot_longer)
  visDataLong <-
    visData %>% 
    
    ## Only pivots the statistic variables, not identifiers
    pivot_longer(
      -c(
        name:gamesPlayed,
        contains("timeOnIce"),
        posGroup
      ),
      ## The .value indicates that the value variable will be split based on a suffix
      names_to = c("stat", ".value"),
      ## The value split is based on column names with . as a splitting character
      names_pattern = "([^\\.]+)\\.(.*)$"
    ) %>%
    ## Renames the two value variables from pivot_longer to better names
    rename(
      Individual = ind,
      Team = teams
    ) %>% 
    
    ## Pivots even more to get individual and team statistics as observations instead of variables
    ## measure_type indicates if the statistic is individual or team
    ## score indicates what the value of the statistic is (normalized)
    pivot_longer(
      c(Individual, Team),
      names_to = "measure_type",
      values_to = "score"
    ) %>% 
    
    ## Filter the selected stats for the player card
    filter(
      stat %in% selectedStats
    ) %>% 
    mutate(
      ## Reformats the statistic (group) variable to a factor with a specific order set in the order of selectedStats
      ## Changes the labels to easier read ones
      stat = 
        factor(
          stat,
          levels = 
            selectedStats,
          labels = 
            c(
              "Goals",
              "Assists",
              "Points",
              "PP Points",
              "Hits",
              "TkA/GvA",
              "Blocked Shots",
              "Corsi For %",
              "Fenwick For %"
            )
        ),
      ## Round the percentile to 3 decimal places
      rank = rank %>% round(3),
      ## Pastes the percentile value alongside the statistic name for use as a facet title
      statPerc = 
        paste0(stat, "\n(", rank*100, "%)") %>% 
        factor(
          levels = unique(.)
        )
    ) %>% 
    ## Removes all Time On Ice stats from the visualization
    select(
      -(timeOnIce.rank:shTimeOnIce.teams)
    )
  
  
  ##---------------------------------------------------------------
  ##              Creates the statistic visualization             -
  ##---------------------------------------------------------------
  
  statistics <- 
    ggplot(visDataLong) + 
    
    ## The y (height) is the standardized score
    aes(x = 1, y = score) + 
    
    ## A bar chart where the fill is defined by the score
    ## Group defines that the individual and team scores should be included
    ## dodge puts the two bars side by side
    geom_bar(
      aes(fill = score, group = measure_type),
      stat = "identity",
      position = "dodge"
    ) +
    
    ## Sets the viewport of the diagram to always show from -3 to 3 standard deviations from the mean
    coord_cartesian(
      ylim = c(-3, 3)
    ) + 
    
    ## Changes theme
    theme_bw() + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) + 
    
    ## Removes x-axis scale
    scale_x_discrete(
      breaks = NULL
    ) +
    
    ## Changes y-axis scale to show breaks at every 1.5 steps
    scale_y_continuous(
      breaks = 
        seq(-3, 3, 1.5)
    ) + 
    
    ## The fill of the bars are set by their score from muted red (bad) to muted green (good)
    scale_fill_gradientn(
      aes(fill = score),
      limits = c(-3,3),
      colors = 
        c(
          scales::muted("red"),
          "white",
          scales::muted("green")
        ),
      guide = FALSE,
      ## If the score is outside the limits the value is squished to the min/max (otherwise it would be gray)
      oob = scales::squish
    ) +
    
    ## As the team bar is not of interest, a new fill scale is added
    new_scale_fill() +
    
    ## This new one is a bar chart on top of the earlier, filled by measure_type
    geom_bar(
      aes(fill = measure_type),
      color = "black",
      stat = "identity",
      position = "dodge"
    ) +
    ## The individual bars is filled with NA to show the earlier bar color, team bar is just gray
    scale_fill_manual(
      "",
      values = c(NA, "gray") # Team bar tested with visData$colors.secondary, but it was too colorful
    ) +
    ## Creates a tougher horizontal line on y = 0 to indicate league mean
    geom_hline(yintercept = 0, size = 1) + 
    
    ## Creates a facet grid based on the statistic (including the percentile)
    facet_wrap(
      vars(statPerc),
      nrow = 3
    ) + 
    
    ## Revises the theme to make the text and data easy to read
    theme(
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold", size = 10),
      
      plot.background = element_rect(fill = NA, color = NA),
      
      text = element_text(color = "white", family = "mono"),
      
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(color = "white", size = 10),
      axis.ticks.y = element_line(color = "white"),
      
      legend.background = element_rect(fill = NA, color = NA),
      legend.text = element_text(size = 10)
    ) +
    labs(x = "", y = "z-score")
  
  
  ##---------------------------------------------------------------
  ##                Building the card using cowplot               -
  ##---------------------------------------------------------------
  ##### NOT CURRENTLY USED AS THE MAGICK PACKAGE IS NEEDED TO ADD SVG LOGOS
  # ggdraw(main) + 
  #   draw_plot(
  #     data, 
  #     x = 0.05,
  #     y = 0.1,
  #     width = 0.95,
  #     height = 0.75
  #   ) + 
  #   draw_text(
  #     text = 
  #       c("A z-score is the player's or team's average unit of standard deviation from the league mean value."),
  #     x = 0.1,
  #     y = 0.1,
  #     hjust = 0,
  #     size = 8,
  #     color = "white"
  #   ) + 
  #   draw_text(
  #     text = "Created by: \nCanadice",
  #     x = 0.927,
  #     y = 0.12,
  #     hjust = 0.5,
  #     size = 8,
  #     color = "white"
  #   ) +
  #   draw_image(
  #     visData$logo,
  #     x = 0.6,
  #     y = 0.6,
  #     width = 0.4,
  #     height = 0.4,
  #     scale = 0.5
  #   ) 
  
  
  ##----------------------------------------------------------------
  ##          Building the card using ggplot and svg files         -
  ##----------------------------------------------------------------
  
  ggCard <- 
    ## Starts with the plain background and title
    main +
    
    ## Add the statistic data 
    annotation_custom(
      statistics %>% ggplotGrob(),
      xmin = 0,
      xmax = 10.5,
      ymin = 1,
      ymax = 26
    ) +
    
    ## Adds informative text on how to read the data on the bottom
    annotate(
      "text",
      x = 0,
      y = 0.25,
      hjust = 0,
      label =
        c("A z-score is the player's, or team's average, unit of standard deviation from the league mean value.\n\n",
          "The time on ice ranking is based on positional (forward/defenseman) ranking in the team."),
      # \nA high Corsi against is bad, so the scale is reversed to correspond to the same color gradient."
      size = 3.5,
      color = "white"
    ) +
    
    ## Adds subtitle
    annotate(
      "text",
      x = 1.3,
      y = 27,
      label = "Statistic and percentile of players in the league",
      size = 4.5,
      color = "white",
      family = "mono",
      fontface = 2,
      hjust = 0
    ) +
    
    ## Adds author in bottom right
    annotate(
      "text",
      x = 10,
      y = 0.25,
      hjust = 0.5,
      label = "Created by: \nCanadice",
      size = 3,
      color = "white"
    ) + 
    
    ## Adds season of the data
    annotate(
      "text",
      x = 0.20,
      y = 26.75,
      label = paste("S", leagueData$season, sep = ""),
      size = 6,
      color = "white",
      family = "mono",
      fontface = 2
    ) + 
    
    ## Adds the time on ice as information along the right hand side of the card
    annotate(
      "text",
      x = 9.50, 
      y = 21,
      family = "mono",
      color = "white",
      size = 4,
      label = 
        paste(
          c(
            "ESTOI", 
            "PPTOI", 
            "SHTOI"
          ),
          c(
            visData$esTimeOnIce.ind,
            visData$ppTimeOnIce.ind,
            visData$shTimeOnIce.ind
          ),
          sep = " - "
        ) %>% 
        paste(
          .,
          paste(
            "(",
            c(
              visData$esRank,
              visData$ppRank,
              visData$shRank
            ),
            ")",
            sep = ""
          ),
          sep = "\n"
        ) %>% 
        paste(collapse = "\n"),
      fontface = 2
    )
  
  ### Initialize the drawing into the variable card
  card <- image_graph(res = 96)
  print(ggCard)
  dev.off()
  
  ### Using magick to add the svg image of the logo to the top right of the card
  card %>% 
    image_composite(
      visData$logoImage[[1]] %>% 
        image_resize(100),
      offset = "+680+15", 
    )
}



