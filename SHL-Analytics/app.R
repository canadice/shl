#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
require(fuzzyjoin)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)
require(ggpubr)
require(png)
require(grid)
require(plotly)

# Packages for svg images
require(magick)
require(rsvg)

## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)
require(stringi)

## Loading package that can talk to Google Sheets
require(googlesheets4)

## Loading Shiny packages
require(shiny)
require(DT)
require(knitr)
require(ggmap)
require(kableExtra)
require(janitor)
require(usmap)
require(shinythemes)
require(shinycssloaders)

##----------------------------------------------------------------
##          Loading the API functions and some data sets         -
##----------------------------------------------------------------

source("https://raw.githubusercontent.com/canadice/shl/main/scripts/API/apiSetup.R")

fileSources <- c("app-documents")

## Loads and runs RMarkdown files
rmdFiles <- 
    sapply(
        X = fileSources,
        FUN = function(x){
            list.files(path = x, pattern = ".Rmd$") %>% 
                paste(x, ., sep = "/")
        },
        simplify = TRUE,
        USE.NAMES = FALSE
    ) %>% 
    unlist() %>% 
    .[str_detect(., pattern = ".Rmd")]

sapply(rmdFiles, rmarkdown::render, quiet = T, output_dir = "app-documents")

## Loads files
sapply(
    X = fileSources,
    FUN = function(x){
        files <- list.files(path = x, pattern = ".R$")
        
        sapply(
            X = paste(x, files, sep = "/"),
            FUN = source
        )
    }
)


##---------------------------------------------------------------
##                  Defining the user interface                 -
##---------------------------------------------------------------


ui <- 
    fluidPage(
        ### Adding the theme of the app
        theme = shinytheme("yeti"),
        
        ## Creating the navigation pane
        navbarPage(
            "SHL Analytics",
            
            
            #################################################################
            ##                           Welcome                           ##
            #################################################################
            tabPanel(
                "Welcome",
                welcome_ui(id = "welcome")
            ),
            
            
            ##################################################################
            ##                    Players by draft class                    ##
            ##################################################################
            tabPanel(
                "Players",
                playersUI(id = "playersUI")
            ),
            
            ##################################################################
            ##                       Visualizations                         ##
            ##################################################################
            tabPanel(
                "Visualizations",
                tabsetPanel(
                    ## Player visualization using radar charts
                    tabPanel(
                        "Player Attributes",
                        titlePanel("Visualization of player attributes"),
                        radarUI(id = "radarUI")
                    ),
                    ## Visualizing the player card
                    tabPanel(
                        "Player Cards",
                        ## Application title
                        titlePanel(
                            "Player cards using z-scores"),
                        playerCardUI(id = "pCardUI")
                    ),
                    ## Player similarity using multidimensional scaling
                    tabPanel(
                        "Player Similarity",
                        ## Application title
                        titlePanel(
                            "Player similarity using multidimensional scaling"),
                        mdsUI(id = "simUI")
                    )
                )
            )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    ##---------------------------------------------------------------
    ##          Loading each of the different backend sites         -
    ##---------------------------------------------------------------
    mdsSERVER(id = "simUI")
    
    radarSERVER(id = "radarUI")
    
    playersSERVER(id = "playersUI")
    
    playerCardSERVER(id = "pCardUI")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
