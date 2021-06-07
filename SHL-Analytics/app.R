#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(XML)
require(ggplot2)
require(stringr)
require(stringi)
require(dplyr)
require(tidyr)
require(RColorBrewer)
require(lubridate)
require(plotly)
require(fuzzyjoin)
require(shiny)
require(DT)
require(knitr)
require(ggmap)
require(kableExtra)
require(janitor)
require(usmap)
require(shinythemes)


##---------------------------------------------------------------
##                Setting the working directory                 -
##---------------------------------------------------------------

## Moves the working directory to the main Github folder
setwd("..")

##----------------------------------------------------------------
##          Loading the API functions and some data sets         -
##----------------------------------------------------------------

fileSources <- 
    c(
        "scripts/API",
        "SHL-Analytics/app-documents"
    )
    
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

sapply(rmdFiles, rmarkdown::render, quiet = T, output_dir = "SHL-Analytics/app-documents")

##---------------------------------------------------------------
##                  Defining the user interface                 -
##---------------------------------------------------------------


ui <- 
    fluidPage(
        ### Adding the theme of the app
        theme = shinytheme("yeti"),
        navbarPage(
            "Tools for SHL",
            ## Welcoming screen
            tabPanel(
                "Welcome",
                welcome_ui(id = "welcome")
            )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    ##---------------------------------------------------------------
    ##          Loading each of the different backend sites         -
    ##---------------------------------------------------------------
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
