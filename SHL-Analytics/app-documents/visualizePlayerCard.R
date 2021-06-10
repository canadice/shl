
############################################################################
############################################################################
###                                                                      ###
###         TOOL TO VISUALIZE DISTRIBUTION OF PLAYERS ATTRIBUTES         ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

playerCardUI <- function(id){
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of type of player/goalie
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        ## Selection of league
        radioButtons(
          inputId = ns("league"),
          label = "Select league",
          choices = 
            c(
              "SHL" = 0,
              "SMJHL" = 1
            ),
          selected = 0
        ),
        
        ## Season selection
        numericInput(
          inputId = ns("season"),
          label = "Season",
          min = 53,
          max = 60,
          value = 59
        ),
        
        ## Type the name of the skater you want to see
        textInput(
          inputId = ns("skater"),
          label = "Which player do you want to visualize?",
          value = "Theo Morgan",
          placeholder = "Theo Morgan"
        ),
        em(paste("The data is taken directly from the SHL Index"))

      ),
      mainPanel(
        width = 8,
        imageOutput(
          outputId = ns("playerCard")
        )
      )
    )
  )
}


## Backend for vizualizations
playerCardSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      ## Selects the data based on input league and input season
      selectedData <- reactive({

        data <- 
          dataLoader(
            league = input$league, 
            season = input$season) 
        
        return(data)
      })
      
      
      output$playerCard <- renderImage({
        withProgress(
          message = "Calculations in progress",
          detail = "This may take a while...",
          value = 0,
          {
            for (i in 1:15) {
              incProgress(1/15)
              Sys.sleep(0.25)
            }
          }
          )
        playerCard(input$skater, selectedData())
      })
      
    }
  )
}

