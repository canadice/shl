
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
        
        
        ## Select player
        withSpinner(uiOutput(ns("selectSkater"))),
        
        # ## Type the name of the skater you want to see
        # textInput(
        #   inputId = ns("skater"),
        #   label = "Which player do you want to visualize?",
        #   value = "Theo Morgan",
        #   placeholder = "Theo Morgan"
        # ),
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
      
      
      output$selectSkater <- renderUI({
        skaterNames <- 
          selectedData()$skaters %>% 
          select(name) %>% 
          arrange(name) %>% 
          unlist() %>% 
          unname()
        
        selectInput(
          inputId = session$ns("skater"),
          label = "Which player do you want to visualize?",
          choices = skaterNames,
          selected = NULL,
          selectize = TRUE, 
          size = NULL
        )
      })

      output$playerCard <- 
        renderImage(
          {
            if(input$skater %>% is.null()){
              tempImage <- 
                image_blank(
                  height= 600, width = 800, 
                  color = "none") %>% 
                image_write(tempfile(fileext = "png"), format = "png")
            } else {
              progress <- Progress$new(session, min=1, max=15)
              on.exit(progress$close())
              
              progress$set(message = 'Calculation in progress',
                           detail = 'This may take a while...')
              
              for (i in 1:15) {
                progress$set(value = i)
                Sys.sleep(0.25)
              }
              
              tempImage <- 
                playerCard(input$skater, selectedData()) %>% 
                image_write(tempfile(fileext = "png"), format = "png")
              
              
            }
            list(src = tempImage, contentType = "image/png")
          },
          deleteFile = TRUE
        )
      
    }
  )
}

