
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
        withSpinner(
          uiOutput(ns("selectLeague")),
          size = 0.75,
          hide.ui = FALSE),
        
        ## Season selection
        withSpinner(
          uiOutput(ns("selectSeason")),
          size = 0.75,
          hide.ui = FALSE),
        
        ## Select player
        withSpinner(
          uiOutput(ns("selectSkater")),
          size = 0.75,
          hide.ui = FALSE),
        
        ## Generate player card
        actionButton(
          inputId = ns("draw"),
          label = "Generate the card",
          width = '100%'
        ),
        br(),
        br(),
        em(paste("The data is taken directly from the SHL Index.")),
        shinyjs::hidden(
          p(
            id = ns("instruction"), 
            br(),
            em("Right click on the image to copy and/or save it to your own device.")
          )
        )

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
      
      observeEvent(
        input$draw,
        {
          shinyjs::toggleState(id = "season")
          shinyjs::toggleState(id = "league")
          shinyjs::toggleState(id = "skater")
          shinyjs::toggleState(id = "draw")
        },
        ignoreInit = TRUE, ignoreNULL = TRUE
      )
      
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
          selectize = TRUE
        )
      })
      
      output$selectSeason <- 
        renderUI(
          {
            numericInput(
              inputId = session$ns("season"),
              label = "Season",
              min = 53,
              max = 61,
              value = 60
            )
          }
        )
      
      output$selectLeague <- 
        renderUI(
          {
            radioButtons(
              inputId = session$ns("league"),
              label = "Select league",
              choices = 
                c(
                  "SHL" = 0,
                  "SMJHL" = 1
                ),
              selected = 0
            )
          }
        )

      output$playerCard <- 
        renderImage(
          {
            ## Only draws the image when the draw button is clicked
            input$draw
            
            data <- selectedData() %>% isolate()
            
            skater <- input$skater %>% isolate()
            
            if(skater %>% is.null()){
              tempImage <- 
                image_blank(
                  height= 600, width = 800, 
                  color = "none") %>% 
                image_write(tempfile(fileext = "png"), format = "png")
            } else if(!((skater) %in% data$skaters$name)){
              tempImage <- 
                image_blank(
                  height= 600, width = 800, 
                  color = "none") %>% 
                image_write(tempfile(fileext = "png"), format = "png")
              
              shinyjs::toggleState(id = "league")
              shinyjs::toggleState(id = "season")
              shinyjs::toggleState(id = "skater")
              shinyjs::toggleState(id = "draw")
              shinyjs::show(id = "instruction")
            } else {
              progress <- Progress$new(session, min=1, max=30)
              on.exit(progress$close())
              
              progress$set(message = 'The player card is being drawn up!',
                           detail = 'This may take a while...')
              
              for (i in 1:20) {
                progress$set(value = i)
                Sys.sleep(0.25)
              }
              
              tempImage <- 
                playerCard(skater, data) %>% 
                image_write(tempfile(fileext = "png"), format = "png")
              
              for (i in 21:30) {
                progress$set(value = i)
                Sys.sleep(0.1)
              }
              
              shinyjs::toggleState(id = "league")
              shinyjs::toggleState(id = "season")
              shinyjs::toggleState(id = "skater")
              shinyjs::toggleState(id = "draw")
              shinyjs::show(id = "instruction")
              
            }
            
            return(list(src = tempImage, contentType = "image/png"))
          },
          deleteFile = TRUE
        )
      
    }
  )
}

