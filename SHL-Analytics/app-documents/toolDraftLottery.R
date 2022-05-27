
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

draftLotteryUI <- function(id){
  ns <- NS(id)
  
  draftTeams <- 
    teamInfo %>% 
    filter(!is.na(fhmID) & leagueID < 2) %>% 
    arrange(
      leagueID,
      team
    ) %>% 
    select(team) %>% 
    unlist() %>% 
    unname()
  
  tagList(
    fluidRow(
      column(
        width = 8,
        offset = 2,
        box(
          id = ns("selection"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          title = "Lottery Setup",
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("team1"),
                label = "First Team",
                choices = draftTeams,
                selected = draftTeams[1]
              ),
              selectInput(
                inputId = ns("original1"),
                label = "via",
                choices = c("",draftTeams),
                selected = ""
              )
            ),
            column(
              width = 6,br(),br(),
              numericInput(
                inputId = ns("balls1"),
                label = "Lottery balls",
                value = 30,
                min = 1
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("team2"),
                label = "Second Team",
                choices = draftTeams,
                selected = draftTeams[2]
              ),
              selectInput(
                inputId = ns("original2"),
                label = "via",
                choices = c("",draftTeams),
                selected = ""
              )
            ),
            column(
              width = 6,br(),br(),
              numericInput(
                inputId = ns("balls2"),
                label = "Lottery balls",
                value = 25,
                min = 1
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("team3"),
                label = "Third Team",
                choices = draftTeams,
                selected = draftTeams[3]
              ),
              selectInput(
                inputId = ns("original3"),
                label = "via",
                choices = c("",draftTeams),
                selected = ""
              )
            ),
            column(
              width = 6,
              br(),br(),
              numericInput(
                inputId = ns("balls3"),
                label = "Lottery balls",
                value = 20,
                min = 1
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("team4"),
                label = "Fourth Team",
                choices = draftTeams,
                selected = draftTeams[4]
              ),
              selectInput(
                inputId = ns("original4"),
                label = "via",
                choices = c("",draftTeams),
                selected = ""
              )
            ),
            column(
              width = 6,br(),br(),
              numericInput(
                inputId = ns("balls4"),
                label = "Lottery balls",
                value = 15,
                min = 1
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              offset = 3,
              actionButton(
                inputId = ns("drawLottery"),
                label = "Roll the dice!",
                width = "100%"
              )
            )
          )
        ) %>% 
          div(
            id = ns("selection-main")
          ),
        shinyjs::hidden(
          box(
            id = ns("wait"),
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            width = NULL,
            title = NULL,
            h3(
              "The balls are being drawn",
              align = "center"
            ),
            p(img(src="lottery.gif"),align = "center")
          ) %>% 
            div(
              id = ns("wait-main")
            )
          ),
        shinyjs::hidden(
          box(
            id = ns("results"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = NULL,
            title = "Lottery Results",
            fluidRow(
              column(
                width = 4,
                offset = 1,
                h4("1st pick", align = "center"),
                actionButton(
                  inputId = ns("firstSelection"),
                  label = "Show the pick",
                  width = "100%"
                ),
                br()
              ),
              column(
                width = 4,
                offset = 1,
                align = "center",
                imageOutput(
                  outputId = ns("firstTeam"),
                  height = "100%"
                )
              ) %>% 
                div(
                  id = ns("first-draw")
                )%>% 
                shinyjs::hidden()
            )%>% 
              div(style = "height:100px"),
            hr(),
            fluidRow(
              column(
                width = 4,
                offset = 1,
                h4("2nd pick", align = "center"),
                actionButton(
                  inputId = ns("secondSelection"),
                  label = "Show the pick",
                  width = "100%"
                ),
                br()
              ),
              column(
                width = 4,
                offset = 1,
                align = "center",
                imageOutput(
                  outputId = ns("secondTeam"),
                  height = "100%"
                )
              ) %>% 
                div(
                  id = ns("second-draw")
                )%>% 
                shinyjs::hidden()
            )%>% 
              div(style = "height:100px"),
            hr(),
            fluidRow(
              column(
                width = 4,
                offset = 1,
                h4("3rd pick", align = "center"),
                actionButton(
                  inputId = ns("thirdSelection"),
                  label = "Show the pick",
                  width = "100%"
                ),
                br()
              ),
              column(
                width = 4,
                offset = 1,
                align = "center",
                imageOutput(
                  outputId = ns("thirdTeam"),
                  height = "100%"
                )
              ) %>% 
                div(
                  id = ns("third-draw")
                )%>% 
                shinyjs::hidden()
            )%>% 
              div(style = "height:100px"),
            hr(),
            fluidRow(
              column(
                width = 4,
                offset = 1,
                h4("4th pick", align = "center"),
                actionButton(
                  inputId = ns("fourthSelection"),
                  label = "Show the pick",
                  width = "100%"
                ),
                br()
              ),
              column(
                width = 4,
                offset = 1,
                align = "center",
                imageOutput(
                  outputId = ns("fourthTeam"),
                  height = "100%"
                )
              ) %>% 
                div(
                  id = ns("fourth-draw")
                ) %>% 
                shinyjs::hidden()
            )%>% 
              div(style = "height:100px"),
            hr(),
            br(),
            br(),
            fluidRow(
              column(
                width = 6,
                offset = 3,
                actionButton(
                  inputId = ns("backLottery"),
                  label = "Go back",
                  width = "100%",
                  class = "btn-info"
                )
              )
            )
          ) %>% 
            div(
              id = ns("results-main")
            )
        )
      )
    )
  )
}


## Backend for vizualizations
draftLotterySERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      pickImage <- function(picks){
        if(str_detect(picks, pattern = "via")){
          
          teams <- (picks %>% str_split(" via ") %>% unlist())
            
          data1 <- teamInfo %>% 
            filter(team == teams[1])
          
          data2 <- teamInfo %>% 
            filter(team == teams[2])
          
          tempImage <- 
            image_montage(
              c(
                data1$logoImage[[1]],
                data2$logoImage[[1]]
              ), geometry = "x150+10+10", tile = "2x1") %>% image_annotate("via", location = "+145+120", size = 20)
          
        } else {
          data <- teamInfo %>% 
            filter(team == picks)
          
          tempImage <- 
            image_draw(
              data$logoImage[[1]],
              xlim = c(0,1), 
              ylim = c(0,1)
            )
        }
        
        return(tempImage)
      }
      
      
      ##---------------------------------------------------------------
      ##      Disable buttons when clicked and data is processing     -
      ##---------------------------------------------------------------
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$drawLottery
        },
        {
          shinyjs::toggle(id = "selection-main")
          
          shinyjs::toggle(id = "wait-main")
          
          Sys.sleep(5)
          
          shinyjs::toggle(id = "wait-main")
          
          shinyjs::toggle(id = "results-main")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$backLottery
        },
        {
          shinyjs::toggle(id = "selection-main")
          shinyjs::toggle(id = "results-main")
          
          shinyjs::enable(id = "firstSelection")
          shinyjs::enable(id = "secondSelection")
          shinyjs::enable(id = "thirdSelection")
          shinyjs::enable(id = "fourthSelection")
          
          shinyjs::hide(id = "first-draw")
          shinyjs::hide(id = "second-draw")
          shinyjs::hide(id = "third-draw")
          shinyjs::hide(id = "fourth-draw")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$firstSelection 
        },
        {
          shinyjs::disable(id = "firstSelection")
          Sys.sleep(1)
          shinyjs::show(id = "first-draw")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$secondSelection 
        },
        {
          shinyjs::disable(id = "secondSelection")
          Sys.sleep(1)
          shinyjs::show(id = "second-draw")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$thirdSelection 
        },
        {
          shinyjs::disable(id = "thirdSelection")
          Sys.sleep(1)
          shinyjs::show(id = "third-draw")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        ## Add all buttons or settings that can be selected
        {
          input$fourthSelection 
          
        },
        {
          shinyjs::disable(id = "fourthSelection")
          Sys.sleep(1)
          
          shinyjs::show(id = "fourth-draw")
        },
        ignoreInit = TRUE
      )
      
      
      ##------------------------------------------------------------------------
      ##  Initialize reactiveValues that will be used as interactive options   -
      ##------------------------------------------------------------------------
      
      rv <- reactiveValues(firstPick = NULL, secondPick = NULL, thirdPick = NULL, fourthPick = NULL)
      
      
      ##----------------------------------------------------------------
      ##                        Doing the draw                         -
      ##----------------------------------------------------------------
      
      observeEvent(
        
        {
          input$drawLottery
        },
        {
          drawProbability <-
            c(
              input$balls1 %>% isolate(),
              input$balls2 %>% isolate(),
              input$balls3 %>% isolate(),
              input$balls4 %>% isolate()
            )
          
          og1 <- input$original1 %>% isolate()
          og2 <- input$original2 %>% isolate()
          og3 <- input$original3 %>% isolate()
          og4 <- input$original4 %>% isolate()
          
          t1 <- input$team1 %>% isolate()
          t2 <- input$team2 %>% isolate()
          t3 <- input$team3 %>% isolate()
          t4 <- input$team4 %>% isolate()
          
          teams <- 
            c(
              if(og1 == "") {t1} else {paste(t1, "via", og1)},
              if(og2 == "") {t2} else {paste(t2, "via", og2)},
              if(og3 == "") {t3} else {paste(t3, "via", og3)},
              if(og4 == "") {t4} else {paste(t4, "via", og4)}
            )
          
          drawn <- 
            sample(teams, size = length(teams), prob = drawProbability)
          
          rv$firstPick <- drawn[1]
          rv$secondPick <- drawn[2]
          rv$thirdPick <- drawn[3]
          rv$fourthPick <- drawn[4]
          
        }
      )
      
      output$firstTeam <- renderImage({
        
        pickImage(rv$firstPick)  %>% 
          image_resize("x100") %>% 
          image_write(tempfile(fileext = "png"), format = "png") %>% 
          list(src = ., contentType = "image/png")
        
      },deleteFile = TRUE)
      
      output$secondTeam <- renderImage({
        
        pickImage(rv$secondPick)  %>% 
          image_resize("x100") %>% 
          image_write(tempfile(fileext = "png"), format = "png") %>% 
          list(src = ., contentType = "image/png")
        
      },deleteFile = TRUE)
      
      output$thirdTeam <- renderImage({
        
        pickImage(rv$thirdPick)  %>% 
          image_resize("x100") %>% 
          image_write(tempfile(fileext = "png"), format = "png") %>% 
          list(src = ., contentType = "image/png")
        
      },deleteFile = TRUE)
      
      output$fourthTeam <- renderImage({
        
        pickImage(rv$fourthPick)  %>% 
          image_resize("x100") %>% 
          image_write(tempfile(fileext = "png"), format = "png") %>% 
          list(src = ., contentType = "image/png")
        
      },deleteFile = TRUE)
      
    }
  )
}

