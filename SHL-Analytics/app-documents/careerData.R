
############################################################################
############################################################################
###                                                                      ###
###         TOOL TO AGGREGATE AND SHOW CAREER DATA                  ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

careerDataUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      ##################################################################
      ##                      Skater information                      ##
      ##################################################################
      
      column(
        width = 12,
        box(
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Filters",
          column(
            width = 4, 
            selectInput(
              inputId = ns("league"),
              label = "Select league",
              width = "100%",
              choices = 
                c(
                  "SHL" = 0,
                  "SMJHL" = 1, 
                  "IIHF" = 2,
                  "WJC" = 3
                )
            )
          ),
          column(
            width = 8,
            selectInput(
              inputId = ns("season"),
              label = "Select season",
              width = "100%",
              choices = 
                max(historySkaterSeason$Season):1
            )
          )
        )
      )
    ),
    ##################################################################
    ##                       Season by Season                       ##
    ##################################################################
    column(
      width = 12,
      box(
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        title = "Careers",
        tabBox(
          width = NULL,
          tabPanel("Skaters", DTOutput(ns("skaterCareer"))),
          tabPanel("Goalies", DTOutput(ns("goalieCareer")))
        )
      ),
      br(),
      em("Disclaimer: Some data may be wrong as historical data from the first seasons are hard to dig up."),
      em("Starting from S53, FHM6 is used to simulate the games which changes the statistics that can be exported."),
      em("S60 saw a change in the update scale to make lower TPE players more impactful."),
      em("S65 restricted tactics to only team (no unit/individual tactical sliders allowed)."),
      em("S65 saw the SHL and SMJHL move to FHM8 while the IIHF and WJC moved to FHM8 in S66."),
      br(),
      br()
    ) %>% 
      fluidRow()
  )
}


## Backend for vizualizations
careerDataSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      # Opens the connection to the database
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      careerOptionList <-
        list(
          ordering = FALSE, 
          ## Removes pages in the table
          paging = FALSE,
          dom = 't'
        )
      
      seasonRankOptionList <-
        list(
          ordering = FALSE, 
          scrollX = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          dom = 't',
          columnDefs = 
            list(
              list(
                targets = 2,
                class = "dt-right"
              )
            )
        )
      
      ### Find and filters the data based on settings. 
      skaterData <- reactive({
        
        league <- input$league
        season <- input$season
        
        tbl(con, "skaterHistory") %>% 
          filter(
            leagueID == league,
            Season == season
          ) %>% 
          collect() %>% 
          left_join(
            teamInfo %>% 
              select(
                teamID,
                franchiseID,
                team,
                abbr,
                primary,
                secondary
              ),
            by = c("newTeamID" = "teamID")
          )
      })
      
      goalieData <- reactive({
        league <- input$league
        season <- input$season
        
        tbl(con, "goalieHistory") %>% 
          filter(
            leagueID == league,
            Season == season
          ) %>% 
          collect() %>% 
          left_join(
            teamInfo %>% 
              select(
                teamID,
                franchiseID,
                team,
                abbr,
                primary,
                secondary
              ),
            by = c("newTeamID" = "teamID")
          )
      })
            
      output$skaterCareer <- DT::renderDT({
        skaterData() %>% 
          select(
            Name,
            Season,
            isPlayoffs,
            leagueID,
            franchiseID = franchiseID.y,
            team,
            GamesPlayed:FFPctRel
          ) %>% 
          rename_with(
            str_to_upper
          ) 
      }, 
      filter = 'bottom',
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '400px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = '600px',
          # pageLength = 10,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'Bfrtip',
          bInfo = FALSE,
          buttons = c('copy', 'csv', 'excel')#,
          # ## Defines which columns the table shows by default
          # columnDefs = list(
          #   list(
          #     visible = TRUE,
          #     targets = 0:6
          #   ),
          #   list(
          #     visible = FALSE,
          #     targets = '_all'
          #   )
          # )
        ),
      extensions = c('Buttons')
      )
      
      output$goalieCareer <- DT::renderDT({
        goalieData() %>% 
          select(
            Name,
            Season,
            isPlayoffs,
            leagueID,
            franchiseID = franchiseID.y,
            team,
            GamesPlayed:GameRating
          ) %>% 
          rename_with(
            str_to_upper
          ) 
      }, 
      filter = 'bottom',
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '400px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = '600px',
          # pageLength = 10,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'Bfrtip',
          bInfo = FALSE,
          buttons = c('copy', 'csv', 'excel')#,
          # ## Defines which columns the table shows by default
          # columnDefs = list(
          #   list(
          #     visible = TRUE,
          #     targets = 0:6
          #   ),
          #   list(
          #     visible = FALSE,
          #     targets = '_all'
          #   )
          # )
        ),
      extensions = c('Buttons')
      )
      
    }
  )
}

