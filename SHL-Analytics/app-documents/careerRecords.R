
############################################################################
############################################################################
###                                                                      ###
###         TOOL TO AGGREGATE AND SHOW A SKATERS CAREER                  ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

careerRecordsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      ##################################################################
      ##                         Data Filters                         ##
      ##################################################################
      
      column(
        width = 12,
        box(
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Data Filters",
          column(
            width = 4,
            actionButton(
              inputId = ns("playoffs"),
              label = "Playoffs?",
              width = "100%"
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = ns("league"),
              label = "SMJHL Stats?",
              width = "100%"
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = ns("careerOrSeason"),
              label = "Career Records?",
              width = "100%"
            )
          ),
          br(),
          br(),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("scopeFrom"),
                label = "From Season",
                width = "100%",
                choices = 1:max(historyGoalieSeason$Season),
                selected = 1
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId = ns("scopeTo"),
                label = "To Season",
                width = "100%",
                choices = 1:max(historyGoalieSeason$Season),
                selected = max(historyGoalieSeason$Season)
              )
            )
          ),
          shinyjs::hidden(
            h4(
              id = ns("processing"), 
              "The Historians are gathering your data for view, please hold...", 
              align = "center")
            )
        )
      )
    ),
    column(
      width = 12,
      box(
        title = "Skater Records",
        status = "primary",
        width = NULL,
        collapsible = TRUE,
        solidHeader = TRUE,
        column(
          width = 4,
          selectInput(
            inputId = ns("skaterStatOne"),
            label = "Select a statistic",
            choices = 
              c(
                "Goals",
                "Assists",
                "Points",
                "+/-" = "PlusMinus",
                "PIM" = "PenaltyMinutes",
                "Hits",
                "Shots",
                "ShotsBlocked",
                "TOI" = "MinutesPlayed"
              ),
            selected = "Goals",
            multiple = FALSE
          ),
          DTOutput(ns("skaterStatOne")) %>% withSpinner()
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("skaterStatTwo"),
            label = "Select a statistic",
            choices = 
              c(
                "Goals",
                "Assists",
                "Points",
                "+/-" = "PlusMinus",
                "PIM" = "PenaltyMinutes",
                "Hits",
                "Shots",
                "ShotsBlocked",
                "TOI" = "MinutesPlayed"
              ),
            selected = "Points",
            multiple = FALSE
          ),
          DTOutput(ns("skaterStatTwo")) %>% withSpinner()
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("skaterStatThree"),
            label = "Select a statistic",
            choices = 
              c(
                "Goals",
                "Assists",
                "Points",
                "+/-" = "PlusMinus",
                "PIM" = "PenaltyMinutes",
                "Hits",
                "Shots",
                "ShotsBlocked",
                "TOI" = "MinutesPlayed"
              ),
            selected = "Hits",
            multiple = FALSE
          ),
          DTOutput(ns("skaterStatThree")) %>% withSpinner()
        )
      )
    ),
    column(
      width = 12,
      box(
        title = "Goalie Records",
        status = "primary",
        width = NULL,
        collapsible = TRUE,
        solidHeader = TRUE,
        column(
          width = 4,
          selectInput(
            inputId = ns("goalieStatOne"),
            label = "Select a statistic",
            choices = 
              c(
                "Wins",
                "Losses",
                "OvertimeLosses",
                "Shutouts",
                "Minutes",
                "GoalsAgainst",
                "ShotsAgainst",
                "Assists"
              ),
            selected = "Wins",
            multiple = FALSE
          ),
          DTOutput(ns("goalieStatOne")) %>% withSpinner()
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("goalieStatTwo"),
            label = "Select a statistic",
            choices = 
              c(
                "Wins",
                "Losses",
                "OvertimeLosses",
                "Shutouts",
                "Minutes",
                "GoalsAgainst",
                "ShotsAgainst",
                "Assists"
              ),
            selected = "Shutouts",
            multiple = FALSE
          ),
          DTOutput(ns("goalieStatTwo")) %>% withSpinner()
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("goalieStatThree"),
            label = "Select a statistic",
            choices = 
              c(
                "Wins",
                "Losses",
                "OvertimeLosses",
                "Shutouts",
                "Minutes",
                "GoalsAgainst",
                "ShotsAgainst",
                "Assists"
              ),
            selected = "ShotsAgainst",
            multiple = FALSE
          ),
          DTOutput(ns("goalieStatThree")) %>% withSpinner()
        )
      )
    ),
    br(),
    em("* indicates that the player is still active."),
    br(),
    br(),
    em("Disclaimer: Some data may be wrong as historical data from the first seasons are hard to dig up."),
    em("Starting from S53 FHM is used to simulate the games which changes the statistics that can be exported."),
    em("S60 saw a change in the update scale to make lower TPE players more impactful."),
    br(),
    br()
  )
}


## Backend for vizualizations
careerRecordsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(
        input$careerOrSeason|
        input$league|
        input$playoffs,
        {
          shinyjs::disable(id = "playoffs")
          shinyjs::disable(id = "league")
          shinyjs::disable(id = "careerOrSeason")
          shinyjs::disable(id = "scopeTo")
          shinyjs::disable(id = "scopeFrom")
          shinyjs::show("processing")
        },
        ignoreInit = FALSE
      ) 
      
      
      
      skaterSeason <- reactive({
        league <- if_else(input$league %% 2 != 0, 2, 1)
        playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
        
        data <- 
          historySkaterSeason %>% 
          filter(
            LeagueId == league,
            isPlayoffs == playoffs
          ) %>% 
          select(
            -isPlayoffs
          ) %>%  
          left_join(
            teamInfo %>% 
              select(
                teamID,
                franchiseID,
                Inaugural.Season,
                team,
                abbr,
                primary,
                secondary
              ),
            by = c("newTeamID" = "teamID")
          ) %>% 
          left_join(
            playerLoader(league - 1)$players %>% 
              select(
                name
              ) %>% 
              mutate(
                currentlyActive = TRUE
              ),
            by = c("Player.Name" = "name")
          ) %>% 
          mutate(
            Player.Name = paste(Player.Name, if_else(!is.na(currentlyActive), "*", ""), sep = "")
          ) 
        
          if(input$careerOrSeason == 1){
            data <- 
              data %>% 
              group_by(Player.Name) %>% 
              summarize(
                Team = paste(team, collapse = " & "),
                across(
                  GamesPlayed:FaceoffWins,
                  sum,
                  na.rm = TRUE
                )
              )
          } else {
            data <- 
              data %>% 
              group_by(Season, Player.Name) %>% 
              summarize(
                Team = paste(team, collapse = " & "),
                across(
                  GamesPlayed:FaceoffWins,
                  sum,
                  na.rm = TRUE
                )
              )
          }
        
        return(data)
      })
      
      goalieSeason <- reactive({
        league <- if_else(input$league %% 2 != 0, 2, 1)
        playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
        
        data <- 
          historyGoalieSeason %>% 
          filter(
            LeagueId == league,
            isPlayoffs == playoffs,
            Season >= input$scopeFrom,
            Season >= input$scopeTo
          ) %>% 
          select(
            -isPlayoffs
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                teamID,
                franchiseID,
                Inaugural.Season,
                team,
                abbr,
                primary,
                secondary
              ),
            by = c("newTeamID" = "teamID")
          ) %>% 
          left_join(
            playerLoader(league - 1)$goalies %>% 
              select(
                name
              ) %>% 
              mutate(
                currentlyActive = 1
              ),
            by = c("Player.Name" = "name")
          ) %>% 
          mutate(
            Player.Name = paste(Player.Name, if_else(!is.na(currentlyActive), "*", ""), sep = "")
          ) 
        
        if(input$careerOrSeason == 1){
          data <- 
            data %>% 
            group_by(Player.Name) %>% 
            summarize(
              Team = paste(team, collapse = " & "),
              across(
                GamesPlayed:PenaltyShotSaves,
                sum,
                na.rm = TRUE
              )
            )
        } else {
          data <- 
            data %>% 
            group_by(Season, Player.Name) %>% 
              summarize(
                Team = paste(team, collapse = " & "),
                across(
                  GamesPlayed:PenaltyShotSaves,
                  sum,
                  na.rm = TRUE
                )
              )
        }
        
        return(data)
      })
      
      statSelection <- function(statistic, type){
        if(type == "skater"){
          data <- skaterSeason()
        } else {
          data <- goalieSeason()
        }
        
        if(input$careerOrSeason == 1){
          data <- 
            data %>% 
            select(
              Player.Name,
              one_of(statistic)
            ) 
        } else {
          data <- 
            data %>% 
            select(
              Player.Name,
              Team,
              Season,
              one_of(statistic)
            )  
        }
        
        data <- 
          data %>% 
          rename(
            Player = Player.Name
          ) %>% 
          arrange(
            desc(.data[[statistic]])
          ) 
        
        if(str_detect(statistic, pattern = "GoalsAgainstAverage")){
          data <-
            data %>% 
            arrange(
              .data[[statistic]]
            )
        }
        
        {
          shinyjs::enable(id = "playoffs")
          shinyjs::enable(id = "league")
          shinyjs::enable(id = "careerOrSeason")
          shinyjs::enable(id = "scopeTo")
          shinyjs::enable(id = "scopeFrom")
          shinyjs::hide("processing")
        }
        
        return(data)
      }
      
      ### Options
      rankingOptions <-
        list(
          scrollX = TRUE,
          ordering = FALSE, 
          paging = TRUE,
          pageLength = 10,
          dom = 'ftp'
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
      
      ### Changes the two buttons that switch from regular season to playoff and league data
      observeEvent(
        input$playoffs,
        {
          if(input$playoffs %% 2 != 0){
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "playoffs",
              label = "Regular Season?"
            )  
          } else {
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "playoffs",
              label = "Playoffs?"
            )
          }
            
        }      
      ) 
      
      observeEvent(
        input$league,
        {
          if(input$league %% 2 != 0){
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "league",
              label = "SHL Stats?"
            )  
          } else {
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "league",
              label = "SMJHL Stats?"
            )
          }
          
        }      
      )
      
      observeEvent(
        input$careerOrSeason,
        {
          if(input$league %% 2 != 0){
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "careerOrSeason",
              label = "Career Records?"
            )  
          } else {
            updateActionButton(
              session = getDefaultReactiveDomain(),
              inputId = "careerOrSeason",
              label = "Per Season Records?"
            )
          }
          
        }      
      ) 
      
      ##----------------------------------------------------------------
      ##                          Career Rank                          -
      ##----------------------------------------------------------------
      
      output$skaterStatOne <- DT::renderDT(
        {
          statSelection(input$skaterStatOne, type = "skater")
        },
        options = 
          rankingOptions,
        rownames = TRUE
      )
      
      output$skaterStatTwo <- DT::renderDT(
        {
          statSelection(input$skaterStatTwo, type = "skater")
        },
        options = 
          rankingOptions,
        rownames = FALSE
      )
      
      output$skaterStatThree <- DT::renderDT(
        {
          statSelection(input$skaterStatThree, type = "skater")
        },
        options = 
          rankingOptions,
        rownames = FALSE
      )
      
      output$goalieStatOne <- DT::renderDT(
        {
          statSelection(input$goalieStatOne, type = "goalie")
        },
        options = 
          rankingOptions,
        rownames = TRUE
      )
      
      output$goalieStatTwo <- DT::renderDT(
        {
          statSelection(input$goalieStatTwo, type = "goalie")
        },
        options = 
          rankingOptions,
        rownames = FALSE
      )
      
      output$goalieStatThree <- DT::renderDT(
        {
          statSelection(input$goalieStatThree, type = "goalie")
        },
        options = 
          rankingOptions,
        rownames = FALSE
      )
      
    }
  )
}

