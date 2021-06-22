
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

careerUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          5,
          fluidRow(
            selectizeInput(
              inputId = ns("skaterName"),
              label = "Select a skater",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            column(
              6,
              actionButton(
                inputId = ns("playoffs"),
                label = "Playoffs?",
                width = "100%"
              )  
            ),
            column(
              6,
              actionButton(
                inputId = ns("league"),
                label = "SMJHL Stats?",
                width = "100%"
              )  
            )
          )
        ),
        column(
          6,
          column(
            6, 
            selectInput(
              inputId = ns("scopeFrom"),
              label = "From Season",
              choices = 1:max(historySkaterSeason$Season),
              selected = 1
            )
          ),
          column(
            6,
            selectInput(
              inputId = ns("scopeTo"),
              label = "To Season",
              choices = 1:max(historySkaterSeason$Season),
              selected = max(historySkaterSeason$Season)
            )
          ),
          DTOutput(ns("careerStats"))
        ),
        column(
          1,
          h5("AWARDS", align = "center"),
          br(),
          br(),
          br()
        )
      ),
      fluidRow(
        column(
          5,
          # style = 'border: 1px solid black;',
          h5("5 Best Seasons", align = "center"),
          DTOutput(ns("bestSeasons")),
          h5("Misc Best Seasons", align = "center"),
          column(
            6,
            h6("Most Goals", align = "center"),
            DTOutput(ns("bestSeasonsGoals"))
          ),
          column(
            6,
            h6("Most Assists", align = "center"),
            DTOutput(ns("bestSeasonsAssists"))
          ),
          column(
            6,
            h6("Most Hits", align = "center"),
            DTOutput(ns("bestSeasonsHits"))
          ),
          column(
            6,
            h6("Most Shots Blocked", align = "center"),
            DTOutput(ns("bestSeasonsSB"))
          ),
          column(
            6,
            h6("Most +/-", align = "center"),
            DTOutput(ns("bestSeasonsPM"))
          ),column(
            6,
            h6("Most Shots", align = "center"),
            DTOutput(ns("bestSeasonsShots"))
          )
        ),
        column(
          7,
          # style = 'border: 1px solid black;',
          h5("Season By Season", align = "center"),
          plotlyOutput(ns("seasonBySeasonGraph")),
          br(),
          h5("Team Statistics", align = "center"),
          DTOutput(ns("teamStats"))
        )
      ),
      fluidRow(
        column(
          12,
          h3("Season by season breakdown", align = "center"),
          DTOutput(ns("seasonBySeason"))
        )
      ),
      br(),
      em("Disclaimer: Some data may be wrong."),
      em("Starting from S53 FHM is used to simulate the games which changes the statistics that can be exported."),
      em("S60 saw a change in the update scale to make lower TPE players more impactful."),
      br(),
      br()
    )
  )
}


## Backend for vizualizations
careerCardSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
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
      
      observe({
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          inputId = "skaterName",
          choices = 
            historySkaterSeason$Player.Name %>% 
            unique() %>% 
            sort(),
          selected = "Sarmad Khan",
          server = TRUE
        )  
      })    
      
      filteredData <- reactive({
        
        league <- if_else(input$league %% 2 != 0, 2, 1)
        
        historySkaterSeason %>% 
          filter(
            Player.Name == input$skaterName,
            LeagueId == league
          ) %>% 
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
      
      ##----------------------------------------------------------------
      ##                          Career Stats                         -
      ##----------------------------------------------------------------
      
      output$careerStats <- DT::renderDT(
        {
          filteredData() %>% 
            select(
              isPlayoffs,
              GamesPlayed,
              Goals,
              Assists,
              Points,
              Hits,
              ShotsBlocked
            ) %>% 
            rename(
              `Career Stats` = isPlayoffs, 
              GP = GamesPlayed,
              G = Goals,
              A = Assists,
              P = Points,
              SB = ShotsBlocked
            ) %>% 
            group_by(`Career Stats`) %>% 
            summarize(
              across(
                GP:SB,
                sum
              )
            ) %>% 
            mutate(
              `Career Stats` =
                case_when(
                  `Career Stats` == 0 ~ "Regular Season",
                  TRUE ~ "Playoffs"
                )
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      ##----------------------------------------------------------------
      ##                           Team Stats                          -
      ##----------------------------------------------------------------
      
      output$teamStats <- DT::renderDT(
        {
          filteredData() %>% 
            select(
              team,
              Season,
              GamesPlayed,
              Goals,
              Assists,
              Points
            ) %>% 
            rename(
              GP = GamesPlayed,
              G = Goals,
              A = Assists,
              P = Points
            ) %>% 
            group_by(team) %>% 
            summarize(
              `First Season` = min(Season),
              `Last Season` = max(Season),
              across(
                GP:P,
                sum
              )
            ) %>% 
            arrange(
              `First Season`
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      ##----------------------------------------------------------------
      ##                          Best Seasons                         -
      ##----------------------------------------------------------------
      output$bestSeasons <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              Goals,
              Assists,
              Points,
              PlusMinus#,
              # Hits,
              # ShotsBlocked
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              G = Goals,
              A = Assists,
              P = Points,
              `+/-` = PlusMinus#,
              # SB = ShotsBlocked
            ) %>%
            group_by(Season) %>% 
            summarize(
              across(
                G:`+/-`,
                sum
              )
            ) %>% 
            arrange(
              -P
            ) %>% 
            slice(
              1:5
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsGoals <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              Goals
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              G = Goals
            ) %>%
            arrange(
              -G
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsAssists <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              Assists,
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              A = Assists,
            ) %>%
            arrange(
              -A
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsHits <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              Hits,
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team 
            ) %>%
            arrange(
              -Hits
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsSB <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              ShotsBlocked
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              Shots = ShotsBlocked
            ) %>%
            arrange(
              -Shots
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsPM <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              PlusMinus
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              `+/-` = PlusMinus
            ) %>%
            arrange(
              -`+/-`
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$bestSeasonsShots <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              Shots
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team
            ) %>%
            arrange(
              -Shots
            ) %>% 
            slice(
              1:3
            )
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      
      ##----------------------------------------------------------------
      ##                    Season by season stats                     -
      ##----------------------------------------------------------------
      
      output$seasonBySeason <- DT::renderDT(
        {
          playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
          
          filteredData() %>% 
            select(
              team,
              isPlayoffs,
              Season,
              GamesPlayed,
              Goals,
              Assists,
              Points,
              PlusMinus,
              Hits,
              ShotsBlocked,
              PPPoints,
              GameWinningGoals,
              GameTyingGoals
            ) %>% 
            filter(
              isPlayoffs == playoffs
            ) %>%
            select(
              -isPlayoffs
            ) %>% 
            rename(
              Team = team, 
              GP = GamesPlayed,
              G = Goals,
              A = Assists,
              P = Points,
              `+/-` = PlusMinus,
              SB = ShotsBlocked,
              PPP = PPPoints,
              GWG = GameWinningGoals,
              GTG = GameTyingGoals
            ) %>% 
            arrange(
              Season
            ) 
        },
        options = 
          list(
            ordering = FALSE, 
            ## Removes pages in the table
            paging = FALSE,
            dom = 't'
          ),
        rownames = FALSE
      )
      
      output$seasonBySeasonGraph <- 
        renderPlotly(
          {
            playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
            
            if(filteredData() %>% nrow() == 0){
              return(NULL)
            }
            
            visData <- 
              filteredData() %>% 
              select(
                isPlayoffs,
                Season,
                Goals,
                Assists,
                Points
              ) %>% 
              filter(
                isPlayoffs == playoffs
              ) %>%
              select(
                -isPlayoffs
              ) %>% 
              rename(
                G = Goals,
                A = Assists,
                P = Points
              ) %>% 
              group_by(Season) %>% 
              summarize(
                across(
                  G:P,
                  sum
                )
              ) %>% 
              pivot_longer(
                -Season,
                names_to = "Stat",
                values_to = "Value"
              ) %>% 
              mutate(
                Stat = factor(Stat, levels = c("G", "A", "P"))
              )
            
            p <- 
              ggplot(visData) + 
              aes(x = Season, y = Value, color = Stat) + 
              geom_line(
                size = 0.75
                ) + 
              theme_bw() + 
              labs(x = "", y = "") %>% 
              scale_y_continuous(
                limits = c(0, 100)
              ) + 
              scale_x_continuous(
                breaks = 1:100
              ) + 
              scale_color_manual(
                name = NULL,
                values = c("#2c6185", "#fcab61", "black")
              ) +
              theme(
                panel.grid.minor.x = element_blank(),
                axis.title.y = element_blank()
              )
            
            p %>% 
              ggplotly(
                width = 500,
                height = 400,
                tooltip = "Value"
              ) %>%
              layout(
                legend = 
                  list(
                    orientation = "h",
                    y = 1.125,
                    x = 0.3
                  )
              ) %>% 
              config(
                modeBarButtonsToRemove =
                  c("pan2d", "zoomIn2d", "zoomOut2d", 
                    "autoScale2d", "resetScale2d", 
                    "hoverClosestCartesian",
                    "hoverCompareCartesian", "toggleSpikelines",
                    "select2d", "lasso2d", "zoom2d"
                  )
              ) 
          }
        )
      
      
    }
  )
}

