
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
    fluidRow(
      
      ##################################################################
      ##                      Skater information                      ##
      ##################################################################
      
      column(
        width = 4,
        box(
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Filters",
          selectizeInput(
            inputId = ns("skaterName"),
            label = "Select a skater",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = NULL
          ),
          actionButton(
            inputId = ns("playoffs"),
            label = "Playoffs?",
            width = "100%"
          ),
          br(),
          br(),
          actionButton(
            inputId = ns("league"),
            label = "SMJHL Stats?",
            width = "100%"
          ),
          br(),
          br(),
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns("scopeFrom"),
                label = "From season",
                width = "100%",
                value = 1,
                min = 1,
                max = max(historySkaterSeason$Season)
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = ns("scopeTo"),
                label = "To season",
                width = "100%",
                value = max(historySkaterSeason$Season),
                min = 1,
                max = max(historySkaterSeason$Season)
              )
            )
          )
        ),
        uiOutput(ns("draftBox"))
      ),
      
      column(
        width = 8,
        tabBox(
          width = NULL,
          
          ##################################################################
          ##                        Career Summary                        ##
          ##################################################################
          
          tabPanel(
            title = "Career",
            h3("Career Summary", align = "center"),
            DTOutput(ns("careerStats")),
            h3("Team Summary", align = "center"),
            DTOutput(ns("teamStats"))  
          ), 
          ##################################################################
          ##                         Best Seasons                         ##
          ##################################################################
          
          tabPanel(
            title = "Best Seasons",
            DTOutput(ns("bestSeasons")),
            br(),
            column(
              width = 6,
              selectInput(
                inputId = ns("bestStatOne"),
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
              DTOutput(ns("bestSeasonsOne"))
            ),
            column(
              width = 6,
              selectInput(
                inputId = ns("bestStatTwo"),
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
                selected = "Assists",
                multiple = FALSE
              ),
              DTOutput(ns("bestSeasonsTwo"))
            )
          ),
          
          ##################################################################
          ##                    Trophies and Awards                       ##
          ##################################################################
          
          tabPanel(
            title = "Trophies & Awards",
            uiOutput(ns("trophyBox")),
            uiOutput(ns("allstarBox")),
            uiOutput(ns("awardBox"))
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
        title = "Season by Season",
        DTOutput(ns("seasonBySeason")),
        br(),
        plotlyOutput(ns("seasonBySeasonGraph"))
      ),
      br(),
      em("Disclaimer: Some data may be wrong as historical data from the first seasons are hard to dig up."),
      em("Starting from S53 FHM is used to simulate the games which changes the statistics that can be exported."),
      em("S60 saw a change in the update scale to make lower TPE players more impactful."),
      br(),
      br()
    ) %>% 
      fluidRow()
  )
}


## Backend for vizualizations
careerCardSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
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
      
      ### Finds the list of names from the data set, and the calculations are done on the server side
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
      
      ### Finds draft information of selected skater
      filteredDraftData <- reactive({
        draftData %>% 
          filter(
            Player == input$skaterName
          )
      })
      
      ### Finds achievement information of selected skater
      filteredAchievementData <- reactive({
        achievementData %>% 
          filter(
            Player == input$skaterName
          )
      })
      
      ### Find and filters the data based on settings. 
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
          # ) %>% 
          # mutate(
          #   MinutesPlayed = MinutesPlayed / GamesPlayed
          # )
      })
      
      ##---------------------------------------------------------------
      ##                  Draft and Achievement boxes                 -
      ##---------------------------------------------------------------

      output$draftBox <- renderUI({
        if(filteredDraftData() %>% nrow() == 0){
          infoBox(
            title = tags$b(""),
            color = "orange",
            width = NULL,
            icon = tags$i(class = "fas fa-vote-yea", style="font-size: 36px; color: white"),
            fill = TRUE,
            value = 
              tags$p("Draft information is missing for this skater.",
                style = "font-size: 75%;"
              )
          )
        } else {
          draftPosition <- filteredDraftData()$Drafted %>% head(1)
          
          if(draftPosition == 1000){
            draftPosition <- "as a GM"
          } else {
            draftPosition <- paste("number", draftPosition, sep = " ")
          }
          
          infoBox(
            title = 
              tags$b(
                paste(
                  "Selected", draftPosition)
                ),
            color = "orange",
            width = NULL,
            icon = tags$i(class = "fas fa-vote-yea", style="font-size: 36px; color: white"),
            fill = TRUE,
            value = 
              tags$p(
                paste(
                  "in season", filteredDraftData()$Season %>% unlist(), 
                  "by", filteredDraftData()$Team %>% unlist(),
                  collapse = " "),
                style = "font-size: 75%;"
              )
          )
        }
      })
      
      output$allstarBox <- renderUI({
        
        firstTeam <- sum(filteredAchievementData()$X1AS, na.rm = TRUE)
        
        secondTeam <- sum(filteredAchievementData()$X2AS, na.rm = TRUE)
        
        thirdTeam <- sum(filteredAchievementData()$X3AS, na.rm = TRUE)
        
        rookieTeam <- sum(filteredAchievementData()$RAS, na.rm = TRUE)
        
        allstarTeam <- sum(filteredAchievementData()$AS, na.rm = TRUE)
        
        if(any(firstTeam > 0, secondTeam > 0, thirdTeam > 0)){
          valueBox(
            subtitle = tags$b("All-Star Teams"),
            color = "orange",
            width = NULL,
            icon = icon("star"),
            value = 
              tags$p(
                paste(
                  "First Team All-Star:", firstTeam, br(),
                  "Second Team All-Star:", secondTeam, br(),
                  "Third Team All-Star:", thirdTeam, br(),
                  "Rookie All-Star:", rookieTeam, 
                  collapse = " ") %>% 
                  HTML(),
                style = "font-size: 50%;"
              )
          )
        } else {
          valueBox(
            subtitle = tags$b("All-Star Teams"),
            color = "orange",
            width = NULL,
            icon = icon("star"),
            value = 
              tags$p(
                paste(
                  "All-Star Team:", allstarTeam, br(),
                  "Rookie All-Star:", rookieTeam, 
                  collapse = " ") %>% 
                  HTML(),
                style = "font-size: 50%;"
              )
          )  
        }
      })
      
      output$awardBox <- renderUI({
        
        wins <- 
          filteredAchievementData() %>% 
          select(
            Bojo.Biscuit:Sarmad.Khan
          ) %>% 
          mutate(
            across(
              .fn = sum,
              na.rm = TRUE
            )
          ) %>% 
          unique()
        
        if (nrow(wins) > 0) {
          wins <- 
            wins %>% 
            select(
              where(
                function(x) x > 0
              )
            )
        }
        
        nominations <- 
          sum(
            filteredAchievementData() %>% 
              select(
                Bojo.Biscuit:Sarmad.Khan
              ) == 0, 
            na.rm = TRUE)
        
        if(any(wins > 0, nominations > 0)){
          valueBox(
            subtitle = tags$b("League Awards"),
            color = "orange",
            width = NULL,
            icon = icon("award"),
            value = 
              tags$p(
                paste(
                  paste0(
                    paste(
                      colnames(wins) %>% 
                        str_replace(pattern = "\\.", replacement = " "),
                      wins,
                      sep = ": "
                    ),
                  collapse = "<br/>"
                ),
                paste("Nominations: ", nominations, sep = ""),
                sep = "<br/>") %>%
                HTML(),
                style = "font-size: 50%;"
              )
          )
        }
      })
      
      output$trophyBox <- renderUI({
        
        championships <- 
          filteredAchievementData() %>% 
          select(
            Season,
            CC
          ) %>% 
          filter(
            CC == 1
          )
        
        if(nrow(championships) > 0){
          valueBox(
            subtitle = tags$b("Challenge Cups"),
            color = "orange",
            width = NULL,
            icon = icon("trophy"),
            value = 
              tags$p(
                paste0(
                  championships$Season %>% 
                    str_replace_all(pattern = "S", replacement = "Season "),
                  collapse = "<br/>"
                ) %>% 
                  HTML(),
                style = "font-size: 75%;"
              )
          )  
        }
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
              PlusMinus,
              Hits,
              ShotsBlocked
            ) %>% 
            rename(
              `Career Stats` = isPlayoffs, 
              GP = GamesPlayed,
              G = Goals,
              A = Assists,
              P = Points,
              `+/-` = PlusMinus,
              Hit = Hits,
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
        options = careerOptionList,
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
              Points,
              PlusMinus,
              Hits,
              ShotsBlocked
            ) %>% 
            rename(
              Team = team,
              GP = GamesPlayed,
              G = Goals,
              A = Assists,
              P = Points,
              `+/-` = PlusMinus,
              Hit = Hits,
              SB = ShotsBlocked
            ) %>% 
            group_by(Team) %>% 
            summarize(
              `First Season` = min(Season),
              `Last Season` = max(Season),
              across(
                GP:SB,
                sum
              )
            ) %>% 
            arrange(
              `First Season`
            )
        },
        options = careerOptionList,
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
              PlusMinus,
              Hits,
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
              G = Goals,
              A = Assists,
              P = Points,
              `+/-` = PlusMinus,
              Hit = Hits,
              SB = ShotsBlocked
            ) %>%
            group_by(Season) %>% 
            summarize(
              Team = paste(Team, collapse = " & "),
              across(
                G:SB,
                sum
              )
            ) %>% 
            relocate(
              Team,
              .before = Season
            ) %>% 
            arrange(
              -P
            ) %>% 
            slice(
              1:5
            )
        },
        options = careerOptionList,
        rownames = FALSE
      )
      
      statSelection <- function(statistic){
        playoffs <- (input$playoffs %% 2 != 0) %>% as.numeric()
        
        data <- 
          filteredData() %>% 
          select(
            team,
            isPlayoffs,
            Season,
            one_of(statistic)
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
            desc(.data[[statistic]])
          ) %>% 
          slice(
            1:3
          )
        
        if(str_detect(statistic, pattern = "MinutesPlayed")){
          data <- 
            data %>% 
            mutate(
              ## Using the format with glue grammar that allows for dynamic variable names
              "{statistic}" := 
                format(
                  as.POSIXct(
                    .data[[statistic]], 
                    origin = "1970-01-01"
                  ), 
                  "%M:%S"
                )
            )
        }
        
        return(data)
      }
      
      output$bestSeasonsOne <- DT::renderDT(
        {
          statSelection(input$bestStatOne)
        },
        options = 
          seasonRankOptionList,
        rownames = FALSE
      )
      
      output$bestSeasonsTwo <- DT::renderDT(
        {
          statSelection(input$bestStatTwo)
        },
        options = 
          seasonRankOptionList,
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
              PPPoints,
              PlusMinus,
              MinutesPlayed,
              Hits,
              ShotsBlocked,
              Shots,
              PenaltyMinutes
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
              Hit = Hits,
              `+/-` = PlusMinus,
              TOI = MinutesPlayed,
              SB = ShotsBlocked,
              PPP = PPPoints,
              Sh = Shots,
              PIM = PenaltyMinutes
            ) %>% 
            arrange(
              Season
            ) %>% 
            mutate(
              TOI := 
                format(
                  as.POSIXct(
                    TOI, 
                    origin = "1970-01-01"
                  ), 
                  "%M:%S"
                )
            )
        },
        options = 
          careerOptionList,
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
                displayModeBar = FALSE
              ) 
          }
        )
      
      
    }
  )
}

