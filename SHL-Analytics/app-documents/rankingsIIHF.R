
############################################################################
############################################################################
###                                                                      ###
###                        IIHF RANKING CALCULATOR                       ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

rankingIIHFUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        offset = 1,
        width = 10,
        p("Each tournament every IIHF nation gathers points towards their IIHF ranking",
          "that is calculated using a weighted sum of the previous four tournaments.",
          "During the round robin, every point a nation obtains is worth 2.5 ranking points,",
          "during the medal rounds (play-offs), a win in the quarter and semi-final are worth 9 ranking points,",
          "and finally a gold medal win gives the nation 12 ranking points."),
        p("The sum of each nation's ranking points is then multiplied by 100 to produce",
          "the final points of the tournament. The IIHF ranking is then calculated by weighting",
          "each of the four previous tournament points in descending weights of 1, 0.5, 0.3, and 0.2."),
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          title = "IIHF Rankings",
          DTOutput(outputId = ns("dataTable")) %>% withSpinner()
        ),
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          title = "Ranking History",
          tagList(
            a(
              "Link to Historical Ranking Data", 
              href = "https://docs.google.com/spreadsheets/d/1V_S72NZHtO5hxuy1nLOtAqdRmA3zz7ckTPWzUVR2REA/edit#gid=0"
            )
          ),
          plotlyOutput(
            outputId = ns("rankingHistory")
          )
        )
      )
      
    )
  )
}


## Backend for vizualizations
rankingIIHFSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      ##---------------------------------------------------------------
      ##      Disable buttons when clicked and data is processing     -
      ##---------------------------------------------------------------
      
      # observeEvent(
      #   ## Add all buttons or settings that can be selected
      #   {
      #     input$
      #   },
      #   {
      #     shinyjs::disable(id = "")
      #   },
      #   ignoreInit = FALSE
      # )
      
      
      ##------------------------------------------------------------------------
      ##  Initialize reactiveValues that will be used as interactive options   -
      ##------------------------------------------------------------------------
      
      chosenRow <- reactiveVal(NULL)
      
      ##---------------------------------------------------------------
      ##  Loads and visualizes historical ranking data for the IIHF   -
      ##---------------------------------------------------------------

      output$rankingHistory <- renderPlotly({
        visData <- 
          historyIIHF %>% 
          pivot_longer(
            cols = -Season,
            names_to = "Federation",
            values_to = "Ranking"
          ) %>% 
          mutate(
            Season = 
              str_extract_all(
                Season, 
                pattern = "[0-9]+",
                simplify = TRUE
              ) %>% 
              as.numeric()
          )
        
        p <- 
          ggplot(data = visData) + 
          aes(
            x = Season, 
            y = Ranking, 
            group = Federation, 
            color = Federation
          ) + 
          geom_line() + 
          theme_bw() + 
          labs(x = "Season", y = "Total\nPoints") +
          theme(
            axis.title.y = 
              element_text(
                angle = 0,
                vjust = 0.5
              )
          ) +
          scale_color_manual(
            "Federation",
            values = 
              teamInfo %>% 
              filter(
                league == "IIHF",
                team != "Unassigned"
              ) %>% 
              mutate(
                primary = 
                  ifelse(team == "Japan", secondary, primary)
              ) %>% 
              select(
                primary
              ) %>% 
              unlist() %>% 
              unname()
          )
        
        ggplotly(
          p,
          tooltip = c("y", "x", "group")
          ) %>% 
          plotly::config(
            modeBarButtonsToRemove =
              c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                "resetScale2d", "hoverClosestCartesian",
                "hoverCompareCartesian", "toggleSpikelines"
              )
          ) %>%
          layout(
            autosize = TRUE
          )
      })

      
      ##---------------------------------------------------------------
      ##            Loads past seasons data and standings             -
      ##---------------------------------------------------------------
      
      currentSeason <- 
        playerLoader(0)$players %>% 
        select(
          season
        ) %>% 
        unique() %>% 
        unlist()
      
      pointsCalculator <- function(season){
        games <- 
          gamesLoader(2, season = season, type = "Regular Season") %>% 
          left_join(
            teamLoader(2, season = season),
            by = c("homeTeam" = "id")
          ) %>% 
          left_join(
            teamLoader(2, season = season),
            by = c("awayTeam" = "id"),
            suffix = c(".home", ".away")
          ) %>% 
          mutate(
            points.home = 
              case_when(
                homeScore > awayScore & overtime == 0 ~ 3,
                homeScore > awayScore & overtime == 1 ~ 2,
                homeScore < awayScore & overtime == 1 ~ 1,
                TRUE ~ 0
              ),
            points.away = 
              case_when(
                homeScore < awayScore & overtime == 0 ~ 3,
                homeScore < awayScore & overtime == 1 ~ 2,
                homeScore > awayScore & overtime == 1 ~ 1,
                TRUE ~ 0
              )
          )
        
        roundRobin <- 
          games %>% 
          # filter(
          #   type == "Regular Season"
          # ) %>% 
          select(
            date,
            name.home,
            name.away,
            points.home,
            points.away
          ) %>% 
          pivot_longer(
            cols = -date,
            names_to = c(".value", "group"),
            names_sep = "\\."
          ) %>% 
          group_by(name, points) %>% 
          summarize(
            n = n()
          ) %>% 
          pivot_wider(
            id_cols = name,
            names_from = points,
            values_from = n 
          ) %>% 
          mutate(
            across(everything(),~ replace_na(.x, 0))
          ) %>% 
          relocate(
            "2",
            .before = "3"
          ) %>% 
          rename(
            Team = "name",
            Losses = "0",
            `OTL/SOL` = "1",
            `OTW/SOW` = "2",
            Wins = "3"
          ) %>% 
          mutate(
            `IIHF RR Points` = 2.5*(Wins*3+`OTW/SOW`*2+`OTL/SOL`*1)
          )
        
        medalRound <- 
          gamesLoader(2, season = season, type = "playoffs") %>% 
          lapply(
            X = .,
            FUN = function(x){
              do.call(what = data.frame, args = x)
            }
          ) %>% 
          do.call(what = rbind.fill, args = .) %>% 
          select(
            team1.id,
            wins.team1 = team1.wins,
            nickname.team1 = team1.nickname,
            wins.team2 = team2.wins,
            nickname.team2 = team2.nickname
          ) %>% 
          pivot_longer(
            cols = -team1.id,
            names_to = c(".value", "group"),
            names_sep = "\\."
          ) %>% 
          group_by(nickname, wins) %>% 
          summarize(
            n = n()
          ) %>% 
          pivot_wider(
            id_cols = nickname,
            names_from = wins,
            values_from = n
          ) %>% 
          mutate(
            across(everything(),~ replace_na(.x, 0)),
            nickname = paste("Team", nickname)
          ) %>% 
          rename(
            Team = nickname,
            `Medal Round Losses` = "0",
            `Medal Round Wins` = "1"
          ) %>% 
          mutate(
            Gold = case_when(
              `Medal Round Wins` == 3 ~ 1,
              TRUE ~ 0
            ),
            Silver = case_when(
              `Medal Round Wins` == 2 ~ 1,
              TRUE ~ 0
            ),
            `IIHF Medal Points` = min(`Medal Round Wins` * 9, 18) + 12*Gold
          )
        
        standings <- 
          roundRobin %>% 
          left_join(
            medalRound,
            by = c("Team")
          ) %>% 
          mutate(
            across(everything(),~ replace_na(.x, 0)),
            `IIHF Points` = (`IIHF RR Points`+`IIHF Medal Points`)*100
          ) %>% 
          ungroup()
        
        return(standings)
      }
      
      standings <- 
        1:5 %>% 
        lapply(
          X = .,
          FUN = function(x){
            pointsCalculator(currentSeason-x)
          })
      
      seasonStandingsCurrent <- standings[[1]]
      
      seasonStandings1stLast <- standings[[2]]
      seasonStandings2ndLast <- standings[[3]]
      seasonStandings3rdLast <- standings[[4]]
      seasonStandings4thLast <- standings[[5]]
      
      output$dataTable <- DT::renderDT({
        rankingData <- 
          seasonStandingsCurrent %>% 
          left_join(
            seasonStandings1stLast %>% 
              select(
                Team,
                `IIHF Points`
              ),
            by = "Team",
            suffix = c(" Current", " Last")
          ) %>% 
          left_join(
            seasonStandings2ndLast %>% 
              select(
                Team,
                `IIHF Points`
              ),
            by = "Team"
          ) %>% 
          left_join(
            seasonStandings3rdLast %>% 
              select(
                Team,
                `IIHF Points`
              ),
            by = "Team",
            suffix = c(" 2 season", " 3 season")
          ) %>% 
          left_join(
            seasonStandings4thLast %>% 
              select(
                Team,
                `IIHF Points`
              ),
            by = "Team"
          ) %>% 
          rename(
            `IIHF Points 4 season` = `IIHF Points`
          ) %>% 
          rowwise() %>% 
          mutate(
            `Final Points Current` = 
              sum(
                `IIHF Points Current`*1,
                `IIHF Points Last`*0.5,
                `IIHF Points 2 season`*0.3,
                `IIHF Points 3 season`*0.2
              ),
            `Final Points Last` = 
              sum(
                `IIHF Points Last`*1,
                `IIHF Points 2 season`*0.5,
                `IIHF Points 3 season`*0.3,
                `IIHF Points 4 season`*0.2
              )
          ) %>% 
          ungroup() %>% 
          mutate(
            `IIHF Rank Current` = rank(-`Final Points Current`),
            `IIHF Rank Last` = rank(-`Final Points Last`),
            Group = 
              case_when(
                `IIHF Rank Current` %in% c(1, 4, 5, 8, 9, 12, 13) ~ "A",
                TRUE ~ "B"
              )
          ) %>% 
          select(
            Team,
            contains("IIHF Points"),
            contains("Final Points"),
            contains("IIHF Rank"),
            Group
          ) %>% 
          left_join(
            teamInfo %>% 
              filter(
                league == "IIHF"
              ) %>% 
              select(
                team,
                primary,
                secondary
              ) %>% 
              mutate(
                team = paste("Team", team)
              ),
            by = c("Team" = "team")
          ) %>% 
          mutate(
            `Change` = `IIHF Rank Last` - `IIHF Rank Current`
          ) %>% 
          relocate(
            c(Group, `IIHF Rank Current`, Change),
            .after = Team
          ) %>% 
          arrange(`IIHF Rank Current`)
        
        colnames(rankingData) <- 
          c(
            "Team",
            "Group",
            paste("IIHF Rank S", currentSeason - 1, sep = ""),
            "Change",
            paste("IIHF Points S", currentSeason - 1, sep = ""),
            paste("IIHF Points S", currentSeason - 2, sep = ""),
            paste("IIHF Points S", currentSeason - 3, sep = ""),
            paste("IIHF Points S", currentSeason - 4, sep = ""),
            paste("IIHF Points S", currentSeason - 5, sep = ""),
            paste("Final Points S", currentSeason - 1, sep = ""),
            paste("Final Points S", currentSeason - 2, sep = ""),
            paste("IIHF Rank S", currentSeason - 2, sep = ""),
            "primary",
            "secondary"
          )
        
        rankingData %>% 
          datatable(
            escape = FALSE, 
            rownames = FALSE,
            extensions = c('Buttons', 'Scroller','RowGroup'),
            fillContainer = TRUE,
            options = 
              list(
                ordering = TRUE, 
                columnDefs = 
                  list(
                    list(
                      targets = c(12:13),
                      visible = FALSE
                    )
                  ),
                ## Sets a scroller for the rows
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                # autoWidth = TRUE,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Brftop',
                # bInfo = FALSE,
                buttons = list(
                  'copy', 
                  list(
                    extend = "collection",
                    buttons = list(
                      list(extend = 'csv', filename = "ranking"),
                      list(extend = 'excel', filename = "ranking")
                    ),
                    text = "Download"
                  )
                )
              )
          ) %>% 
          formatStyle(
            columns = 0:12,
            valueColumns = "primary",
            backgroundColor = 
              styleEqual(
                sort(unique(rankingData$primary)), 
                sort(unique(rankingData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:12,
            valueColumns = "secondary",
            color = 
              styleEqual(
                sort(unique(rankingData$secondary)), 
                sort(unique(rankingData$secondary))
              )
          ) 
      })
        
    }
  )
}

