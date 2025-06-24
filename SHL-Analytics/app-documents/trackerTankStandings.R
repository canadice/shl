tankStandingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("season"),
      label = "Select the season",
      choices = readAPI("https://portal.simulationhockey.com/api/v1/season")$season:55
    ),
    DTOutput(ns("tankStandings")) %>% 
      withSpinner()
  )
}

tankStandingsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### Functions ####
      
      schedule <- reactive({
        gamesLoader(leagueID = 0, season = input$season) %>% 
          filter(
            type == "Regular Season"
          ) %>% 
          mutate(
            date = as_date(date)
          ) %>% 
          arrange(date) %>% 
          filter(
            played == 1
          )
      })
      
      teamInfo <- reactive({
        readAPI("https://index.simulationhockey.com/api/v1/teams", query = list(league = 0, season = input$season)) %>% 
          select(
            conference, 
            !!if (input$season < 83) quo(division) else NULL,
            name,
            id
          )
      })
      
      standings <- reactive({
        schedule <- schedule()
        teamInfo <- teamInfo()
        
        eliminatedAt <- 
          list(
            standings = NA,
            elimination = 
              data.frame(
                team = unique(c(schedule$awayTeam, schedule$homeTeam)) %>% as.numeric(), 
                gp = NA, 
                points = NA, 
                ROW = NA,
                gDiff = NA, 
                date = NA, 
                stringsAsFactors = FALSE
              )
          )
        
        
        for(i in schedule$date %>% unique()){
          data <- schedule 
          startDate <- min(schedule$date)
          endDate <- i
          eliminationData <- eliminatedAt$elimination
           
          currentStandings <- 
            data %>% 
            filter(date <= endDate & date >= startDate) 
          
          if(nrow(currentStandings) > 0){
            # Creates current standings
            currentStandings <- 
              currentStandings %>%
              mutate(
                awayPoints = 
                  case_when(
                    awayScore > homeScore ~ 2,
                    awayScore < homeScore & (overtime == 1 | shootout == 1) ~ 1,
                    TRUE ~ 0),
                homePoints = 
                  case_when(
                    awayScore < homeScore ~ 2,
                    awayScore > homeScore & (overtime == 1 | shootout == 1) ~ 1,
                    TRUE ~ 0),
                awayROW =
                  if_else(awayPoints == 2 & !(shootout == 1), 1, 0),
                homeROW =
                  if_else(homePoints == 2 & !(shootout == 1), 1, 0)
              ) 
            
            
            away <- currentStandings %>%
              group_by(awayTeam) %>%
              summarise(
                points = sum(awayPoints), 
                gf = sum(awayScore, na.rm = TRUE), 
                ga = sum(homeScore, na.rm = TRUE), 
                gp = length(homeScore),
                ROW = sum(awayROW)
              ) %>%
              rename(team = awayTeam)
            
            home <- currentStandings %>%
              group_by(homeTeam) %>%
              summarise(
                points = sum(homePoints), 
                gf = sum(homeScore, na.rm = TRUE), 
                ga = sum(awayScore, na.rm = TRUE), 
                gp = length(homeScore),
                ROW = sum(homeROW)
              ) %>%
              rename(team = homeTeam)
            
            standings <- 
              rbind(
                away,
                home
              ) %>% 
              group_by(team) %>% 
              dplyr::summarize(
                across(
                  everything(),
                  ~ sum(.x, na.rm = TRUE)
                )
              ) %>% 
              ungroup() %>% 
              mutate(
                maxPotPts = points + (66 - gp)*2,
                maxROW = ROW + (66-gp),
                gDiff = gf - ga
              ) %>% 
              left_join(
                teamInfo,
                by = c("team" = "id")
              ) %>% 
              { if (input$season >= 83) group_by(., conference) else group_by(., conference, division) } %>% 
              mutate(
                divRank = frank(data.frame(-points, -ROW, -gDiff))
              ) %>%
              ungroup() %>%
              group_by(conference) %>%
              mutate(
                wcRank = if (input$season >= 83) NA_integer_ else case_when(divRank < 4 ~ 99, TRUE ~ frank(data.frame(-points, -ROW, -gDiff))) %>% rank(),
                playoffTeam = if (input$season >= 83) if_else(divRank <= 8, TRUE, FALSE) else if_else(divRank < 4 | wcRank < 3, TRUE, FALSE),
                minROW = ROW[points == min(points[playoffTeam]) & playoffTeam == TRUE] %>% unique() %>% sort() %>% .[1],
                eliminated = if_else(
                  (maxPotPts < min(points[playoffTeam])) |
                    (maxPotPts == min(points[playoffTeam]) & maxROW < minROW),
                  TRUE,
                  FALSE
                )
              ) %>%
              ungroup()
              
            eliminationData <- 
              data.frame(
                apply(
                  eliminationData, 
                  MARGIN = 1, 
                  FUN = function(x){
                    current <- which(standings$team %in% x["team"])
                    if(!is.na(x["date"])){
                      x
                    } else if(length(current) == 0){
                      c(x["team"], NA, NA, NA, NA, NA)
                    } else if(standings$eliminated[current]){
                      c(x["team"], standings$gp[current], standings$points[current], standings$ROW[current], standings$gDiff[current], endDate %>% as_date())
                    } else {
                      x
                    }
                  }) %>% 
                  t(),
                stringsAsFactors = FALSE
              )
            
            colnames(eliminationData) <- c("team", "gp" ,"points", "ROW", "gDiff", "date")
          } else {
            
          } 
          
          eliminatedAt <- list(standings = standings, elimination = eliminationData)
          
        }
        
        full_join(
          eliminatedAt$standings,
          eliminatedAt$elimination %>% 
            mutate(
              team = as.numeric(team)
            ),
          by = "team",
          suffix = c(" current", " elimination")
        ) %>% 
        dplyr::mutate(
          across(
            c(`gp elimination`:`gDiff elimination`),
            ~ as.numeric(.x)
          )
        ) %>% 
        select(
          name,
          conference,
          !!if (input$season < 83) quo(division) else NULL,
          GP = `gp current`,
          P = `points current`,
          gf,
          ga,
          GD = `gDiff current`,
          ROW = `ROW current`,
          `GP at Elimination` = `gp elimination`,
          `GD at Elimination` = `gDiff elimination`,
          `P at Elimination` = `points elimination`,
          `ROW at Elimination` = `ROW elimination`
        ) %>% 
        mutate(
          `P after Elimination` = P - `P at Elimination`,
          `ROW after Elimination` = ROW - `ROW at Elimination`,
          `GD after Elimination` = GD - `GD at Elimination`
        ) %>% 
        arrange(
          `P after Elimination` %>% desc(),
          `ROW after Elimination` %>% desc(),
          `GD after Elimination` %>% desc(),
          conference,
          if (input$season < 83) division else NULL,
          P %>% desc(),
          ROW %>% desc,
          GD %>% desc()
        )
        
      }) |> 
        bindEvent(input$season)
      
      output$tankStandings <- renderDT({
        standings() %>% 
          mutate(
            `GP after Elimination` = GP - `GP at Elimination`
          ) %>% 
          arrange(`P after Elimination` %>% desc(), `GP after Elimination`) %>% 
          select(
            name,
            contains("after")
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
          orderClasses = FALSE, 
          ordering = FALSE,
          ## Sets a scroller for the rows
          scrollY = '800px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = '600px',
          pageLength = 20,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'Brtip',
          bInfo = FALSE,
          buttons = list( 
            list(extend = 'csv',   filename =  paste("tankStandings", input$season, sep = "-")),
            list(extend = 'excel', filename =  paste("tankStandings", input$season, sep = "-"))),
          ## Defines which columns the table shows by default
          columnDefs = list(
            list(
              width = "200px",
              targets = 0
            # ),
            # list(
            #   visible = FALSE,
            #   targets = 1:2
            )
          )
        ),
      extensions = c('Buttons')
      )
    }
  )
}