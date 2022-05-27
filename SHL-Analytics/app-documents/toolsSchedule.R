
##################################################################
##                 Scheduling Tool for Sim Team                 ##
##                                                              ##
##             Created by Luke, Adapted by Canadice             ##
##################################################################

### UI module for player similarities using MDS
scheduleUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 4,
          fileInput(
            inputId = ns("fhmSchedule"),
            label = "Upload the exported schedule from FHM as a csv.",
            accept = ".csv",
            buttonLabel = "Select a file",
            placeholder = "No file uploaded"
          ),
          actionButton(
            inputId = ns("generate"),
            label = "Generate the schedule",
            width = '100%'
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = ns("seasonLength"),
            label = "How many sim days are in the season?",
            value = 25, 
            min = 0,
            max = 40
          ),
          dateInput(
            inputId = ns("seasonStart"),
            label = "When does the season start?",
            value = today(),
            format = "yyyy-mm-dd",
            daysofweekdisabled = 0
          )
          
        ),
        column(
          width = 2,
          radioButtons(
            inputId = ns("leagueID"),
            label = "Select the league to schedule.",
            choices = 
              c(
                "SHL" = 0,
                "SMJHL" = 1,
                "IIHF" = 2,
                "WJC" = 3
              ),
            selected = 0
          )
        ),
        column(
          width = 2,
          radioButtons(
            inputId = ns("gameType"),
            label = "Select the type of game to schedule.",
            choices = 
              c(
                "Pre-Season",
                "Regular Season",
                "Post-Season"
              ),
            selected = "Regular Season"
          )
        )
      ),
      h3("Processed schedule"),
      column(
        width = 8,
        verbatimTextOutput(
          outputId = ns("scheduleTable")
        )
      ),
      column(
        width = 4,
        DTOutput(
          outputId = ns("numberGamesPerSim")
        )
      )
    )
  )
}

## Backend module for player similarities
scheduleSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      schedule <- 
        reactive({
          input$generate
          
          file <- input$fhmSchedule %>% isolate()
          
          if(file %>% is.null()){
            "No schedule has been uploaded." %>% 
              as_tibble() %>% 
              rename(matchup = value) %>% 
              return()
          } else{
            
            startDate <- input$seasonStart %>% isolate() %>% as_date()
            seasonLength <- input$seasonLength %>% isolate()
            gameType <- input$gameType %>% isolate()
            leagueID <- input$leagueID %>% isolate() %>% as.numeric()
            
            calendar <- 
              (startDate:(startDate + days(seasonLength-1))) %>% 
              as_date() %>% 
              as_tibble() %>% 
              rename(
                simDate = value
              ) %>% 
              mutate(
                simDay = 1:(n()),
                sundays = (simDate %>% wday(week_start = 1) == 7) %>% as.numeric(),
                nrSundays = cumsum(sundays),
                simDateNoSun = simDate + days(nrSundays)
              ) %>% 
              select(simDay, simDateNoSun) %>% 
              rename(simDate = simDateNoSun)
            
            temp <- 
              read.csv(
                file$datapath,
                sep = ";"
              ) %>% 
              filter(
                Type == (gameType)
              ) 
            
            if(nrow(temp) == 0){
              "The uploaded schedule does not contain any games of this type." %>% 
                as_tibble() %>% 
                rename(matchup = value) %>% 
                return()
            } else {
              gamesPerDay <- nrow(temp) / seasonLength
              
              temp <- 
                temp %>% 
                mutate(
                  League.Id = leagueID,
                  Date = as.Date(Date, format = "%Y-%m-%d"),
                  simDay = 0,
                  gameNumber = 1:nrow(.)
                ) %>% 
                group_by(Date) %>% 
                mutate(
                  numberGames = n(),
                  gameDay = cur_group_id(),
                  gameDayNumber = 1:n()
                ) %>% 
                ungroup() %>% 
                mutate(
                  newGameDay = if_else(gameDayNumber == 1, 1, 0)
                ) 
              
              games <- 
                temp %>% 
                select(gameDay, numberGames, simDay) %>% 
                unique() %>% 
                mutate(
                  totalGames = numberGames
                )
              
              gameDiff <- 0
              over <- FALSE
              for(i in 2:nrow(games)){
                if(over){
                  ## If over threshold total games summation resets
                  games$totalGames[i] <- games$numberGames[i]
                  
                  ## Updates the game differential
                  gameDiff <- gameDiff + games$totalGames[i-1] - gamesPerDay
                  
                  over <- FALSE
                } else {
                  ## If not over threshold then sum previous with current
                  games$totalGames[i] <- games$totalGames[i-1] + games$numberGames[i]
                  
                  ## Checks if current total games is over threshold                  
                  over <- games$totalGames[i] > (gamesPerDay - gameDiff)
                }
              }
                
              
              games <- 
                games %>% 
                mutate(
                  simDay = 
                    if_else(totalGames < lag(totalGames), simDay + 1, simDay),
                  simDay = 
                    if_else(is.na(simDay), 0, simDay) %>% cumsum() + 1
                )
              
              temp <- 
                temp %>% 
                select(-numberGames, -simDay) %>% 
                left_join(
                  games %>% 
                    select(-totalGames),
                  by = c("gameDay")
                )
              
              temp <- 
                temp %>% 
                left_join(
                  teamInfo %>% 
                    select(leagueID, fhmID, abbr),
                  by = c("League.Id" = "leagueID", "Home" = "fhmID")
                ) %>% 
                left_join(
                  teamInfo %>% 
                    select(leagueID, fhmID, abbr),
                  by = c("League.Id" = "leagueID", "Away" = "fhmID"),
                  suffix = c(" Home", " Away")
                ) %>% 
                left_join(
                  calendar,
                  by = "simDay"
                ) %>% 
                select(
                  simDate,
                  gameDay,
                  `abbr Home`,
                  `abbr Away`
                ) %>% 
                group_by(
                  simDate, gameDay
                ) %>% 
                mutate(
                  matchup = paste("\n", paste(`abbr Away`, "@", `abbr Home`, collapse = "\n"), "\n", sep = "")
                ) %>% 
                group_by(simDate) %>% 
                mutate(
                  matchup = paste("\n", simDate, paste(matchup %>% unique(), collapse = ""), sep = "")
                ) %>% 
                return()
            }
          }
        })
      
      
      output$scheduleTable <- renderText({
        schedule()$matchup %>% unique()
      })
      
      output$numberGamesPerSim <- renderDT({
        
        if((schedule() %>% nrow())==1){
          NULL
        } else {
          schedule() %>% 
            group_by(simDate) %>% 
            summarize(
              nGames = n()
            ) %>% 
            ungroup()
        }
      },
      options = 
        list(
          ordering = FALSE, 
          scrollX = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          dom = 't'
        )
      )
      
      
    }
  )
}
