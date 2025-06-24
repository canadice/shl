
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

leagueScheduleUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        radioButtons(
          inputId = ns("league"),
          label = "Select the league",
          choices = c("SHL" = 0, "SMJHL" = 1)
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$b("Schedule Setup"),
        column(
          width = 12,
          
          dateInput(
            ns("startDate"),
            label = "In-game start of season",
            value = "2022-10-04",
            min = "2022-10-04",
            startview = "year"
          ),
          dateInput(
            ns("endDate"),
            label = "In-game end of season",
            value = "2023-03-31",
            startview = "year"
          ),
          numericInput(
            ns("intraConference"),
            label = "Number of INTRA conference games between teams (within)",
            value = 4,
            step = 2
          ),
          numericInput(
            ns("interConference"),
            label = "Number of INTER conference games between teams (between)",
            value = 6,
            step = 2
          ),
          actionButton(
            ns("createSchedule"),
            label = "Create the schedule"
          )
        )
      ),
      column(
        width = 8,
        shinyjs::hidden(
          div(
            id = ns("processing"),
            h4(
              "The schedule is being built, please hold...", 
              align = "center"),
            img(src="schedule.gif", align = "center",height='300px',width='400px'),
            align = "center"
          ),
          div(
            id = ns("scheduleResults"),
            downloadButton(ns("downloadData"), "Download")
          )
        )
      )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          verbatimTextOutput(
            ns("Results")
          )
        )
    )
  )
}


## Backend for vizualizations
leagueScheduleSERVER <- function(id){
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
      
      teams <- reactive({
        data <- readAPI("http://index.simulationhockey.com/api/v1/teams", query = list(league = input$league))
        
        if (input$league == 999) {
          data |> select(team = name, conference, division)
        } else {
          data |> select(team = name, conference)
        }
      })
      
      values <- reactiveValues(
        divisions = 0,
        conferences = 2,
        nGames = 66
      )
      
      observe({
        values$startDay <- input$startDate
        values$endDay <- input$endDate
        
        values$nDays <- lubridate::as_date(values$endDay) - lubridate::as_date(values$startDay)
        
        values$nWeeks <- (values$nDays / 7) %>% floor()
      }) |> 
        bindEvent(c(input$startDate, input$endDate))
      
      
      
      observe({
        values$teams <- teams() |> nrow()
        
        if(input$league == 999) {
          values$teamID <- teams() |> 
            arrange(conference, division) |> 
            mutate(ID = seq_len(n()))
        } else {
          values$teamID <- teams() |> 
            arrange(conference) |> 
            mutate(ID = seq_len(n()))
        }
        
        if(input$league != 999){
          values$divisions = 0
        }
        
      }) |> 
        bindEvent(input$league)
      
      observe({
        intraCon1 <- matrix(input$intraConference/2, nrow = (values$teamID$conference == 0) %>% sum(), ncol = (values$teamID$conference == 0) %>% sum())
        intraCon2 <- matrix(input$intraConference/2, nrow = (values$teamID$conference == 1) %>% sum(), ncol = (values$teamID$conference == 1) %>% sum())
        interCon <- matrix(input$interConference/2, nrow = (values$teamID$conference == 0) %>% sum(), ncol = (values$teamID$conference == 1) %>% sum())
        
        values$matchups <- bdiag(list(intraCon1, intraCon2))
        if((values$teamID$conference == 0) %>% sum() > 0 & (values$teamID$conference == 1) %>% sum() > 0){
          values$matchups[1:((values$teamID$conference == 0) %>% sum()), ((values$teamID$conference == 0) %>% sum()+1):((values$teamID$conference == 0) %>% sum()+(values$teamID$conference == 1) %>% sum())] <-
            values$matchups[((values$teamID$conference == 0) %>% sum()+1):((values$teamID$conference == 0) %>% sum()+(values$teamID$conference == 1) %>% sum()), 1:((values$teamID$conference == 0) %>% sum())] <-
            interCon
        }
        diag(values$matchups) <- 0
        
        values$games <- NA
        
        k <- 1
        for(i in 1:nrow(values$matchups)){
          for(j in 1:ncol(values$matchups)){
            if(values$matchups[i,j] == 0){
              #DO NOTHING
            } else {
              values$games[k:(k+values$matchups[i,j]-1)] <-
                paste(i, " @ ", j, " (",1:values$matchups[i,j], ")", sep = "")
              
              k <- k + values$matchups[i,j]
            }
          }
        }
        
      }) |> 
        bindEvent(c(input$intraConference, input$interConference, input$league))
      
      output$Results <- renderPrint({
        print(values %>% reactiveValuesToList())
      })
      
      observeEvent(input$createSchedule,{
        shinyjs::show("processing")
        shinyjs::hide("scheduleResults")
        shinyjs::disable("createSchedule")
        shinyjs::disable("startDate")
        shinyjs::disable("endDate")
        shinyjs::disable("intraConference")
        shinyjs::disable("interConference")
        
        # values$seed <- runif(1, min = 0, max = 1000) %>% floor()
        values$seed <- sample(c(309), size = 1)
        # values$seed <- 698
        
        set.seed(values$seed)
        
        print(values$seed)
        
        values$dataMatchups <- 
          values$matchups %>% as.matrix() %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(cols = -1) %>% 
          mutate(
            name = stringr::str_extract_all(name, pattern = "[0-9]+", simplify = TRUE) %>% c()
          ) %>% 
          filter(
            value != 0
          ) %>% 
          rename(
            away = rowname,
            home = name,
            meetings = value
          )
        
        values$rounds <- matrix(nrow = values$teams / values$conferences, ncol = sum(values$matchups)/values$teams * 2)
        
        sampleGames <- filteredSampleGames <- values$samplingGames
        
        progress <- shiny::Progress$new(max = ncol(values$rounds))
        on.exit(progress$close())
        
        progress$set(message = "Scheduling round:", value = 0)
        
        i <- 1
        while(i <= ncol(values$rounds)){
          
          j <- 1
          
          k <- 1
          
          progress$set(value = i, detail = i)
          while(j <= nrow(values$rounds)){
            
            if(all(values$rounds[,i] %>% is.na())){
              current <- 
                values$dataMatchups %>% 
                slice_sample(n = 1, weight_by = meetings)
              
              values$rounds[j,i] <- paste(current$away, current$home, sep = " @ ")
              
              tempDataMatchups <- 
                values$dataMatchups %>% 
                mutate(
                  meetings = if_else(away == current$away & home == current$home, meetings - 1, meetings)
                )
              
            } else {
              chosen <- values$rounds[,i] %>% str_split(pattern = " ", simplify = TRUE) %>% .[,c(1,3)] %>% c() %>% stringi::stri_remove_empty_na()
              
              filteredDataMatchups <- 
                tempDataMatchups %>% 
                filter(
                  !(away %in% chosen | home %in% chosen) 
                ) 
              
              if(all(filteredDataMatchups$meetings == 0) | (filteredDataMatchups %>% nrow() == 0)){
                values$rounds[j:1, i] <- NA
                
                j <- 0
                k <- k + 1
                print(paste(i, j, k))
                
                if(k > 200){
                  i <- 1000
                  j <- 1000
                  
                  modalDialog(
                    title = "The scheduling did not converge!",
                    "Please restart the scheduling again.",
                  ) %>%
                    showModal()
                }
              } else {
                current <- 
                  filteredDataMatchups %>% 
                  slice_sample(n = 1, weight_by = meetings)
                
                values$rounds[j,i] <- paste(current$away, current$home, sep = " @ ")
                
                tempDataMatchups <- 
                  tempDataMatchups %>% 
                  mutate(
                    meetings = if_else(away == current$away & home == current$home, meetings - 1, meetings)
                  )
              }
            }
            j <- j + 1
          }
          
          ## Overwrites the sampling source when an entire round is finished.
          values$dataMatchups <- 
            tempDataMatchups
          i <- i + 1
        }
        
        if(any(values$rounds %>% is.na())){
          # DO NOTHING
        } else {
          
          values$schedule <- 
            values$rounds %>% 
            as.data.frame() %>% 
            rownames_to_column() %>% 
            pivot_longer(-1) %>% 
            transmute(
              round = str_extract_all(name, pattern = "[0-9]+", simplify = TRUE) %>% as.numeric(),
              away = str_split(value, pattern = " @ ", simplify = TRUE)[,1] %>% as.numeric(),
              home = str_split(value, pattern = " @ ", simplify = TRUE)[,2] %>% as.numeric()
            ) %>%
            arrange(round) %>% 
            left_join(
              values$teamID %>% 
                select(
                  team, ID
                ),
              by = c("away" = "ID")
            ) %>% 
            left_join(
              values$teamID %>% 
                select(
                  team, ID
                ),
              by = c("home" = "ID"),
              suffix = c(" away", " home")
            ) %>% 
            select(
              round,
              `team away`,
              `team home`
            ) %>% 
            mutate(
              date = values$startDay + lubridate::days((round-1)*((values$nDays/values$nGames) %>% floor()))
            ) %>% 
            mutate(
              date = 
                case_when(
                  round < 9 ~ date,
                  round < 17 ~ date - lubridate::days(((values$nDays/values$nGames) %>% round(0))-1),
                  round < 25 ~ date - lubridate::days(2*(((values$nDays/values$nGames) %>% round(0))-1)),
                  round < 33 ~ date - lubridate::days(4*(((values$nDays/values$nGames) %>% round(0))-1)),
                  round < 41 ~ date - lubridate::days(6*(((values$nDays/values$nGames) %>% round(0))-1)),
                  round < 49 ~ date - lubridate::days(8*(((values$nDays/values$nGames) %>% round(0))-1)),
                  round < 57 ~ date - lubridate::days(10*(((values$nDays/values$nGames) %>% round(0))-1)),
                  round < 65 ~ date - lubridate::days(12*(((values$nDays/values$nGames) %>% round(0))-1)),
                  TRUE ~ date - lubridate::days(14*(((values$nDays/values$nGames) %>% round(0))-1))
                )
            )
          
          scheduledMatchups <- values$schedule %>% group_by(`team away`, `team home`) %>% summarize(n = n())
          correctMatchups <- 
            values$matchups %>% 
            as.matrix() %>% 
            as.data.frame() %>% 
            rownames_to_column() %>% 
            pivot_longer(cols = -1) %>% 
            filter(value != 0) %>% 
            mutate(
              name = stringr::str_extract_all(name, pattern = "[0-9]+", simplify = TRUE) %>% as.numeric(),
              rowname = rowname %>% as.numeric()
            ) %>% 
            left_join(
              values$teamID %>% 
                select(
                  team, ID
                ),
              by = c("rowname" = "ID")
            ) %>% 
            left_join(
              values$teamID %>% 
                select(
                  team, ID
                ),
              by = c("name" = "ID"),
              suffix = c(" away", " home")
            ) %>% 
            arrange(
              `team away`,
              `team home`
            )
          
          values$checkMatchups <- 
            scheduledMatchups %>% 
            left_join(
              correctMatchups %>% 
                select(
                  `team away`,
                  `team home`,
                  value
                ),
              by = c("team away", "team home")
            ) %>% 
            filter(
              n != value
            )
          
          
          values$schedule <-
            values$schedule %>%
            mutate(
              date =
                case_when(
                  lubridate::month(date) == 12 & lubridate::day(date) == 24 ~ date - lubridate::days(1),
                  date > values$endDay ~ date - lubridate::days(1),
                  TRUE ~ date
                ),
              YEAR = lubridate::year(date),
              MONTH = lubridate::month(date),
              DAY = lubridate::day(date),
              TYPE = if_else(date >= values$startDay, "Regular", "PreSeason")
            ) %>%
            rename(
              `TEAM HOME` = `team home`,
              `TEAM AWAY` = `team away`
            ) %>% 
            # arrange(
            #   date
            # ) %>% 
            select(
              YEAR, MONTH, DAY, `TEAM HOME`, `TEAM AWAY`, TYPE
            ) 
          
          shinyjs::show("scheduleResults")
          
        }
        
        shinyjs::hide("processing")
        shinyjs::enable("createSchedule")
        shinyjs::enable("startDate")
        shinyjs::enable("endDate")
        shinyjs::enable("intraConference")
        shinyjs::enable("interConference")
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("schedule_", input$league,"_", min(values$schedule$YEAR), ".csv")
        },
        content = function(file) {
          write.csv(values$schedule, file, row.names = FALSE, quote = FALSE)
        }
      )
      
      # 
      # games[round %>% as.numeric()] %>% matrix(nrow = 7)
      # 
      # games[sampleGames %>% as.numeric()]
      # 
      # checkGames[sampleGames %>% as.numeric(),] %>% table()
      # 
      # 
      # print(seed)
      # 
      
      
      
      
    }
  )
}
