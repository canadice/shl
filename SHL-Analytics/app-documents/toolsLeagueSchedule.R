
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
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    fluidRow(
      column(
        tags$b("Division Setup"),
        width = 12,
        bucket_list(
          header = "Drag the teams to the desired divisions.",
          group_name = ns("bucket_list_group"),
          orientation = "horizontal",
          add_rank_list(
            text = "Division 1",
            labels = NULL,
            input_id = ns("division1")
          ),
          add_rank_list(
            text = "Division 2",
            labels = NULL,
            input_id = ns("division2")
          ),
          add_rank_list(
            text = "Division 3",
            labels = NULL,
            input_id = ns("division3")
          ),
          add_rank_list(
            text = "Division 4",
            labels = NULL,
            input_id = ns("division4")
          ),
          add_rank_list(
            text = "Drag from here",
            labels = 
              teamData %>% 
              filter(league == "SMJHL", !is.na(fhmID)) %>% 
              select(
                team
              ) %>% 
              c() %>% 
              nth(1),
            input_id = ns("Teams")
          )
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
      
      values <- reactiveValues(
        divisions = 4,
        conferences = 2,
        nGames = 66
      )
      
      observeEvent(input$startDate,{
        values$startDay <- input$startDate
        
        values$nDays <- lubridate::as_date(values$endDay) - lubridate::as_date(values$startDay)
        
        values$nWeeks <- (values$nDays / 7) %>% floor()
      })
      observeEvent(input$endDate,{
        values$endDay <- input$endDate
        
        values$nDays <- lubridate::as_date(values$endDay) - lubridate::as_date(values$startDay)
        
        values$nWeeks <- (values$nDays / 7) %>% floor()
      })
      
      observeEvent({
        input$Teams
        input$division1
        input$division2
        input$division3
        input$division4
      }, {
        values$teams <- c(input$Teams, input$division1, input$division2,input$division3,input$division4) %>% length()
        
        # values$division1 <- input$division1
        # values$division2 <- input$division2
        # values$division3 <- input$division3
        # values$division4 <- input$division4
        
        values$teamID <- 
          data.frame(
            team = 
              c(
                input$division1,
                input$division2,
                input$division3,
                input$division4
              )
          ) %>% 
          {
            if(nrow(.)>0){
              dplyr::mutate(
                .,
                ID = 1:n(),
                division = 
                  rep(
                    1:4, 
                    times = 
                      c(
                        length(input$division1),
                        length(input$division2),
                        length(input$division3),
                        length(input$division4)
                      )
                  ),
                conference = 
                  rep(
                    1:2, 
                    times = 
                      c(
                        c(input$division1, input$division2) %>% length(),
                        c(input$division3, input$division4) %>% length()
                      )
                  )
              )
            } else {
              .
            }
          }
      })
      
      observeEvent(
        {
          input$intraConference
          input$interConference
          input$division1
          input$division2
          input$division3
          input$division4
        },
        {
          intraCon1 <- matrix(input$intraConference/2, nrow = (values$teamID$conference == 1) %>% sum(), ncol = (values$teamID$conference == 1) %>% sum())
          intraCon2 <- matrix(input$intraConference/2, nrow = (values$teamID$conference == 2) %>% sum(), ncol = (values$teamID$conference == 2) %>% sum())
          interCon <- matrix(input$interConference/2, nrow = (values$teamID$conference == 1) %>% sum(), ncol = (values$teamID$conference == 2) %>% sum())

          values$matchups <- bdiag(list(intraCon1, intraCon2))
          if((values$teamID$conference == 1) %>% sum() > 0 & (values$teamID$conference == 2) %>% sum() > 0){
            values$matchups[1:((values$teamID$conference == 1) %>% sum()), ((values$teamID$conference == 1) %>% sum()+1):((values$teamID$conference == 1) %>% sum()+(values$teamID$conference == 2) %>% sum())] <-
              values$matchups[((values$teamID$conference == 1) %>% sum()+1):((values$teamID$conference == 1) %>% sum()+(values$teamID$conference == 2) %>% sum()), 1:((values$teamID$conference == 1) %>% sum())] <-
              interCon
          }
          diag(values$matchups) <- 0

          if(length(input$Teams) == 0){
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

          }
        },
        ignoreInit = TRUE)
      
      output$Results <- renderPrint({
        print(values %>% reactiveValuesToList())
      })

      observeEvent(input$createSchedule,{
        if(ncol(values$matchups) != 14){
          modalDialog(
            title = "Something is wrong",
            "Please move one team to and from their division.",
          ) %>% 
            showModal()
        } else {
          shinyjs::show("processing")
          shinyjs::disable("createSchedule")
          shinyjs::disable("startDate")
          shinyjs::disable("endDate")
          shinyjs::disable("intraConference")
          shinyjs::disable("interConference")
          
          values$seed <- runif(1, min = 0, max = 1000) %>% floor()
          # values$seed <- sample(c(245, 795, 192, 309), size = 1)
          # values$seed <- 698
          
          set.seed(values$seed)
          
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
                date = values$startDay + lubridate::days((round-1)*((values$nDays/values$nGames) %>% round(0)))
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
            
            progress$set(value = i, detail = "Pre-season")
            
            ### CREATING PRE-SEASON SCHEDULE
            pre_season <-
              data.frame(
                round = rep(-6, 7),
                `team away` = c(input$division1, input$division2),
                `team home` = c(input$division3, input$division4)
              ) %>% 
              add_row(
                data.frame(
                  round = rep(-5, 7),
                  `team away` = c(input$division2, input$division3),
                  `team home` = c(input$division1, input$division4)  
                )
              ) %>% 
              add_row(
                data.frame(
                  round = rep(-4, 7),
                  `team away` = c(input$division3, input$division4),
                  `team home` = c(input$division2, input$division1)  
                )
              ) %>% 
              mutate(
                date = rep(c(values$startDay - 24, values$startDay - 21, values$startDay - 18), each = 7)
              ) %>% 
              add_row(
                data.frame(
                  round = rep(-3, 7),
                  `team away` = c(input$division4, input$division3),
                  `team home` = c(input$division2, input$division1)
                ) %>% 
                  add_row(
                    data.frame(
                      round = rep(-2, 7),
                      `team away` = c(input$division3, input$division2),
                      `team home` = c(input$division4, input$division1)  
                    )
                  ) %>% 
                  add_row(
                    data.frame(
                      round = rep(-1, 7),
                      `team away` = c(input$division1, input$division4),
                      `team home` = c(input$division2, input$division3)  
                    )
                  ) %>% 
                  mutate(
                    date = c(values$startDay - 15, values$startDay - 12, values$startDay - 9) %>% rep(each = 7)
                  ) 
              ) %>% 
              add_row(
                data.frame(
                  round = rep(0, 7),
                  `team away` = c(input$division1 %>% sample(), input$division4 %>% sample()),
                  `team home` = c(input$division2 %>% sample(), input$division3 %>% sample()) 
                ) %>% 
                  mutate(
                    date = rep(values$startDay - 6, 7)
                  )
              ) %>% 
              rename(
                `team away` = team.away,
                `team home` = team.home
              )
            
            values$schedule <-
              values$schedule %>%
              add_row(
                pre_season
              ) %>% 
              mutate(
                date =
                  case_when(
                    lubridate::month(date) == 12 & lubridate::day(date) == 24 ~ date - lubridate::days(1),
                    date > values$endDay ~ date - lubridate::days(1),
                    TRUE ~ date
                  ),
                YEAR = lubridate::year(date),
                MONTH = lubridate::month(date),
                DATE = lubridate::day(date),
                TYPE = if_else(date >= values$startDay, "Reg", "Pre")
              ) %>%
              arrange(
                date
              ) %>% 
              select(
                -round
              )
            
            shinyjs::show("scheduleResults")
            
          }
          
          shinyjs::hide("processing")
          shinyjs::enable("createSchedule")
          shinyjs::enable("startDate")
          shinyjs::enable("endDate")
          shinyjs::enable("intraConference")
          shinyjs::enable("interConference")
        }
        
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("scheduleSMJHL.csv", sep = "")
        },
        content = function(file) {
          write.csv(values$schedule, file, row.names = FALSE)
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

