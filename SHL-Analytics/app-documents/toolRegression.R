
#################################################################################
#################################################################################
###                                                                           ###
###  REGRESSION TOOL THAT SHOWS PLAYERS UP FOR REGRESSION AND THEIR REQUIRED  ###
###                                  TPE LOSS                                 ###
###                                                                           ###
#################################################################################
#################################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

regressionUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        offset = 1,
        width = 10,
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          title = "Current Regression",
          DTOutput(outputId = ns("dataTable"))
        )
      )

    )
  )
}


## Backend for vizualizations
regressionSERVER <- function(id){
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
      ##          Selecting the data and relevant variables           -
      ##---------------------------------------------------------------
      
      currentSeason <- 
        playerLoader(0)$players %>% 
        select(
          season
        ) %>% 
        unique() %>% 
        unlist()
      
      user <- 
        forumData %>% 
        group_by(USER) %>% 
        summarize(
          n = n()
        )
      
      regressionData <- 
        forumData %>% 
        select(
          NAME,
          LINK,
          USER,
          USERLINK,
          CLASS,
          TPE,
          team,
          primary,
          secondary
        ) %>% 
        arrange(
          team
        ) %>% 
        left_join(
          user,
          by = "USER"
        ) %>% 
        mutate(
          RETIRED = if_else(n > 1, "Yes", "No"),
          NAME = paste0("<a href='",LINK,"' target='_blank'>",NAME,"</a>"),
          USER = paste0("<a href='",USERLINK,"' target='_blank'>",USER,"</a>"),
          AGE = currentSeason+2 - (str_extract(CLASS, pattern = "[0-9]+") %>% as.numeric())
        ) %>% 
        select(
          -LINK,
          -USERLINK
        ) %>% 
        filter(
          # Changed in S63 to start one year earlier
          # AGE > 10
          AGE > 9
        ) %>% 
        mutate(
          REGRESSIONGROUP = 
            case_when(
              # Scale prior to S62
              # AGE < 15 ~ 0.10,
              # AGE < 18 ~ 0.15,
              # AGE < 20 ~ 0.18,
              # AGE < 21 ~ 0.20,
              # TRUE ~ 0.25
              
              # The new scale after changes in S63 (implemented S65)
              AGE < 11 ~ 0.09,
              AGE < 12 ~ 0.12,
              AGE < 15 ~ 0.15,
              AGE < 16 ~ 0.18,
              AGE < 17 ~ 0.20,
              AGE < 19 ~ 0.22,
              AGE < 23 ~ 0.25,
              TRUE ~ 0.3
              
              # Special scale for S63              
              # AGE < 11 ~ 0.07,
              # AGE < 12 ~ 0.17,
              # AGE < 15 ~ 0.18,
              # AGE < 16 ~ 0.21,
              # AGE < 17 ~ 0.22,
              # AGE < 19 ~ 0.24,
              # AGE < 23 ~ 0.25,
              # TRUE ~ 0.3
              
              # # Special scale for S64              
              # AGE < 11 ~ 0.09,
              # AGE < 12 ~ 0.14,
              # AGE < 15 ~ 0.17,
              # AGE < 16 ~ 0.20,
              # AGE < 17 ~ 0.20,
              # AGE < 19 ~ 0.22,
              # AGE < 23 ~ 0.25,
              # TRUE ~ 0.3
            ),
          LOSTTPE = (TPE * REGRESSIONGROUP) %>% ceiling(),
          REMAININGTPE = TPE - LOSTTPE
        )
      
      output$dataTable <- DT::renderDT({
        regressionData %>% 
          select(
            Player = NAME,
            User = USER,
            Class = CLASS,
            `Regression Group` = REGRESSIONGROUP,
            Team = team,
            `Current Claimed TPE` = TPE,
            # `TPE Lost` = LOSTTPE,
            # `Remaining TPE` = REMAININGTPE,
            Retired = RETIRED,
            primary,
            secondary
          ) %>% 
          datatable(
            escape = FALSE, 
            rownames = FALSE,
            extensions = c('Buttons', 'Scroller','RowGroup'),
            fillContainer = TRUE,
            options = 
              list(
                rowGroup = list(dataSrc = 4),
                columnDefs = 
                  list(
                    list(
                      targets = c(4,7:8),
                      visible = FALSE
                    )
                  ),
                ordering = FALSE, 
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
                      list(extend = 'csv', filename = "regression"),
                      list(extend = 'excel', filename = "regression")
                    ),
                    text = "Download"
                  )
                )
              )
          ) %>% 
          formatPercentage(
            "Regression Group",
            interval = 3,
            mark = " ",
            digits = 0
          ) %>% 
          formatStyle(
            columns = 0:9,
            valueColumns = "primary",
            backgroundColor = 
              styleEqual(
                sort(unique(regressionData$primary)), 
                sort(unique(regressionData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:9,
            valueColumns = "secondary",
            color = 
              styleEqual(
                sort(unique(regressionData$secondary)), 
                sort(unique(regressionData$secondary))
              )
          ) 
      })
      
      
    }
  )
}

