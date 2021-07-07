
###########################################################################
###########################################################################
###                                                                     ###
###              Looking at players per draft class                     ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
iihfUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectizeInput(
            inputId = ns("iihfNation"),
            label = "Select a IIHF Nation",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = NULL
          ),
          br(),
          br(),
          DT::DTOutput(
            outputId = ns("aggData")
          )
        ),
        column(
          width = 9,
          tags$head(tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}"))),
          
          DT::DTOutput(
            outputId = ns("dataTable")
          )
        )
      )
    )
 )
}

## Backend module for player similarities
iihfSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      observe({
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          inputId = "iihfNation",
          choices = forumData$IIHF.Nation %>% 
            unique() %>% 
            sort(),
          selected = "Unassigned",
          server = TRUE
        )  
      })
      
      
      ## Selects the current data from the selected season
      currentData <- reactive({
        
        forumData %>% 
          select(
            NAME,
            CLASS,
            LINK,
            POSITION,
            USER,
            USERLINK,
            TPE,
            Active,
            abbr,
            league,
            IIHF.Nation,
            Original,
            Transfer.Season
          ) %>%
          filter(
            IIHF.Nation == (input$iihfNation)
          ) %>%
          rename(
            team = abbr
          ) %>%
          rename_with(
            .cols =
              c(
                -TPE,
                -IIHF.Nation,
                -Original,
                -Transfer.Season
              ),
            stringr::str_to_title
          ) %>%
          arrange(Class) %>%
          mutate(
            Name = iconv(Name, to = "UTF-8"),
            User = iconv(User, to = "UTF-8")
          ) %>%
          mutate(
            Namesort = Name,
            Usersort = User,
            Name = paste0("<a href='",Link,"' target='_blank'>",Name,"</a>"),
            User = paste0("<a href='",Userlink,"' target='_blank'>",User,"</a>")
          ) %>%
          select(
            -Link,
            -Userlink
          ) %>%
          return()
      })
      
      ## Outputs a datatable of all the players
      output$dataTable <- DT::renderDT({
        currentData()
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '600px',
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
          buttons = c('copy', 'csv', 'excel'),
          ## Defines which columns the table shows by default
          columnDefs = 
            list(
              list(
                targets = 1,
                orderData = 13,
                width = '100px'
              ),
              list(
                targets = 4,
                orderData = 14,
                width = '70px'
              ),
              list(
                targets = c(11:12),
                visible = FALSE
              )
            )
        ),
      extensions = c('Buttons')
      )
      
      
      output$aggData <- DT::renderDT({
        currentData() %>% 
          mutate(
            Position =
              case_when(
                Position %>% str_detect(pattern = "Defense") ~ "Defense",
                Position %>% str_detect(pattern = "Goalie") ~ "Goalie",
                TRUE ~ "Forward"
              ) %>% 
              factor(
                levels = 
                  c(
                    "Forward",
                    "Defense",
                    "Goalie"
                  )
              )
          ) %>% 
          group_by(Active, Position, League) %>% 
          summarize(
            Amount = n()
          ) %>% 
          pivot_wider(
            names_from = League,
            names_sep = " ",
            values_from = Amount
          )
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '600px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = '600px',
          # pageLength = 10,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 't'
        )
      )
    }
  )
}

