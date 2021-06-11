
###########################################################################
###########################################################################
###                                                                     ###
###              Looking at players per draft class                     ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playersUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of season of players
    # Option for coloring in visualization
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        ## Selection of league
        selectInput(
          inputId = ns("class"),
          label = "Select Draft Class",
          choices = unique(forumData$CLASS) %>% sort(decreasing = TRUE),
          selected = unique(forumData$CLASS) %>% max()
        ),
        em("Source: SHL Forums")
      ),
      ## Shows the results of the visualization and explanation
      mainPanel(
        DT::DTOutput(
          outputId = ns("dataTable")
        )
     )
   )
 )
}

## Backend module for player similarities
playersSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## Selects the current data from the selected season
      currentData <- reactive({
        
        forumData %>% 
          filter(
            CLASS == (input$class %>% as.character())
          ) %>% 
          select(
            NAME,
            POSITION,
            USER,
            Bank.Balance,
            TPE,
            Active,
            Posts,
            Online.For,
            abbr,
            primary,
            secondary,
            IIHF.Nation
          ) %>% 
          rename(
            team = abbr,
            Bank = Bank.Balance
          ) %>% 
          rename_with(
            .cols = c(-TPE, -IIHF.Nation),
            stringr::str_to_title
          ) %>% 
          return()
      })
      
      ## Outputs a datatable of all the players
      output$dataTable <- DT::renderDT({
        currentData() %>% 
          arrange(-TPE) %>% 
          mutate(
            Rank = row.names(.),
            Name = iconv(Name, to = "UTF-8"),
            User = iconv(User, to = "UTF-8")
          ) %>% 
          relocate(
            c(Primary, Secondary, Rank),
            .before = Name
          ) %>% 
          datatable(
            extensions = c('Buttons'),
            options = 
              list(
                orderClasses = TRUE, 
                ## Sets a scroller for the rows
                scrollX = FALSE,
                scrollY = '600px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                autoWidth = TRUE,
                columnDefs = 
                  list(
                    list(
                      targets = 3:5,
                      width = '10px' 
                    ),
                    list(
                      targets = 6,
                      width = '20px' 
                    ),
                    list(
                      targets = c(0:2),
                      visible = FALSE
                    )
                  ),
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # scrollX = '600px',
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                # bInfo = FALSE,
                buttons = c('copy', 'csv', 'excel')
              )
          ) %>% 
          formatStyle(
            columns = 0:13,
            valueColumns = "Primary",
            backgroundColor = 
              styleEqual(
                sort(unique(forumData$primary)), 
                sort(unique(forumData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:13,
            valueColumns = "Secondary",
            color = 
              styleEqual(
                sort(unique(forumData$secondary)), 
                sort(unique(forumData$secondary))
              )
          ) %>% 
          formatCurrency(
            "Bank",
            currency = "$",
            interval = 3,
            mark = " ",
            digits = 0
          )
      },
      class = 'compact cell-border stripe'
      ) 
    }
  )
}

