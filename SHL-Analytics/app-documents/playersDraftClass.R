
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
          choices = unique(forumData$CLASS) %>% sort(),
          selected = unique(forumData$CLASS) %>% min()
        ),
        em("Source: SHL Forums")
      ),
      ## Shows the results of the visualization and explanation
      mainPanel(
        DTOutput(
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
            TPE,
            Active,
            abbr,
            primary,
            secondary,
            IIHF.Nation
          ) %>% 
          rename_with(
            stringr::str_to_title()
          ) %>% 
          return()
      })
      
      ## Outputs a datatable of all the players
      output$dataTable <- DT::renderDT({
        currentData() %>% 
          arrange(TPE)
      },
      filter = 'bottom',
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '400px',
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
          buttons = c('copy', 'csv', 'excel')
        ),
      extensions = c('Buttons')
      )
    }
  )
}

