
###########################################################################
###########################################################################
###                                                                     ###
###              Looking at players per draft class                     ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
draftClassUI <- function(id){
  
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
          choices = c(
            "ALL",
            unique(forumData$CLASS) %>% sort(decreasing = TRUE)),
          selected = unique(forumData$CLASS) %>% max(na.rm = TRUE)
        ),
        em("Source: SHL Forums, scraped daily."),
        br(),
        em("At high resolutions note that the headers sometimes are left aligned while the table is centered. Resize the screen manually and they will sync again.")
      ),
      ## Shows the results of the visualization and explanation
      mainPanel(
        tags$head(tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}"))),
        
        tabBox(
          width = NULL,
          selected = "All",
          tabPanel(
            "All",
            DT::DTOutput(
              outputId = ns("dataTableAll")
            )
          ),
          tabPanel(
            "Goalies",
            DT::DTOutput(
              outputId = ns("dataTableG")
            )
          ),
          tabPanel(
            "Defensemen",
            DT::DTOutput(
              outputId = ns("dataTableD")
            )
          ),
          tabPanel(
            "Forwards",
            DT::DTOutput(
              outputId = ns("dataTableF")
            )
          )
        )
     )
   )
 )
}

## Backend module for player similarities
draftClassSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## Selects the current data from the selected season
      currentData <- reactive({
        
        if(input$class != "ALL"){
          data <- 
            forumData %>% 
            filter(
              CLASS == (input$class %>% as.character())
            )  
        } else {
          data <- 
            forumData
        }
        
        data %>% 
          select(
            NAME,
            LINK,
            POSITION,
            USER,
            USERLINK,
            TPE,
            `BANK BALANCE`,
            ACTIVE,
            `ONLINE FOR`,
            ABBR = abbr,
            PRIMARY = primary,
            SECONDARY = secondary,
            `IIHF NATION`
          ) %>% 
          mutate(
            PRIMARY = if_else(is.na(PRIMARY), "#ffffff", PRIMARY),
            SECONDARY = if_else(is.na(SECONDARY), "#000000", SECONDARY)
          ) %>% 
          rename(
            TEAM = ABBR,
            BANK = `BANK BALANCE`,
            `IIHF Federation` = `IIHF NATION`
          ) %>% 
          rename_with(
            .cols = c(-TPE, -`IIHF Federation`),
            stringr::str_to_title
          ) %>% 
          arrange(-TPE) %>% 
          mutate(
            Rank = row.names(.),
            OnlineNum = `Online For`,
            `Online For` = 
              `Online For` %>% 
              seconds_to_period() %>% 
              round(digits = 0) %>% 
              as.character()
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
          relocate(
            c(Primary, Secondary, Rank),
            .before = Name
          ) %>% 
          mutate(
            PositionGroup =
              case_when(
                Position %>% str_detect(pattern = "Defense") ~ "D",
                Position %>% str_detect(pattern = "Goalie") ~ "G",
                TRUE ~ "F"
              ) %>% 
              factor(
                levels = 
                  c(
                    "F",
                    "D",
                    "G"
                  )
              )
          ) %>% 
          return()
      })
      
      ## js function for automatic reranking
      js <- c(
        "table.on('draw.dt', function(){",
        "  var PageInfo = table.page.info();",
        "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
        "    cell.innerHTML = i + 1 + PageInfo.start;",
        "  });",
        "})")
      
      columnDefinitions <- 
        list(
          ## Sets ordering variable for Online 
          list(
            targets = 10,
            orderData = 13
          ),
          list(
            targets = 4,
            orderData = 14,
            width = '100px'
          ),
          list(
            targets = 6,
            orderData = 15,
            width = '70px'
          ),
          list(
            targets = 7,
            width = '80px' 
          ),
          list(
            targets = c(1:3, 13:15), #The js object updates ranking based on the selected column, otherwise 0:2 to show TPE RANk only
            visible = FALSE
          )
        )
      
      
      ## Outputs a datatable of all the players
      output$dataTableAll <- DT::renderDT({
        currentData() %>% 
          select(-PositionGroup) %>% 
          datatable(
            escape = FALSE, 
            callback = JS(js),
            extensions = c('Buttons', 'Scroller'),
            fillContainer = TRUE,
            options = 
              list(
                orderClasses = TRUE, 
                ## Sets a scroller for the rows
                scrollX = '800px',
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                autoWidth = TRUE,
                columnDefs = columnDefinitions,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                # bInfo = FALSE,
                buttons = c('copy', 'csv', 'excel')
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
            valueColumns = "Primary",
            backgroundColor = 
              styleEqual(
                sort(unique(forumData$primary)), 
                sort(unique(forumData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
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
        
      }
      )
      
      output$dataTableG <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "G") %>% 
          select(-PositionGroup) %>% 
          datatable(
            escape = FALSE, 
            callback = JS(js),
            extensions = c('Buttons', 'Scroller'),
            fillContainer = TRUE,
            options = 
              list(
                orderClasses = TRUE, 
                ## Sets a scroller for the rows
                scrollX = '800px',
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                autoWidth = TRUE,
                columnDefs = columnDefinitions,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                # bInfo = FALSE,
                buttons = c('copy', 'csv', 'excel')
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
            valueColumns = "Primary",
            backgroundColor = 
              styleEqual(
                sort(unique(forumData$primary)), 
                sort(unique(forumData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
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
        
        
      }
      ) 
      
      output$dataTableD <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "D") %>% 
          select(-PositionGroup) %>% 
          datatable(
            escape = FALSE, 
            callback = JS(js),
            extensions = c('Buttons', 'Scroller'),
            fillContainer = TRUE,
            options = 
              list(
                orderClasses = TRUE, 
                ## Sets a scroller for the rows
                scrollX = '800px',
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                autoWidth = TRUE,
                columnDefs = columnDefinitions,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                # bInfo = FALSE,
                buttons = c('copy', 'csv', 'excel')
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
            valueColumns = "Primary",
            backgroundColor = 
              styleEqual(
                sort(unique(forumData$primary)), 
                sort(unique(forumData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
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
        
        
      }
      ) 
      
      output$dataTableF <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "F") %>% 
          select(-PositionGroup) %>% 
          datatable(
            escape = FALSE, 
            callback = JS(js),
            extensions = c('Buttons', 'Scroller'),
            fillContainer = TRUE,
            options = 
              list(
                orderClasses = TRUE, 
                ## Sets a scroller for the rows
                scrollX = '800px',
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                autoWidth = TRUE,
                columnDefs = columnDefinitions,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                # bInfo = FALSE,
                buttons = c('copy', 'csv', 'excel')
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
            valueColumns = "Primary",
            backgroundColor = 
              styleEqual(
                sort(unique(forumData$primary)), 
                sort(unique(forumData$primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:14,
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
        
        
      }
      ) 
    }
  )
}

