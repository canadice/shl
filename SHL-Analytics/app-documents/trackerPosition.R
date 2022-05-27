
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
posTrackerUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          offset = 1,
          radioButtons(
            inputId = ns("activity"),
            label = "Filter on activity",
            choices = 
              c(
                "Active" = "Active",
                "Inactives" = "IA",
                "All" = "All"
              ),
            selected = "Active",
            width = "100%"
          )
        ),
        column(
          width = 6,
          DT::DTOutput(
            outputId = ns("leagueAggregate")
          ),
          em("Orange indicates that more than 80 percent of the slots have been filled.")
        )
      ),
      fluidRow(
        hr(),
        br(),
        p("The following histograms shows the distribution of TPE for all positions",
          "separated by the two leagues. Hovering over each bar, you will see the",
          "number of players from each class in the TPE interval."),
        column(
          width = 6,
          box(
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = NULL,
            title = "SHL",
            h5("Goalies", align = "center"),
            plotlyOutput(
              outputId = ns("goalieSHL")
            ),
            hr(),
            h5("Defense", align = "center"),
            plotlyOutput(
              outputId = ns("defenseSHL")
            ),
            hr(),
            h5("Forward", align = "center"),
            plotlyOutput(
              outputId = ns("forwardSHL")
            )
          )
        ),
        column(
          width = 6,
          box(
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = NULL,
            title = "SMJHL",
            h5("Goalies", align = "center"),
            plotlyOutput(
              outputId = ns("goalieSMJHL")
            ),
            hr(),
            h5("Defense", align = "center"),
            plotlyOutput(
              outputId = ns("defenseSMJHL")
            ),
            hr(),
            h5("Forward", align = "center"),
            plotlyOutput(
              outputId = ns("forwardSMJHL")
            )
          )
        )
      )
    )
 )
}

## Backend module for player similarities
posTrackerSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      observeEvent(
        input$activity,
        {
          shinyjs::disable(id = "activity")
        },
        ignoreInit = FALSE
      )
      
      
      ## Creates function to build histograms
      buildHist <- function(data){
        if(max(data$binCount) > 40){
          stepLength <- 4
        } else {
          stepLength <- 2
        }
        
        data <- 
          data %>%
          ungroup() %>% 
          select(bin, binCount, text) %>% 
          unique()
        
        p <- 
          ggplot(data) + 
          aes(x = bin, y = binCount, text = text) +
          geom_col(
            color = "black",
            fill = "#e08b46",
            width = 1
          ) + 
          labs(y = "Count", x = "TPE") + 
          theme_minimal() + 
          scale_y_continuous(
            expand = expansion(add = c(0, 1)),
            breaks = seq(0, 100, stepLength)
          ) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray60")
          )
        
        p %>% 
          ggplotly(tooltip = "text") %>% 
          layout(hoverlabel=list(bgcolor="white")) %>% 
          config(
            displayModeBar = FALSE
          ) %>% 
          return()
    }
      
      
      ## Selects the current data based on the selected filtering
      currentData <- reactive({
        
        temp <- 
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
          ) %>%
          rename(
            Team = abbr
          ) %>%
          rename_with(
            .cols =
              c(
                -TPE
              ),
            stringr::str_to_title
          ) %>%
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
          ) 
        
        if(input$activity != "All"){
          temp <- 
            temp %>% 
            filter(
              Active == input$activity
            )  
        }
        
        temp <- 
          temp %>% 
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
              ),
            bin = 
              cut(
                TPE, 
                breaks = c(100,200,seq(300,2100,200), 2500),
                labels = 
                  c("155\n-\n200",
                    "201\n-\n300",
                    "301\n-\n500",
                    "501\n-\n700",
                    "701\n-\n900",
                    "901\n-\n1100",
                    "1101\n-\n1300",
                    "1301\n-\n1500",
                    "1501\n-\n1700",
                    "1701\n-\n1900",
                    "1901\n-\n2100",
                    "2101+"))
          ) %>%
          group_by(League, Position, bin, Class) %>% 
          mutate(
            classCount = n()
          ) 
        
        
        classText <- 
          temp %>% 
          group_by(League, Position, bin) %>% 
          summarize(
            binCount = n(),
            text = paste(Class, classCount, sep = ": ") %>% unique() %>% sort() %>% paste(collapse = "\n")
          )
        
        temp <- 
          temp %>% 
          left_join(
            classText,
            by = c("League", "bin", "Position")
          ) %>% 
          return()
      })
      
      output$leagueAggregate <- DT::renderDT({
        nSHL <- (teamLoader(leagueID = 0) %>% nrow())
        nSMJHL <- (teamLoader(leagueID = 1) %>% nrow())
        
        temp <- 
          currentData() %>% 
          group_by(Class, Position) %>% 
          mutate(
            classAmount = n()
          ) %>% 
          group_by(League, Position) %>% 
          mutate(
            leagueAmount = n()
          ) %>% 
          select(Position, League, leagueAmount) %>% 
          unique() %>% 
          pivot_wider(
            names_from = League,
            values_from = leagueAmount
          ) %>% 
          mutate(
            slotsSHL =
              case_when(
                Position == "Forward" ~ 9 * nSHL,
                Position == "Defense" ~ 6 * nSHL,
                TRUE ~ 2 * nSHL,
              ),
            slotsSMJHL = 
              case_when(
                Position == "Forward" ~ 12 * nSMJHL,
                Position == "Defense" ~ 8 * nSMJHL,
                TRUE ~ 2 * nSMJHL,
              )
          ) %>% 
          relocate(
            slotsSHL,
            .after = SHL
          ) %>% 
          select(
            -`NA`
          ) %>% 
          arrange(
            Position
          ) %>% 
          mutate(
            fillSHL = SHL / slotsSHL,
            fillSMJHL = SMJHL / slotsSMJHL
          ) %>% 
          rename(
            `Currently in SHL` = SHL,
            `Currently in SMJHL` = SMJHL,
            `Total Slots in SHL` = slotsSHL,
            `Total Slots in SMJHL` = slotsSMJHL 
          )
        
        table <- 
          datatable(
            temp,
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
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
                dom = 't',
                columnDefs = 
                  list(
                    list(
                      targets = c(5,6),
                      visible = FALSE  
                    )
                  )
              )
          ) %>% 
          formatStyle(
            "Currently in SHL",
            valueColumns = "fillSHL",
            textAlign = "left",
            color = styleInterval(cuts = c(0.8, 1), values = c("white", "#e08b46", "red")),
            background = styleColorBar(c(0,1), "#2c6185", angle = -90),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>% 
          formatStyle(
            "Currently in SMJHL",
            valueColumns = "fillSMJHL",
            textAlign = "left",
            color = styleInterval(cuts = c(0.8, 1), values = c("white", "#e08b46", "red")),
            background = styleColorBar(c(0,1), "#2c6185", angle = -90),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        
        shinyjs::enable(id = "activity")
        
        return(table)
      })
      
      output$goalieSHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SHL",
            Position == "Goalie"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
          
      })
      output$defenseSHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SHL",
            Position == "Defense"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
        
      })
      output$forwardSHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SHL",
            Position == "Forward"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
        
      })
      output$goalieSMJHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SMJHL",
            Position == "Goalie"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
        
      })
      output$defenseSMJHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SMJHL",
            Position == "Defense"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
        
      })
      output$forwardSMJHL <- renderPlotly({
        data <- 
          currentData() %>% 
          filter(
            League == "SMJHL",
            Position == "Forward"
          ) 
        
        p <- 
          buildHist(data)  
        
        ggplotly(p)
        
      })
    }
  )
}

