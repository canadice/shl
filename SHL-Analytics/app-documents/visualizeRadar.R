
############################################################################
############################################################################
###                                                                      ###
###         TOOL TO VISUALIZE DISTRIBUTION OF PLAYERS ATTRIBUTES         ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

radarUI <- function(id){
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of type of player/goalie
    fluidRow(
      column(
        width = 2,
        offset = 1,
        box(
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Filters",
          radioButtons(
            inputId = ns("league"),
            label = "Select league",
            choices = 
              c(
                "SHL" = 0,
                "SMJHL" = 1
              ),
            selected = 0
          ),
          
          ## Selection of player type to show
          radioButtons(
            inputId = ns("player_type"),
            label = "Select player",
            choices = 
              c(
                "Goalies" = "goalies",
                "Skaters" = "players"
              ),
            selected = "players"
          )
        ),
        em(paste("The data is taken directly from the SHL Index which means that ",
                 "there exist some scouting errors. A scouting error means that a player's",
                 "attributes are shown +/- 1 thereby showing the wrong used TPE."))
      ),
      column(
        width = 8,
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Visualization",
          plotlyOutput(
            outputId = ns("radarPlotly")
          )
        ),
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Players",
          p("The data contains more information than what is shown. Filter", 
            "your own colums via the", em("Columns Visibility"), "button."),
          DTOutput(
            outputId = ns("dataTable")
          )
        )
      )
    )
  )
}


## Backend for vizualizations
radarSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(
        {input$league
        input$player_type},
        {
          shinyjs::disable(id = "league")
          shinyjs::disable(id = "player_type")
        },
        ignoreInit = FALSE
      )
      
      
      ## Initializes reactiveValues
      chosenRow <- reactiveVal(NULL)
      
      ## Observes options of player and goalie
      selectedData <- reactive({

        data <- 
          playerLoader(leagueID = input$league)[[input$player_type]] %>% 
          select(
            -id,
            -league,
            -season
          ) %>% 
          select(
            team,
            name,
            position,
            usedTPE,
            where(is.integer)
          )
        
        return(data)
      })
      
      ## Outputs a datatable of all the players
      output$dataTable <- DT::renderDT({
        table <- selectedData() %>% 
          rename_with(
            str_to_upper
          )
        
        {
          shinyjs::enable(id = "player_type")
          shinyjs::enable(id = "league")
        }
        
        table
        
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
            buttons = c('copy', 'csv', 'excel', I('colvis')),
            ## Defines which columns the table shows by default
            columnDefs = list(
              list(
                visible = TRUE,
                targets = 0:6
              ),
              list(
                visible = FALSE,
                targets = '_all'
              )
            )
          ),
        extensions = c('Buttons')
      )
      
      ## Observes event of last row that is clicked
      observeEvent(
        input$dataTable_rows_selected,
        {
          chosenRow(input$dataTable_rows_selected)
        }
      )
      
      output$radarPlotly <- renderPlotly({
        if(is.null(chosenRow())){
          plotly_empty(
            type = "scatter", 
            mode = "markers",
            width = 500,
            height = 400
            ) %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = "Select a player in the table below to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          selectedData() %>%
            slice(
              chosenRow()
            ) %>%
            select(
              name,
              position,
              where(is.integer)
            ) %>%
            pivot_longer(
              where(is.integer),
              names_to = "attributeIndex",
              values_to = "Rating"
            ) %>%
            left_join(
              attKey,
              by = c("attributeIndex")
            ) %>%
            mutate(
              Abbreviation =
                factor(
                  abb,
                  levels = c("SCR", "GTO", 'PAS', 'PHA', 'SAC', 'SRA', 'OFR',
                             'CHE', 'HIT', 'POS', 'SCH', 'SBL', 'FOF', 'DFR',
                             'ACC', 'AGI', 'SPD', 'STA', 'STR', 'BAL', 'FIG',
                             'AGR', 'BRA', 'DET', 'TPL', 'LEA', 'TEM', 'PRO',
                             'MTO', 'GST', 'BLO', 'GLO', 'GPA', 'POK', 'GPO', 
                             'REB', 'REC', 'GPH', 'LOW', 'REF', 'GSK')
                ),
              text = paste(attribute, Rating, sep = ": ")
            ) %>%
            arrange(
              Abbreviation
            ) %>%
            plot_ly(
              type = 'scatterpolar',
              mode = "markers",
              r = ~Rating,
              theta = ~Abbreviation,
              text = ~text,
              fill = 'toself',
              hoverinfo = "text",
              color = I("#cf5b00"),
              name = ~name,
              width = 500,
              height = 400
            ) %>%
            plotly::config(
              modeBarButtonsToRemove =
                c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                  "resetScale2d", "hoverClosestCartesian",
                  "hoverCompareCartesian", "toggleSpikelines"
                )
            ) %>%
            layout(
              autosize = FALSE,
              polar =
                list(
                  radialaxis =
                    list(
                      visible = TRUE,
                      range = c(0,20)
                    )
                ),
              ## Legend is put to false so the plot is the same size
              showlegend = FALSE
          )
        }
      })
    }
  )
}

