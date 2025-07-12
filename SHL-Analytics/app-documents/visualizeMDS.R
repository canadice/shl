
###########################################################################
###########################################################################
###                                                                     ###
###              VISUALIZING PLAYER SIMILARITIES USING MDS              ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
mdsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of season of players
    # Option for coloring in visualization
    fluidRow(
      column(
        width = 3,
        box(
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          title = "Filters",
          ## Selection of league
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
          
          ## Season selection
          uiOutput(
            outputId = ns("selectedSeason")
          ),
          ## Coloring selection
          radioButtons(
            inputId = ns("color_group"),
            label = "What should define the colors?",
            choices = 
              c(
                "Position" = "position",
                "Team" = "team"
              )
          ),
          ## Selection of measure
          radioButtons(
            inputId = ns("measure"),
            label = "Type of ratings",
            choices = 
              c(
                "Absolute" = "absolute",
                "Relative" = "relative"
              ),
            selected = "relative"
          ),
          br(),
          p("The Absolute option looks at the player ratings in order of magnitude,",
            "i.e. 18 is 6 different from 12, while the Relative option looks at a",
            "player's relative attributes, for example 10 percent of the total sum",
            "of attributes could be Checking irregardless of the total amount of TPE used.",
            "This option should be able to pick up on templates better than Absolute."
          )
        ),
        em(
          "Disclaimer: The data comes from the current index with",
          "the given position from that source. I am aware that some differences",
          "between the played and created position might be present. There also",
          "exists minor 'scouting errors', where some attributes are shown +/- 1 from",
          "the real values."
        )
      ),
      column(
        width = 8,
        tabBox(
          width = NULL,
          tabPanel(
            "Visualization",
            br(),
            ## Some explanatory text of the visualization
            h4("This is an interactive plot"
            ),
            p("Clicking on individual groups in the legend,
              removes their points from the graph."
            ),
            p("If you double click on a specific value in the legend,
              only the points from that group are shown."
            ),
            p("Clicking on removed groups in the legend, adds them to the selection."
            ),
            p("Create a box with your mouse cursor to select and view ratings 
              for at most three players. Hold in shift while dragging to 
              select players from different areas of the plot."
            ),
            ## The MDS plot
            plotlyOutput(
              outputId = ns("distPlot")
            ),
            
            ## The following is useful for testing purposes what data is shown
            # verbatimTextOutput("click"), 
            br(),
            
            ## The radar plot
            uiOutput(
              outputId = ns("ratingHeader")
            ),
            plotlyOutput(
              outputId = ns("ratingPlot")
            )
          ),
          # The explanation of distances
          tabPanel(
            "Explanation",
            p("Explanation doesn't work.")
            # withMathJax(
            #   includeMarkdown(
            #     "app-documents/mdExplanation.md"
            #   )
            # )
          )
        )
      )
    )
  )
}

## Backend module for player similarities
mdsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      currentSeason <- 
        playerLoader(0)$players %>% 
        select(
          season
        ) %>% 
        unique() %>% 
        unlist()
      
      output$selectedSeason <- 
        renderUI({
          numericInput(
            inputId = session$ns("season"),
            label = "Season",
            min = 53,
            max = currentSeason,
            value = currentSeason
          )
        })
      
      ## Selects the current data from the selected season
      current_data <- reactive({
        
        metadata <- 
          playerLoader(
            leagueID = input$league,
            season = input$season)$players %>% 
          select(
            -league,
            -season
          ) %>% 
          select(
            id,
            team,
            name,
            position,
            usedTPE,
            where(is.integer)
          ) %>% 
          tibble::column_to_rownames(
            var = "name"
          ) %>% 
          mutate(
            position = 
              factor(
                position,
                levels = 
                  c("LD", "RD", "LW", "RW", "C")
                )
          )
        
        
        if(input$measure == "relative"){
          data <- 
            metadata %>% 
            select(
              id, 
              where(is.integer)
            ) %>% 
            rowwise() %>% 
            mutate(
              id = id %>% as.character(),
              across(
                where(
                  is.integer
                ),
                ~ .x / sum(c_across(where(is.integer)))
              )
            ) %>% 
            mutate(
              id = id %>% as.numeric()
            )
          
          ## Calculates Euclidean distances between the players
          d <- dist(
            data[,-1], 
            method = "euclidean"
          )
          
        } else {
          data <- 
            metadata %>% 
            select(
              id, 
              where(is.integer)
            ) %>% 
            mutate(
              across(.fns = as.numeric)
            )
          
          ## Calculates Manhattan distances between the players
          d <- dist(
            data[,-1], 
            method = "manhattan"
          )
        }
        
        ## Fits the MDS onto two dimensions based on the distances
        fit <- cmdscale(
          d, 
          eig = TRUE, 
          k = 2
        )
        
        ## Creates a data.frame with the two dimensional coordinates
        ## and the player ID
        distances <- 
          data.frame(
            x = fit$points[,1],
            y = fit$points[,2], 
            id = data$id
          ) %>% 
          
          ## Joins the other data to the players by their ID
          left_join(
            metadata %>% 
              tibble::rownames_to_column(
                "name"
              ), 
            by = c("id" = "id")
          ) %>% 
          
          ## Creates a column called group based on the option selected
          mutate(
            group = 
              metadata[,input$color_group]
          )
        
        ## Returns the created data.frame of the players from the current season, 
        ## their distances, and the coloring column
        return(distances)
      })
      
      ## Produces the plot of the MDS results
      output$distPlot <- renderPlotly({
        ## Loads the currently selected data 
        distances <- current_data()
        
        ## Creates a color palette for the groupings
        ## "Team" has too many groups for any palette so some colors are added
        if(input$color_group == "team"){
          fill <- 
            teamInfo %>% 
            select(
              abbr,
              primary
            ) %>% 
            tibble::column_to_rownames("abbr") %>% 
            t() %>% 
            unlist()
          
          color <- 
            teamInfo %>% 
            select(
              abbr,
              secondary
            ) %>% 
            tibble::column_to_rownames("abbr") %>% 
            t() %>% 
            unlist()
   
        } else {
          fill <- 
            brewer.pal(
              n = length(levels(distances$group)),
              name = "Paired"
            )
        }

        if(input$color_group == "team"){
          ggplot(distances) + 
            aes(
              x, 
              y, 
              fill = group,
              color = group,
              text = name
              ) + 
            geom_point(pch = 21, size = 3) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) + 
            scale_fill_manual("Team", values = fill) + 
            scale_color_manual("Team", values = color) +
            labs(x = "Dimension 1", y = "Dimension 2") +
            scale_x_continuous(labels = NULL) + 
            scale_y_continuous(labels = NULL)
        } else {
          ggplot(distances) + 
            aes(
              x, 
              y, 
              fill = group, 
              text = name
              ) + 
            geom_point(pch = 21, color = "black", size = 3) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) + 
            scale_fill_manual(input$color_group, values = fill) + 
            labs(x = "Dimension 1", y = "Dimension 2") +
            scale_x_continuous(labels = NULL) + 
            scale_y_continuous(labels = NULL) 
        }

        ggplotly(tooltip = c("fill", "text"), source = "distances") %>% 
          plotly::config(modeBarButtonsToRemove = c("pan2d", 
                                            "zoomIn2d", "zoomOut2d",
                                            "autoScale2d", "resetScale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian",
                                            "toggleSpikelines")) %>% 
          layout(title = paste("Season", input$season),
                 margin = list(t = 30),
                 dragmode = "select") %>% 
          event_register(event = "plotly_selected")
        
      })
      
      ##### For testing purposes #####
      # output$click <- renderPrint({
      #     d <- event_data("plotly_click", source = "distances")
      # 
      #     current_data <- current_data() %>%
      #         mutate(x = round(x, digits = 5),
      #                y = round(y, digits = 5))
      # 
      #     if(is.null(d)){
      #         "Click events appear here (double-click to clear)"
      #     } else{
      #         d <- d %>%
      #             mutate(x = round(x, digits = 5),
      #                    y = round(y, digits = 5))
      # 
      #         d <- d %>% left_join(current_data) %>%
      #             select(Name, AGR:DFR) %>%
      #             mutate(across(2:last_col(), .fns = as.numeric)) %>%
      #             pivot_longer(AGR:DFR, names_to = "Attribute", values_to = "Rating")
      # 
      #         d
      # 
      # 
      #     }
      # })
      #####
      
      output$ratingHeader <- renderUI({
        d <- event_data("plotly_selected", source = "distances")
        if(is.null(d)){
          NULL
        } else{
          h4("The attributes of the chosen player(s)")
        }

      })

      output$ratingPlot <- renderPlotly({
        d <- event_data("plotly_selected", source = "distances")

        current_data <- current_data() %>%
          mutate(x = round(x, digits = 5),
                 y = round(y, digits = 5))

        if(is.null(d) | is.null(nrow(d))){
          NULL
        } else if (nrow(d)<4){
          nrPlayers <- nrow(d)

          d <- d %>%
            mutate(x = round(x, digits = 5),
                   y = round(y, digits = 5))

          d <- d %>% 
            left_join(
              current_data, 
              by = c("x", "y")
            ) %>%
            select(
              -id
            ) %>% 
            select(
              name,
              usedTPE,
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
                )
            )

          players <- d %>%
            group_by(name) %>%
            group_split()


          fig <- 
            plot_ly(
              type = 'scatterpolar',
              mode = "markers",
              r = players[[1]]$Rating,
              theta = players[[1]]$Abbreviation,
              text = 
                paste(
                  players[[1]]$attribute, 
                  players[[1]]$Rating, 
                  sep = ":"
                  ),
              fill = 'toself',
              hoverinfo = "text",
              color = I("darkorange"),
              name = 
                paste(
                  unique(players[[1]]$name), 
                  "\nUsed TPE:", 
                  unique(players[[1]]$usedTPE)
                )
              )
          if(nrPlayers > 1){
            fig <- 
              fig %>% 
              add_trace(
                r = players[[2]]$Rating,
                theta = players[[2]]$Abbreviation,
                text = 
                  paste(
                    players[[2]]$attribute, 
                    players[[2]]$Rating,
                    sep = ":"
                    ),
                color = I("black"),
                name = 
                  paste(
                    unique(players[[2]]$name), 
                    "\nUsed TPE:", 
                    unique(players[[2]]$usedTPE)
                    )
                )
          }
          if(nrPlayers==3){
            fig <- fig %>%
              add_trace(
                r = players[[3]]$Rating,
                theta = players[[3]]$Abbreviation,
                text = 
                  paste(
                    players[[3]]$attribute, 
                    players[[3]]$Rating, 
                    sep = ":"
                    ),
                color = I("lightblue"),
                name = 
                  paste(
                    unique(players[[3]]$name), 
                    "\nUsed TPE:", 
                    unique(players[[3]]$usedTPE)
                  )
                )
          }

          fig %>%
            plotly::config(
              modeBarButtonsToRemove = 
                c("pan2d",
                  "zoomIn2d", "zoomOut2d",
                  "autoScale2d", "resetScale2d",
                  "hoverClosestCartesian",
                  "hoverCompareCartesian",
                  "toggleSpikelines")
              ) %>%
            layout(
              polar = 
                list(
                  radialaxis = 
                    list(
                      visible = TRUE,
                      range = c(0,20)
                      )
                  ),
              showlegend = TRUE
              )
        } else {
          NULL
        }
      })
    }
  )
}

