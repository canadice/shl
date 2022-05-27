
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

teamUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(
      HTML('
         #buttons {
         background-color:#f3f3f300; position:fixed; margin-bottom:50px; opacity:1; height:50px; z-index:5;
         display: flex;
         align-items: center;
         justify-content: center;
         }

         ')
    ),
    fluidRow(
      column(
        width = 12,
        ### Select the league
        div(
          id = ns("intro"), 
          br(),
          tags$button(
            id = ns("shl_button"),
            class = "btn action-button",
            tags$img(src = "https://raw.githubusercontent.com/canadice/shl/main/graphics/shl_logo.png",
                     height = "300px")
          ),
          tags$button(
            id = ns("smjhl_button"),
            class = "btn action-button",
            tags$img(src = "https://raw.githubusercontent.com/canadice/shl/main/graphics/smjhl_logo.png",
                     height = "300px")
          ),
          align = "center"
        ),
        ### The actual content to show after selecting a league
        div(
          id = ns("content"),
          column(
            width = 12,
            div(
              br(),
              imageOutput(
                outputId = ns("logo"),
                height = "100%"
              ),
              style=
              "position: relative;
              margin: auto;",
              align = "center",
              div(
                style = 
                "position: absolute;
                top: 0px;
                left: 0px;
                width: 100%;",
                align = "left",
                fluidRow(
                  column(
                    width = 1,
                    offset = 1,
                    tags$button(
                      id = ns("back_button"),
                      class = "btn action-button",
                      icon("caret-square-left", "fa-3x")
                    )
                  ),
                  column(
                    width = 4,
                    uiOutput(outputId = ns("selectTeams"))
                  )
                ),
                fluidRow(
                  column(
                    width = 10,
                    offset = 1,
                    id = ns("teamTabBox"),
                    uiOutput(outputId = ns("dataTableCSS")),
                    tabBox(
                      width = NULL,
                      id = ns("tabBox"),
                      tabPanel(
                        title = "Current Roster",
                        id = ns("roster"),
                        DTOutput(outputId = ns("dataTableRoster")),
                        
                      ),
                      tabPanel(
                        title = "Prospects",
                        id = ns("prospect"),
                        DTOutput(outputId = ns("dataTableProspect")) 
                      ),
                      tabPanel(
                        title = "TPE Visualizer",
                        id = ns("tpeVisualizer"),
                        fluidRow(
                          column(
                            width = 4,
                            offset = 4,
                            radioButtons(
                              inputId = ns("tpeGrouping"),
                              label = "Select filter",
                              choices = 
                                c(
                                  "Position" = "Positiongroup",
                                  # "Class",
                                  "Roster"
                                ),
                              selected = "Positiongroup"
                            )  
                          )
                        ),
                        column(
                          width = 8,
                          offset = 2,
                          plotlyOutput(
                            outputId = ns("teamTPECharts")
                          )  
                        )
                      )
                    )
                  )
                )
              ) %>% 
                fluidRow()
            )
          )
        ) %>% 
          hidden() %>% 
          fluidRow()
      )
    )
  )
}


## Backend for vizualizations
teamSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      ##---------------------------------------------------------------
      ##                    Settings for datatables                   -
      ##---------------------------------------------------------------
      
      teamTableOptions <- 
        list(
          orderClasses = FALSE, 
          ## Sets a scroller for the rows
          scrollY = '500px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = TRUE,
          # pageLength = 10,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'ft',
          bInfo = FALSE,
          ## Defines which columns the table shows by default
          columnDefs = 
            list(
              list(
                targets = 0,
                orderData = 6
              ),
              list(
                targets = 3,
                orderData = 7
              ),
              list(
                targets = c(6:7),
                visible = FALSE
              )
            )
        )
      ##----------------------------------------------------------------
      ##                    Observers for the page                     -
      ##----------------------------------------------------------------
      
      ### Observes when either SHL or SMJHL is clicked
      observeEvent(
        {
          input$shl_button
        },
        {
          toggle(id = "intro")
          toggle(id = "content")
          activeData(SHLData)
          activeLeague("SHL")
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        {
          input$smjhl_button
        },
        {
          toggle(id = "intro")
          toggle(id = "content")
          activeData(SMJHLData)
          activeLeague("SMJHL")
        },
        ignoreInit = TRUE
      )
      
      ### Observes when the back button is clicked
      observeEvent(
        {
          input$back_button
        },
        {
          toggle(id = "intro")
          toggle(id = "content")
        },
        ignoreInit = TRUE
      )
      
      ### Observes when a team is selected
      observeEvent(
        {
          input$selectedTeam
        },
        {
          ### Updates the player data to the active team
          activeData() %>% 
            filter(
              Team == input$selectedTeam |
                Shl.team == input$selectedTeam
            ) %>% 
            teamData()
          
          ### Updates the team colors to the selected team
          teamInfo %>% 
            filter(
              team == input$selectedTeam
            ) %>% 
            filter(
              Inaugural.Season == max(Inaugural.Season)
            ) %>% 
            select(primary) %>% 
            unname() %>% 
            teamColor()
        }
      )
      
      ##---------------------------------------------------------------
      ##      Creating the SHL and SMJHL data that is used            -
      ##---------------------------------------------------------------
      
      SHLData <- 
        forumData %>% 
        mutate(
          Roster = if_else(leagueID == 0, TRUE, FALSE),
          Prospect = if_else(leagueID != 0, TRUE, FALSE)
        ) %>% 
        mutate(
          PositionGroup =
            case_when(
              POSITION %>% str_detect(pattern = "Defense") ~ "D",
              POSITION %>% str_detect(pattern = "Goalie") ~ "G",
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
        rename_with(
          .cols =
            c(
              -TPE
            ),
          stringr::str_to_title
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
        mutate(
          TPEbins = 
            cut(
              TPE, 
              breaks = c(155,425,800, 1200, 1600, 2000, 2500),
              labels = 
                c("155-425",
                  "426-800",
                  "801-1200",
                  "1201-1600",
                  "1601-2000",
                  "2001+"),
              right = FALSE
            )
        )
      
      SMJHLData <- 
        forumData %>% 
        filter(leagueID == 1) %>% 
        mutate(
          Roster = TRUE,
          Prospect = if_else(!is.na(SHL.Team), TRUE, FALSE)
        ) %>% 
        mutate(
          PositionGroup =
            case_when(
              POSITION %>% str_detect(pattern = "Defense") ~ "D",
              POSITION %>% str_detect(pattern = "Goalie") ~ "G",
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
        rename_with(
          .cols =
            c(
              -TPE
            ),
          stringr::str_to_title
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
        mutate(
          TPEbins = 
            cut(
              TPE, 
              breaks = c(155, 275, 350, 425, 800, 2500),
              labels = 
                c("155-275",
                  "276-350",
                  "351-425",
                  "426-800",
                  "800+"),
              right = FALSE
            ) 
        )
      
      
      ##----------------------------------------------------------------
      ##                          UI Outputs                           -
      ##----------------------------------------------------------------
      
      ### Outputs the team selector based on the league selected
      output$selectTeams <- 
        renderUI({
          if(is.null(activeData())){
            # Do nothing
          } else {
            selectInput(
              inputId = session$ns("selectedTeam"),
              label = "Select a team",
              choices = activeData() %>% filter(League==activeLeague()) %>% select(Team) %>% unique()
            )  
          }
        })
      
      ### Changes the CSS values of the datatable
      output$dataTableCSS <- 
        renderUI({
          tags$style(
            HTML(
              ### Kenvald you beautiful specimen!
              "
              #teamUI-teamTabBox .nav-tabs-custom,
              #teamUI-teamTabBox .nav-tabs-custom>.tab-content, 
              #teamUI-teamTabBox .nav-tabs-custom>.nav-tabs
              {background: #FFFFFF00;}
              ",
              ## The use of !important is important to overwrite site default css values for dataTables
              ## #id of the datatable only changes it for this module and not the entire datatable format
              paste0("#teamUI-teamTabBox tbody tr.odd, 
                    #teamUI-teamTabBox tbody tr.odd 
                    {background-color:", paste0(teamColor(), 25), "!important}
                    "),
              paste0("#teamUI-teamTabBox tbody tr
                     {background-color:", paste0("#FFFFFF", 10), "!important}")
            )
          )
        })
      
      ##---------------------------------------------------------------
      ##                      Datatable Outputs                       -
      ##---------------------------------------------------------------
      
      output$dataTableRoster <- DT::renderDT({
        if(teamData() %>% is.null()){
          NULL
        } else{
          teamData() %>% 
            filter(
              Team == input$selectedTeam
            ) %>% 
            select(
              Name,
              Class,
              Position,
              User,
              TPE,
              Active,
              Roster,
              Prospect,
              Namesort,
              Usersort
            ) %>%
            filter(
              Roster == TRUE
            ) %>%
            arrange(
              Active,
              -TPE
            ) %>% 
            select(
              -Roster,
              -Prospect
            ) %>% 
            datatable(
              rownames = FALSE,
              escape = FALSE,
              selection = "none",
              options = teamTableOptions
            )
        }
      }
      )
      
      output$dataTableProspect <- DT::renderDT({
        if(teamData() %>% is.null()){
          NULL
        } else{
          teamData() %>% 
            select(
              Name,
              Class,
              Position,
              User,
              TPE,
              Active,
              Roster,
              Prospect,
              Namesort,
              Usersort
            ) %>%
            filter(
              Prospect == TRUE
            ) %>% 
            arrange(
              Active,
              -TPE
            ) %>% 
            select(
              -Roster,
              -Prospect
            ) %>%  
            datatable(
              rownames = FALSE,
              escape = FALSE,
              selection = "none",
              options = teamTableOptions
            )
        }
      }
      )
      
      
      ##---------------------------------------------------------------
      ##                        Image Outputs                         -
      ##---------------------------------------------------------------
      
      output$logo <- 
        renderImage({
          
          if(input$selectedTeam %>% is.null()){
            tempImage <- 
              image_blank(width = 200, height = 200) %>% 
              image_write(tempfile(fileext = "png"), format = "png")
          } else {
            visData <- 
              teamInfo %>% 
              filter(
                team == input$selectedTeam
              ) %>% 
              filter(
                Inaugural.Season == max(Inaugural.Season)
              )
            
            tempImage <- 
              image_draw(
                visData$logoImage[[1]],
                xlim = c(0,1),
                ylim = c(0,1)
              ) %>%
              image_resize("x600") %>% 
              image_colorize(opacity = 90, color = "white") %>% 
              image_write(tempfile(fileext = "png"), format = "png")
            
            dev.off()
          }
          
          list(src = tempImage, contentType = "image/png")
        },
        deleteFile = TRUE)
      
      
      ##---------------------------------------------------------------
      ##              Visualization of TPE Distribution               -
      ##---------------------------------------------------------------
      
      output$teamTPECharts <- renderPlotly({
        
        data <- 
          teamData() %>% 
          filter(
            Roster | Prospect
          ) %>% 
          mutate(
            group = get(input$tpeGrouping)
          ) %>% 
          group_by(group, TPEbins, Class) %>% 
          mutate(
            classCount = n()
          ) 
          
        classText <- 
          data %>% 
          group_by(group, TPEbins) %>% 
          summarize(
            binCount = n(),
            text = paste(Class, classCount, sep = ": ") %>% unique() %>% sort() %>% paste(collapse = "\n")
          )
        
        visData <- 
          data %>% 
          left_join(
            classText,
            by = c("TPEbins", "group")
          ) %>% 
          ungroup() %>% 
          select(
            group,
            TPEbins,
            binCount,
            text
          ) %>% 
          unique()
        
        p <- 
          ggplot(visData) + 
          aes(x = TPEbins, y = binCount, text = text) +
          geom_col(
            color = "black",
            fill = paste(teamColor()),
            width = 1
          ) + 
          labs(y = "Count", x = "TPE") + 
          facet_grid(
            rows = vars(group)
          ) +
          theme_minimal() + 
          scale_x_discrete(drop = FALSE) + 
          scale_y_continuous(
            expand = expansion(add = c(0, 1)),
            breaks = seq(0, 100, 2)
          ) +
          theme(
            panel.spacing.y = unit(1.5, "lines"),
            strip.background.y = element_rect(fill = "white"),
            strip.text.y = element_text(angle = 0, size = 12), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray60"),
            panel.background = element_rect(fill = "transparent"), 
            axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"),
            plot.background = element_rect(fill = "transparent", color = "black")
          ) + 
          annotate("segment", x=-Inf, xend=Inf, y=0, yend=0)
        
        p %>% 
          ggplotly(tooltip = "text") %>% 
          layout(hoverlabel=list(bgcolor="white")) %>% 
          config(
            displayModeBar = FALSE
          )
          
      })
      
      ##------------------------------------------------------------------------
      ##  Initialize reactiveValues that will be used as interactive options   -
      ##------------------------------------------------------------------------
      
      chosenRow <- reactiveVal(NULL)
      
      activeData <- reactiveVal(NULL)
      
      activeLeague <- reactiveVal(NULL)
      
      teamData <- reactiveVal(NULL)
      
      teamColor <- reactiveVal(NULL)
      
      
    }
  )
}

