
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
          div(
            imageOutput(
              outputId = ns("logo"),
              height = "100px"
            ),
            align = "center"
          ),
          selectizeInput(
            inputId = ns("iihfNation"),
            label = "Select a IIHF Federation",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = NULL
          ),
          textOutput(
            outputId = ns("aggText")
          ),
          br(),
          DT::DTOutput(
            outputId = ns("aggData")
          )
        ),
        column(
          width = 9,
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
 )
}

## Backend module for player similarities
iihfSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ### Defines options for DT tables that will be used multiple times
      playerTableOptions <- 
        list(
          orderClasses = FALSE, 
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
                targets = 0,
                orderData = 8,
                width = '100px'
              ),
              list(
                targets = 3,
                orderData = 9,
                width = '70px'
              ),
              list(
                targets = c(8:9),
                visible = FALSE
              )
            )
        )
      
      ## Observes and updates the list of nationalities in the UI list
      observe({
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          inputId = "iihfNation",
          choices = forumData$`IIHF NATION` %>% 
            unique() %>% 
            sort(),
          selected = "Unassigned",
          server = TRUE
        )  
      })
      
      ## Outputs nation logo
      output$logo <- 
        renderImage({
          
          if(input$iihfNation == "Unassigned"){
            tempImage <-
              image_read(
                "https://raw.githubusercontent.com/canadice/shl/main/graphics/Old/shl_old.png"
              ) %>% 
              image_resize("x100") %>%
              image_write(tempfile(fileext = "png"), format = "png")
          } else {
            visData <- 
              teamData %>% 
              filter(
                team == input$iihfNation
              )  
            
            tempImage <- 
              image_draw(
                visData$logoImage[[1]],
                xlim = c(0,1), 
                ylim = c(0,1)
              ) %>%
              image_resize("x100") %>% 
              image_write(tempfile(fileext = "png"), format = "png")
          }
          list(src = tempImage, contentType = "image/png")
        },
        deleteFile = TRUE)
      
      
      ## Outputs aggregated numbers in text
      output$aggText <- renderText({
        paste(
          "Team", input$iihfNation, "has", nrow(currentData() %>% filter(Active == "Active")), "active players."
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
            ACTIVE,
            ABBR,
            LEAGUE,
            `IIHF NATION`,
            Original,
            `Transfer Season`
          ) %>%
          filter(
            `IIHF NATION` == (input$iihfNation)
          ) %>%
          rename(
            TEAM = ABBR,
            `IIHF Federation` = `IIHF NATION`
          ) %>%
          rename_with(
            .cols =
              c(
                -TPE,
                -`IIHF Federation`
              ),
            stringr::str_to_title
          ) %>%
          arrange(Class) %>%
          # mutate(
          #   Name = iconv(Name, to = "UTF-8"),
          #   User = iconv(User, to = "UTF-8")
          # ) %>%
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
          relocate(
            `IIHF Federation`:`Transfer Season`,
            .after = PositionGroup
          ) %>% 
          arrange(
            Active,
            -TPE
          ) %>% 
          return()
      })
      
      ## Outputs a datatable of all the players
      output$dataTableAll <- DT::renderDT({
        currentData() %>% 
          select(-PositionGroup)
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = playerTableOptions,
      extensions = c('Buttons')
      )
      
      output$dataTableG <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "G") %>% 
          select(-PositionGroup, -IIHF.Nation, -Original, -`Transfer Season`)
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = playerTableOptions,
      extensions = c('Buttons')
      )
      
      output$dataTableD <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "D") %>% 
          select(-PositionGroup, -IIHF.Nation, -Original, -`Transfer Season`)
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = playerTableOptions,
      extensions = c('Buttons')
      )
      
      output$dataTableF <- DT::renderDT({
        currentData() %>% 
          filter(PositionGroup == "F") %>% 
          select(-PositionGroup, -IIHF.Nation, -Original, -`Transfer Season`)
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = playerTableOptions,
      extensions = c('Buttons')
      )
      
      output$distrTPE <- renderPlot({
        
        
        
        ggplot(data) + aes(x = x, y = y) + 
          geom_violin(
            fill = "#52307c"
          ) +
          theme_bw() +
          scale_x_discrete(
            labels = 
              c(
                "No", 
                "Yes"
              )
          ) +
          labs(
            x = "In HoF?", 
            y = statistic
          ) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray60"),
            panel.grid.minor.y = element_line(color = "gray75")
          ) +
          stat_summary(
            fun = median,
            geom = "point",
            shape = 21,
            size = 4,
            fill = "white"
          )+ 
          stat_summary(
            fun = function(x){
              quantile(
                x,
                probs = c(0.25)
              )
            },
            geom = "point",
            shape = 25,
            size = 3,
            fill = "white"
          )+ 
          stat_summary(
            fun = function(x){
              quantile(
                x,
                probs = c(0.75)
              )
            },
            geom = "point",
            shape = 24,
            size = 3,
            fill = "white"
          )
        
      })
      
      output$aggData <- DT::renderDT({
        currentData() %>% 
          group_by(Active, PositionGroup, League) %>% 
          summarize(
            Amount = n()
          ) %>% 
          pivot_wider(
            names_from = League,
            names_sep = " ",
            values_from = Amount
          ) %>% 
          rename(
            Pos = PositionGroup
          )
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          orderClasses = FALSE, 
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

