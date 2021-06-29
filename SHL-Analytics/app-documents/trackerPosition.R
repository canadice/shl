
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
          width = 3
        ),
        column(
          width = 6,
          DT::DTOutput(
            outputId = ns("leagueAggregate")
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
      
      output$leagueAggregate <- DT::renderDT({
        nSHL <- (teamLoader(leagueID = 0) %>% nrow())
        nSMJHL <- (teamLoader(leagueID = 1) %>% nrow())
        
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
            IIHF.Nation,
            Original,
            Transfer.Season
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
          select(
            Class,
            Position,
            League,
            Active
          ) %>% 
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
          group_by(Class, Position) %>% 
          mutate(
            classAmount = n()
          ) %>% 
          group_by(League, Position) %>% 
          mutate(
            leagueAmount = n()
          ) %>% 
          filter(
            Active == "Active"
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
          ) 
        
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
            "SHL",
            valueColumns = "fillSHL",
            background = styleColorBar(c(0,1), 'steelblue', angle = -90),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>% 
          formatStyle(
            "SMJHL",
            valueColumns = "fillSMJHL",
            background = styleColorBar(c(0,1), 'steelblue', angle = -90),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      })
    }
  )
}

