
############################################################################
############################################################################
###                                                                      ###
###                             AUDIT TOOL                               ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

auditUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        radioButtons(
          inputId = ns("league"),
          label = "Select the league",
          choices = c("SHL" = 0, "SMJHL" = 1)
        )
      )
    ),
    fluidRow(
      tabBox(
        width = NULL,
        tabPanel(
          "Discrepancies",
          uiOutput(
            ns("count")
          ),
          DTOutput(
            ns("comparison")
          )
        ),
        tabPanel(
          "Missing players",
          p("A number of players are either spelled wrong in the Updater Sheet or FHM and is not matched properly.",
            "There are also instances of players either existing only on the forum or only in FHM.",
            "For each name and source (Forum or Index/FHM), the closest match is shown in the respective column,",
            "indicating what, if anything, is wrong and from what source."),
          column(
            width = 6,
            h4("Players only found on the Forum"),
            DTOutput(
              ns("forumMissing")
            )
          ),
          column(
            width = 6,
            h4("Players only found in the Index"),
            DTOutput(
              ns("indexMissing")
            )
          )
        )
      ) %>% 
        column(
          width = 12
        )
    )
  )
}


## Backend for vizualizations
auditSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){

      
      forumPlayers <- reactive({
        forumData %>% 
          filter(
            leagueID == input$league
          ) %>% 
          mutate(
            name = 
              NAME %>% 
              tolower %>% 
              stringi::stri_trans_general(id = "Latin-ASCII") %>% 
              str_remove_all(pattern = "[[:punct:]]")
          )
      })
        
      indexPlayers <- reactive({
        indexAttributes %>% 
          filter(
            team %in% 
              (teamInfo %>% 
                 filter(
                   leagueID == input$league
                 ) %>% 
                 select(
                   abbr
                 ) %>% 
                 unlist()
              )
          ) %>% 
          mutate(
            name = 
              name %>% 
              tolower %>% 
              stringi::stri_trans_general(id = "Latin-ASCII") %>% 
              str_remove_all(pattern = "[[:punct:]]"),
            abbr = team
          ) %>% 
          select(-team)
      })
        
      
      forumMissing <- reactive({
        forumPlayers()$name[!((forumPlayers()$name) %in% (indexPlayers()$name))] %>% sort()
      })
        
      indexMissing <- reactive({
        indexPlayers()$name[!((indexPlayers()$name) %in% (forumPlayers()$name))] %>% sort()
      })
      
      discrepancy <- reactive({
        comparedf(
          forumPlayers() %>% 
            arrange(name) %>% 
            select(
              name,
              abbr,
              screening:goaltenderStamina
            ) %>% 
            mutate(
              across(
                where(is.numeric),
                ~ as.integer(.x)
              ),
              id = paste(name, abbr)
            ) %>% 
            rename_with(
              ~ str_to_lower(.x) %>% 
                str_replace_all(pattern = "x\\.|\\.", replacement = "")
            ) %>% 
            rename(goaliestamina = goaltenderstamina,
                   ),
          indexPlayers() %>% 
            arrange(name) %>% 
            select(
              name,
              abbr,
              Screening:Positioning
            ) %>% 
            mutate(
              id = paste(name, abbr)
            ) %>% 
            select(
              !(c(passing, puckhandling, positioning) | contains("goalieP"))
            ) %>% 
            rename_with(
              ~ str_to_lower(.x) %>% 
                str_replace_all(pattern = "x\\.|\\.", replacement = "")
            ),
          by = c("id")
        ) %>% 
          summary() %>% 
          .$diffs.table %>% 
          filter(
            !(values.x %>% is.na() | values.y %>% is.na()) 
          ) %>% 
          rename_with(
            ~ str_replace(.x, pattern = "\\.x", replacement = " from Player Page")
          ) %>% 
          rename_with(
            ~ str_replace(.x, pattern = "\\.y", replacement = " from Index")
          ) %>% 
          select(
            !contains("row") &
              !`var from Index`
          ) %>% 
          rename(
            Attribute = `var from Player Page`
          ) %>%
          left_join(
            forumPlayers() %>%
              select(
                name,
                abbr,
                team,
                leagueID
              ) %>% 
              mutate(
                id = paste(name, abbr)
              ),
            by = c("id")
          ) %>%
          select(
            -abbr,
            -id
          ) %>% 
          rename(
            Name = name,
            Team = team,
            League = leagueID
          ) %>%
          arrange(League, Team, Name)
      })
      
      matches <- reactive({
        stringdistmatrix(
          forumMissing(),
          indexMissing()
        )
      })
        
      output$count <- renderUI({
        p("The number of errors found are:", discrepancy() %>% nrow(), ".", 
          "The number of errors with differences larger than 1 are:", 
          discrepancy() %>% 
            mutate(
              difference = unlist(`values from Player Page`) - unlist(`values from Index`)
            ) %>% 
            summarize(
              n = sum(abs(difference) > 1)
            ) %>% 
            select(n) %>% 
            unlist() %>% 
            unname(),
          ".", "Anything above 10 discrepancies of differences larger than 1 indicates erorrs other than scouting errors."
          )
      })
      
      output$forumMissing <- renderDT({
        data.frame(
          forum = forumMissing(),
          index = 
            indexMissing()[
              apply(
                matches(), 
                MARGIN = 1, 
                FUN = function(x){
                  (x == min(x)) %>% which() %>% min()
                }
              )
            ],
          distance = 
            apply(
              matches(), 
              MARGIN = 1, 
              FUN = function(x){
                x[(x == min(x)) %>% which() %>% min()]
              }
            )
        ) %>% 
          arrange(distance) %>% 
          select(-distance)
      },
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollX = '800px',
          scrollY = '650px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Sets width of columns
          autoWidth = FALSE,
          ## Removes pages in the table
          paging = FALSE,
          dom = 't'
        )
      ) 
        
      output$indexMissing <- renderDT({
        data.frame(
          index = indexMissing(),
          forum = 
            forumMissing()[
              apply(
                matches(), 
                MARGIN = 2, 
                FUN = function(x){
                  (x == min(x)) %>% which() %>% min()
                }
              )
            ],
          distance = 
            apply(
              matches(), 
              MARGIN = 2, 
              FUN = function(x){
                x[(x == min(x)) %>% which() %>% min()]
              }
            )
        ) %>% 
          arrange(distance) %>% 
          select(-distance)
      },
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollX = '800px',
          scrollY = '650px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Sets width of columns
          autoWidth = FALSE,
          ## Removes pages in the table
          paging = FALSE,
          dom = 't'
        )
      ) 
      
      
        
      output$comparison <- renderDT({
        discrepancy()
      },
      escape = FALSE, 
      extensions = c('Buttons', 'Scroller'),
      fillContainer = TRUE,
      rownames = FALSE,
      options = 
        list(
          orderClasses = TRUE, 
          ## Sets a scroller for the rows
          scrollX = '800px',
          scrollY = '650px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Sets width of columns
          autoWidth = FALSE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          # pageLength = 20,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'Bfrtip',
          # bInfo = FALSE,
          buttons = c('copy', 'csv', 'excel')
        )) 
    }
  )
}

