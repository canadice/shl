
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
      tabBox(
        width = NULL,
        tabPanel(
          "Discrepancies",
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

      forumMissing <- 
        forumAttributes$name[!((forumAttributes$name) %in% (indexAttributes$name))] %>% sort()
      indexMissing <- 
        indexAttributes$name[!((indexAttributes$name) %in% (forumAttributes$name))] %>% sort()
      
      matches <- 
        stringdistmatrix(
          forumMissing,
          indexMissing
        )
      
      output$forumMissing <- renderDT({
        data.frame(
          forum = forumMissing,
          index = 
            indexMissing[
              apply(
                matches, 
                MARGIN = 1, 
                FUN = function(x){
                  (x == min(x)) %>% which() %>% min()
                }
              )
            ],
          distance = 
            apply(
              matches, 
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
          index = indexMissing,
          forum = 
            forumMissing[
              apply(
                matches, 
                MARGIN = 2, 
                FUN = function(x){
                  (x == min(x)) %>% which() %>% min()
                }
              )
            ],
          distance = 
            apply(
              matches, 
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
        comparedf(
          forumAttributes %>% 
            arrange(name) %>% 
            mutate(
              across(
                where(is.numeric),
                ~ as.integer(.x)
              )
            ) %>% 
            select(-team),
          indexAttributes %>% 
            arrange(name) %>% 
            select(-team),
          by = "name"
        ) %>% 
          summary() %>% 
          .$diffs.table %>% 
          rename_with(
            ~ str_replace(.x, pattern = ".x", replacement = " from Forum")
          ) %>% 
          rename_with(
            ~ str_replace(.x, pattern = ".y", replacement = " from Index")
          ) %>% 
          select(
            !contains("row") &
            !`var from Index`
          ) %>% 
          rename(
            Attribute = `var from Forum`,
            Name = name
          ) %>% 
          left_join(
            forumAttributes %>% 
              select(
                name,
                team
              ),
            by = c("Name"="name")
          ) %>% 
          rename(Team = team) %>% 
          arrange(Team, Name)
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
