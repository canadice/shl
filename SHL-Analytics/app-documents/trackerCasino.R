
###########################################################################
###########################################################################
###                                                                     ###
###              Looking at players per draft class                     ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
casinoUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        p("The table below shows the current standings taken from the SHL Index",
          "and compares it to the casino lines. The expected number of wins is",
          "calculated using the win rate of the current record and extrapolating",
          "it to the full season, and the prediction is a result of those wins",
          "compared to the casino line."),
        box(
          width = NULL,
          DTOutput(ns("current"))
        )
      )
    )
 )
}

## Backend module for player similarities
casinoSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ### Loads the current standings from Index
      standings <- 
        teamLoader(0) %>% 
        select(
          id,
          name,
          abbreviation,
          stats
        )
      
      ### Loads schedule from index
      games <- gamesLoader(0, type = "regular")
      
      ### Activates googlesheets4 without authentication
      gs4_deauth()
      
      casinoLines <- 
        googlesheets4::read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1YMnvEmMikRodX6k6Ig5tpGR8gpQuqNJIJNXZGuPHxcs/edit?usp=sharing"
        )
      
      ### Combines casino lines and standings
      currentCasino <- 
        left_join(
          casinoLines %>% 
            select(
              TeamId, 
              Casino
            ),
          standings,
          by = c("TeamId" = "id")
        ) %>% 
        mutate(
          GP = stats$wins + stats$losses + stats$overtimeLosses + stats$shootoutLosses,
          Record = 
            paste(
              stats$wins,
              stats$losses + stats$overtimeLosses + stats$shootoutLosses,
              sep = "-"
            ),
          XWin = 
            ((stats$wins)/GP * 66) %>% 
            round(2),
          Prediction = 
            case_when(
              XWin > Casino ~ "Over",
              TRUE ~ "Under"
            ),
          RemainingRecordForOver =
            case_when(
              (Casino - (stats$wins)) < 0 ~ "Achieved",
              (Casino - (stats$wins)) > (66 - GP) ~ "Not possible",
              TRUE ~ 
                paste(
                  (Casino - (stats$wins)) %>% 
                    ceiling(),
                  (66 - GP - (Casino - (stats$wins))) %>% 
                    floor(),
                  sep = "-"
                )
            ),
          PaceVsCasino = XWin - Casino %>% round(2)
        ) %>% 
        select(-TeamId, -stats, -abbreviation) %>% 
        relocate(name, .before = Casino) %>% 
        rename(
          Team = name,
          `Casino Line` = Casino,
          `Games Played` = GP,
          `Expected Wins` = XWin,
          `Current Record (W-L)` = Record,
          `Remaining for Over` = RemainingRecordForOver,
          `Expected Difference vs Casino` = PaceVsCasino 
          ) %>% 
        arrange(Team)
      
      output$current <- renderDT({
        currentCasino %>% 
          datatable(
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = TRUE, 
                ## Removes pages in the table
                paging = FALSE,
                dom = 't'
              )
          ) %>% 
          formatRound("Expected Difference vs Casino", digits = 2)
      })
      
    }
  )
}

