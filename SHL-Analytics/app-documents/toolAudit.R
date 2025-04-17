
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
          uiOutput(ns("count")),
          reactableOutput(ns("comparison"))
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
            reactableOutput(ns("forumMissing"))
          ),
          column(
            width = 6,
            h4("Players only found in the Index"),
            reactableOutput(ns("indexMissing"))
          )
        ),
        tabPanel(
          "TPE Checks",
          p("This table shows players that in the file have exceeded the SMJHL cap or have applied more TPE than they have earned."),
          reactableOutput(ns("tpeChecks"))
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
      
      portalBuilds <- reactive({
        readAPI("https://portal.simulationhockey.com/api/v1/player/snapshot") |> 
          mutate(
            currentLeague = if_else(currentLeague == "SMJHL", 1, 0)
          ) |> 
          filter(
            currentLeague == input$league
          )
      })
      
      indexAttributes <- reactive({
        playerLoader(input$league) %>% 
          do.call(what = rbind.fill, args = .) |>  
          mutate(
            position = 
              factor(
                position,
                levels = 
                  c(
                    "C", "LW", "RW", "LD", "RD", "G"
                  )
              )
          ) %>% 
          relocate(
            usedTPE,
            .after = position
          ) %>% 
          relocate(
            team,
            .before = name
          ) %>% 
          select(
            -league, -season
          ) %>% 
          arrange(
            team, position, name
          ) |> 
          dplyr::mutate(
            Passing = case_when(
              is.na(passing) ~ goaliePassing,
              TRUE ~ passing
            ),
            Puckhandling = case_when(
              is.na(puckhandling) ~ goaliePuckhandling,
              TRUE ~ puckhandling
            ),
            Positioning = case_when(
              is.na(positioning) ~ goaliePositioning,
              TRUE ~ positioning
            )
          ) |> 
          select(!c(goaliePassing, goaliePositioning, goaliePuckhandling)) |> 
          rename(goaltenderStamina = goalieStamina)
      })
      
      forumMissing <- reactive({
        portalBuilds()$name[!((portalBuilds()$pid) %in% (indexAttributes()$id))] %>% sort()
      })
        
      indexMissing <- reactive({
        indexAttributes()$name[!((indexAttributes()$id) %in% (portalBuilds()$pid))] %>% sort()
      })
      
      discrepancy <- reactive({
        comparedf(
          indexAttributes() |> 
            select(id, screening:professionalism, blocker:goaltenderStamina) |> 
            mutate(id = as.numeric(id)) |> 
            arrange(id),
          cbind(
            pid = portalBuilds()$pid,
            portalBuilds()$attributes
            ) |> 
            arrange(pid),
          by.y = "pid",
          by.x = "id"
        ) %>% 
          summary() %>% 
          .$diffs.table %>% 
          filter(
            !(values.x %>% is.na() | values.y %>% is.na()) 
          ) %>% 
          rename_with(
            ~ str_replace(.x, pattern = "\\.x", replacement = " from Index/FHM")
          ) %>% 
          rename_with(
            ~ str_replace(.x, pattern = "\\.y", replacement = " from Portal")
          ) %>% 
          select(!contains("row") & !`var from Index/FHM`) %>% 
          rename(Attribute = `var from Portal`) %>%
          mutate(id = as.numeric(id)) |> 
          left_join(
            portalBuilds() %>%
              select(
                pid,
                name,
                currentTeamID
              ),
            by = c("id" = "pid")
          ) %>%
          select(!id) %>%
          arrange(currentTeamID, name)
      })
      
      output$forumMissing <- renderReactable({
        data.frame(
          forum = forumMissing()
          )|> 
          reactable()
      }) 
        
      output$indexMissing <- renderReactable({
        data.frame(
          index = indexMissing()
          )|> 
          reactable()
      }) 
        
      output$comparison <- renderReactable({
        discrepancy() |> 
          reactable()
      }) 
      
      output$tpeChecks <- renderReactable({
        indexAttributes() |> 
          filter(
            if(input$league == 1){
              appliedTPE > 425 | appliedTPE > usedTPE
            } else {
              appliedTPE > usedTPE
            }
          ) |> 
          select(
            team, name, usedTPE, appliedTPE
          ) |> 
          arrange(team, name) |> 
          reactable()
      })
    }
  )
}

