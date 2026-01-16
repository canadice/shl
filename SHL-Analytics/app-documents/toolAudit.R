
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
          reactableOutput(ns("comparison")) |> 
            withSpinner()
        ),
        tabPanel(
          "Missing players",
          p("Some players are listed wrong on either the portal or FHM. Given the selected league, the Portal or FHM unique players are listed below. 
            If the same name appears in both tables that means that the FHM ID matching is not correct, reach out to the Dev Team."),
          column(
            width = 6,
            h4("Players only found on the Portal"),
            reactableOutput(ns("forumMissing"))
          ),
          column(
            width = 6,
            h4("Players only found in FHM"),
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
      
      # portalBuilds <- reactive({
      #   readAPI("https://portal.simulationhockey.com/api/v1/player/snapshot") |>
      #     mutate(
      #       currentLeague = if_else(currentLeague == "SMJHL", 1, 0)
      #     ) |>
      #     filter(
      #       currentLeague == input$league
      #     )
      # })
      
      portalBuilds <- reactive({
        read_sheet("https://docs.google.com/spreadsheets/d/1jxqfu5uXrFpv8_KfOfVnBpeQUhP6d0ag-tvoCCxj41Q/edit?usp=sharing") |> 
          mutate(
            currentLeague = if_else(currentLeague == "SMJHL", 1, 0)
          ) |>
          filter(
            currentLeague == input$league
          ) |> 
          select(name, appliedTPE, currentTeamID, screening:professionalism...80, 
                 contains(paste0("league_", input$league))) |> 
          rename(
            passing = passing...37,
            puckhandling = puckhandling...38,
            positioning = positioning...44,
            aggression = aggression...56,
            determination = determination...58,
            teamPlayer = teamPlayer...59,
            leadership = leadership...60,
            professionalism = professionalism...62,
            id = 50
          ) |> 
          select(!contains("...")) |> 
          filter(
            !is.na(id)
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
        portalBuilds()$name[!((portalBuilds()$id) %in% (indexAttributes()$id))] %>% sort()
      })
        
      indexMissing <- reactive({
        indexAttributes()$name[!((indexAttributes()$id) %in% (portalBuilds()$id))] %>% sort()
      })
      
      discrepancy <- reactive({
        comparedf(
          indexAttributes() |> 
            select(id, screening:professionalism, blocker:goaltenderStamina) |> 
            mutate(id = as.numeric(id)) |> 
            arrange(id),
          # cbind(
          #   pid = portalBuilds()$pid,
          #   portalBuilds()$attributes
          #   ) |> 
          #   arrange(pid),
          portalBuilds() |> 
            select(id, screening:professionalism, blocker:goaltenderStamina) |> 
            mutate(across(everything(), as.integer)),
          by = "id"
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
                id,
                name,
                currentTeamID
              ),
            by = "id"
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
          select(id, name, team, `Index Applied TPE` = appliedTPE) |> 
          full_join(
            portalBuilds() |> 
              select(id, name, `Portal Applied TPE` = appliedTPE),
            by = "id"
          ) |> 
          filter(
            if(input$league == 1){
              `Index Applied TPE` > 425 | `Index Applied TPE` != `Portal Applied TPE`
            } else {
              `Index Applied TPE` != `Portal Applied TPE` | `Index Applied TPE` < 155
            }
          ) |> 
          select(
            team, name = name.x, `Index Applied TPE`, `Portal Applied TPE`
          ) |> 
          arrange(team, name) |> 
          reactable()
      })
    }
  )
}

