
############################################################################
############################################################################
###                                                                      ###
###                        IIHF LIMITS CALCULATOR                       ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

limitsIIHFUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        tabBox(
          width = NULL,
          tabPanel("Forum Output", verbatimTextOutput(ns("forum"))),
          tabPanel("Discord Output", verbatimTextOutput(ns("discord")))
        )
      )
    )
  )
}


## Backend for vizualizations
limitsIIHFSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      limits <- 
        forumData %>% 
        mutate(
          `IIHF NATION` = if_else(`IIHF NATION` == "Russia", "Independent Russia", `IIHF NATION`)
        ) %>% 
        dplyr::group_by(`IIHF NATION`) %>% 
        dplyr::filter(
          TPE >= 425,
          !(`IIHF NATION` %in% c("Unassigned", NA))
        ) %>% 
        dplyr::summarize(
          n = n()
        ) %>% 
        dplyr::mutate(
          `Transfer In` = 
            dplyr::case_when(
              n < 36 ~ 3,
              n < 46 ~ 2,
              TRUE ~ 1
            ),
          `Transfer Out` = 
            dplyr::case_when(
              n < 26 ~ 1,
              n < 36 ~ 1,
              n < 46 ~ 2,
              n < 66 ~ 3,
              TRUE ~ 4
            )
        )
      
      output$forum <- renderText({
        paste(
          "See the revised [url=https://simulationhockey.com/iihfrulebook.php]Rulebook[/url]. If you have any questions, reach out to me on site or discord.

An [b]unassigned transfer[/b] can be completed when a player's birthplace is not from one of the 14 assigned IIHF Federations, see rule 5.f.
An [b][color=#f012be]assigned transfer[/color][/b] can be completed in three cases:
[list=1]
[*]a player is not considered locked in to their IIHF Federation (they have not dressed for their IIHF nation at 426 or more applied TPE), see rule 5.a and 5.g.
[*]an active locked-in skater does not play in 2 consecutive seasons, they may apply to the IIHF Commissioners to be eligible for a transfer once per career, see rule 5.h.
[*]an active backup goalie does not play at least 6 games over the course of 2 seasons (can play 5 games 1 season and 1 the other), see rule 5.i.
[/list]To complete a transfer both the Federation Head and transferring player must post in this thread confirming the transfer. Please state the Federation the player is transferring from (when applicable) as well as their 'new' Federation.

[b]Note that:[/b]
[b][color=#ff4136]- SMJHL rookies may be contacted to join a team only after the first sim of the SMJHL playoffs.[/color][/b]
- Rookies, if you feel like this has happened, reach out to IIHF Commissioner or HO. 
- Federations may have unlimited unassigned player transfers.
- A Federation may not transfer more than 1 player from the same Federation in a season
  
  
[b]Special note to uncapped players:[/b]

[i]Remember that accepting a transfer to another Federation is permanent and that you will be unable to transfer back for the remainder of your career. Therefore consider the decision carefully as it may be worth it to wait an extra season to be able to represent your home Federation. Before accepting a transfer, talk to your current Federation Head.[/i]

[b]Transfers In Limits ([color=#f012be]assigned[/color][/b][b]/allotted)[/b][b]:[/b]\n",
          paste0(
            ":", limits$`IIHF NATION` %>% tolower() %>% str_remove_all(pattern = " ") %>% str_replace_all(pattern = "greatbritain", "uk"), ": (0/", limits$`Transfer In`, ") \n\n", collapse = ""
          ),
          "[b]Transfer Out Limits ([color=#f012be]assigned[/color]/allotted):[/b]\n",
          paste0(
            ":", limits$`IIHF NATION` %>% tolower() %>% str_remove_all(pattern = " ") %>% str_replace_all(pattern = "greatbritain", "uk"), ": (0/", limits$`Transfer Out`, ") \n\n", collapse = ""
          )
        ) 
        
      })
      
      output$discord <- renderText({
        ## For Discord
        paste(
          "**Transfer In Slots**:\n",
          paste0(
            ":", limits$`IIHF NATION` %>% tolower() %>% str_remove_all(pattern = " "), ": ", limits$`Transfer In`, "\n", collapse = ""
          ),
          "**Transfer Out Slots**:\n",
          paste0(
            ":", limits$`IIHF NATION` %>% tolower() %>% str_remove_all(pattern = " "), ": ", limits$`Transfer Out`, "\n", collapse = ""
          )
        ) %>% str_replace_all(pattern = "japan", replacement = "japan~1")
      })
        
    }
  )
}

