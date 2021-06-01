
############################################################################
############################################################################
###                                                                      ###
###                      VISUALIZING PLAYER RATINGS                      ###
###                                                                      ###
############################################################################
############################################################################

require(plotly)

data <- 
  allPlayers %>% 
  select(
    NAME,
    POSITION,
    aggression:defensiveRead
  ) %>%
  filter(
    !(NAME %in% 
        c(
          "Mattias Zetterstrom", "Harald Malmquist", 
          "Freyja Hellström", "Fredrik Stefansson",
          "Benjamin Jansson"))
  ) 
  
visualize <- function(player){
  data  %>% 
    filter(
      NAME == player
    ) %>% 
    pivot_longer(
      aggression:defensiveRead,
      names_to = "Attribute",
      values_to = "Rating"
    ) %>%
    mutate(
      Attribute =
        factor(
          Attribute,
          levels = c("screening", "gettingOpen", 'passing', 'puckhandling', 'shootingAccuracy', 'shootingRange', 'offensiveRead',
                     'checking', 'hitting', 'positioning', 'stickChecking', 'shotblocking', 'faceoffs', 'defensiveRead',
                     'acceleration', 'agility', 'speed', 'stamina', 'strength', 'balance', 'fighting',
                     'aggression', 'bravery', 'determination', 'teamPlayer', 'leadership', 'temperament', 'professionalism')
        ),
      text = 
        paste(
          Attribute %>% 
            toupper(), 
          Rating, sep = ": ")
    ) %>%
    arrange(
      Attribute
    ) %>%
    plot_ly(
      type = 'scatterpolar',
      mode = "markers",
      r = ~Rating,
      theta = ~Attribute,
      text = ~text,
      fill = 'toself',
      hoverinfo = "text",
      color = I("#cf5b00"),
      name = ~NAME,
      width = 500,
      height = 400
    ) %>%
    config(
      modeBarButtonsToRemove =
        c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian", "toggleSpikelines"
        )
    ) %>%
    layout(
      autosize = FALSE,
      polar =
        list(
          radialaxis =
            list(
              visible = TRUE,
              range = c(0,20)
            )
        ),
      ## Legend is put to false so the plot is the same size
      showlegend = FALSE
    )
}

visualize("Tomas Lind")
    
  
team <- 
  data %>% 
  group_by(
    POSITION
  ) %>% 
  summarize(
    across(aggression:defensiveRead, ~ mean(.x) %>% round(2))
  )



