
#################################################################
##                         Roster type                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-04-07                    ##
#################################################################

### Loaded required packages
require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(plotly)

### Loads the API Setup with functions
source("scripts/api_setup.R")


### Loads and merges player and goalie data
smjhl <- playerLoader(league = 1) %>% 
  do.call(
    what = plyr::rbind.fill,
    args = .
  ) %>% 
  mutate(
    position = 
      case_when(
        position %in% c("RD", "LD") ~ "D",
        position == "G" ~ "G",
        TRUE ~ "F"
      ) 
  ) %>% 
  group_by(
    team,
    position
  ) %>% 
  summarize(
    n = n(), 
    avgTPE = mean(usedTPE)
  ) %>% 
  mutate(
    position = factor(position, levels = c("G", "D", "F"))
  ) %>% 
  left_join(
    teamInfo,
    by = c("team" = "abbr")
  )

ggplot(smjhl) + 
  aes(x = position, 
      y = avgTPE, 
      group = team,
      fill = team) + 
  geom_bar(stat = "identity",
           color = "black") +
  facet_wrap(facets = vars(team)) + 
  scale_fill_manual("Number of\nPlayers", values = smjhl$primary %>% unique()) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor.y = element_line(color = "gray75"),
    axis.title.y = 
      element_text(
        angle = 0,
        vjust = 0.5
        ),
    axis.text = element_text(size = 11),
    legend.position = "none"
    ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 500)
    ) + 
  geom_segment(
    aes(
      x = 0.5, 
      xend = 3.5, 
      y = 425, 
      yend = 425
      ), 
    color = "black",
    size = 1
    ) +
  labs(
    y = "Average\nTPE\n(capped)", 
    x = "Position",
    caption = 
      paste(
        "Data scraped from API:",
        lubridate::today()
      )
  )




