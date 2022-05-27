
#################################################################
##                         Roster type                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-04-07                    ##
#################################################################

### Loads the API Setup with functions
source("scripts/API/apiSetup.R")


### Loads and merges player and goalie data
shl <- 
  forumData %>% 
  filter(
    SHL.Team != "zzz SHL Free Agents",
    !is.na(SHL.Team)
  ) %>% 
  mutate(
    POSITION =
      case_when(
        POSITION %>% str_detect(pattern = "Defense") ~ "D",
        POSITION %>% str_detect(pattern = "Goalie") ~ "G",
        TRUE ~ "F"
      ) %>% 
      factor(
        levels = 
          c(
            "F",
            "D",
            "G"
          )
      ),
    PROSPECT = if_else(SHL.Team == team | !is.na(team), "Roster", "Prospect")
  ) %>% 
  group_by(
    SHL.Team,
    POSITION,
    PROSPECT
  ) %>% 
  summarize(
    n = n(), 
    avgTPE = mean(TPE)
  ) %>% 
  left_join(
    teamInfo %>% 
      filter(
        league == "SHL",
        !is.na(fhmID)
      ),
    by = c("SHL.Team" = "team")
  )

p <- 
  ggplot(shl) + 
    aes(x = interaction(POSITION,PROSPECT), 
        y = avgTPE, 
        group = SHL.Team,
        fill = SHL.Team) + 
    geom_bar(stat = "identity",
             color = "black") +
    facet_wrap(facets = vars(SHL.Team)) + 
    scale_fill_manual("", values = shl %>% ungroup() %>% select(SHL.Team, primary) %>% unique() %>% select(primary) %>% unlist() %>% unname()) +
    theme_bw() +
    theme(
      # strip.background = element_rect(fill = "black"),
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
    labs(
      y = "Average\nTPE\n(capped)", 
      x = "Position",
      caption = 
        paste(
          "Data taken from the SHL Forum:",
          lubridate::today()
        )
    )


g <-
  p %>% 
  ggplot_build() %>% 
  ggplot_gtable()
  
strip <- 
  which(
    grepl(
      'strip-', 
      g$layout$name
    )
  )

fills <- 
  shl %>% 
  arrange(SHL.Team) %>% 
  ungroup() %>% 
  select(primary) %>% 
  unique() %>% 
  unlist()

k <- 1
for (i in strip) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

