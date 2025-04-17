
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)

seasonPrediction <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/18qd7gnJYqRSxYQz2rDphOs4tc1b0CM5-uujqQ9QCBB0/edit?usp=sharing"
  )

visPredictions <- function(data){
  p <- 
    ggplot(data) + aes(x = value) + 
    geom_bar() + 
    facet_grid(
      rows = vars(name)
    ) + 
    theme_bw() + 
    scale_y_continuous(
      expand = expansion(add = 1),
      breaks = seq(0, 300, 100)
    ) + 
    theme(
      panel.spacing.y = unit(1, "lines"),
      strip.background.y = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 0, size = 12), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray60"),
      panel.background = element_rect(fill = "transparent"), 
      axis.line.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      plot.background = element_rect(fill = "transparent", color = "black")
    ) + 
    labs(
      x = "Placement",
      y = "Count"
    )
  
  print(p)
}

pac <- 
  seasonPrediction %>% 
  select(
    contains("Pacific")
  ) %>% 
  pivot_longer(
    cols= everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()


cen <- 
  seasonPrediction %>% 
  select(
    contains("Central")
  ) %>% 
  pivot_longer(
    cols= everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()

ne <- 
  seasonPrediction %>% 
  select(
    contains("North East")
  ) %>% 
  pivot_longer(
    cols= everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()

atl <- 
  seasonPrediction %>% 
  select(
    contains("Atlantic")
  ) %>% 
  pivot_longer(
    cols= everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()

cowplot::plot_grid(pac, cen, ne, atl, ncol = 2)