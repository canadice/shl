
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)

seasonPrediction <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1JzZ8-KC4MhJVH5NdJE7olYO2-OBkPJo_sNV7mtX7-kI/edit?gid=160432151#gid=160432151"
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

wes <- 
  seasonPrediction %>% 
  select(
    contains("Western")
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()


eas <- 
  seasonPrediction %>% 
  select(
    contains(" East")
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  mutate(
    name = name %>% str_extract_all("\\[[^()]+\\]") %>% str_remove_all(pattern = "\\[|\\]"),
    value = value %>% as.factor()
  ) %>% 
  visPredictions()

cowplot::plot_grid(wes, eas, ncol = 2)
