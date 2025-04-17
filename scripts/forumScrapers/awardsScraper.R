## Loads the API Setup
source("scripts/API/apiSetup.R")

{
  theme <- theme_bw() +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 15),
          axis.title.x = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray70"),
          panel.grid.minor.y = element_line(color = "gray80"),
          axis.text = element_text(size = 12, color = "black"))
  
  theme_set(theme)
}

sheet <- "https://docs.google.com/spreadsheets/d/1889-PNiKzMjV7bMt5rytnUNQ9ptHR4mUZSgmjTIwOLM/edit?usp=sharing"

data <- read_sheet(ss = sheet)

  
tables <- 
  apply(
    X = data[,-c(1:2, ncol(data))], 
    MARGIN = 2, 
    FUN = function(x) {table(x) %>% prop.table() %>% as.data.frame() %>% arrange(Freq) %>% slice_tail(n = 3)}, 
    simplify = TRUE)

pickFreq <- 
  data.frame(
    Pick = rep(names(tables), sapply(tables, nrow)),
    Obs = do.call(rbind,tables)
  ) %>% 
  dplyr::rename(
    Name = Obs.x
  ) %>% 
  mutate(
    Name = if_else(Name == "", "EMPTY", Name %>% as.character())
  )


ggplot(pickFreq) + 
  aes(
    x = Name,
    y = Obs.Freq) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(vars(Pick), scales = "free_x") + 
  labs(x = "Player", y = "Top 3\nPlayers\n\n%\nof\nPredictions") +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.8)) + 
  scale_y_continuous(labels = scales::percent)
#   
# 
# myplots <- lapply(split(pickFreq,pickFreq$Pick), function(x){
#   x$Name <- factor(x$Name, levels=x$Name[order(-x$Obs.Freq,decreasing=F)])
#   
#   p <- ggplot(x, aes(x = Name, y = Obs.Freq, width=0.75)) +
#     geom_bar(stat = "identity") +
#     labs(x = "", y = "") + 
#     scale_y_continuous(labels = scales::percent, limits = c(0, 0.75)) + 
#     theme(
#       axis.text.x = element_text(angle = 20, hjust = 0.8),
#       plot.margin = margin(0.25, 0.5, -3, 0, "cm")
#     )
# })
# 
# library(cowplot)
# 
# do.call(
#   plot_grid,
#   c(
#     myplots,
#     align = "vh",
#     ncol = 5,
#     rel_heights = 0.25
#   )
# ) + 
#   draw_plot_label(
#     label = 1:20
#   )









  
