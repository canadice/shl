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

url <- "https://simulationhockey.com/showthread.php?tid=138043"

mockDrafts <- 
  url %>% 
  c(
    .,
    paste(
      .,
      paste("&page=", 2:25, sep = ""),
      sep = ""
    )
  )

draftees <- 
  forumData %>% 
  filter(
    CLASS == "S79"
  )

combiner <- function(x){
  cleanData <- character(32)
  
  j <- 1
  
  for(i in 1:(length(x)-1)){
    if(str_detect(x[i], pattern = "[0-9]\\.") & !str_detect(x[i+1], pattern = "[0-9]\\.")){
      cleanData[j] <- 
        stringi::stri_trans_general(
          x[i+1],
          id = "Latin-ASCII"
        )
      
      j <- j + 1
      
    } else if(str_detect(x[i], pattern = "[0-9]\\.") & str_detect(x[i+1], pattern = "[0-9]\\.")){
      j <- j + 1
    } else {
      #NOTHING
    }
  }
  
  return(cleanData[1:32])
}

parser <- function(x){
  data <- 
    x %>% 
    rvest::html_nodes("div#two") %>% 
    rvest::html_nodes(".post_body") %>% 
    rvest::html_text() %>% 
    lapply(
      X = .,
      FUN = 
        function(x){
          stringr::str_extract_all(
            string = x,
            pattern = 
              paste(
                paste0(1:32, "\\.", collapse = "|"), 
                paste0(draftees$NAME %>% str_replace_all(pattern = "\\|", replacement = "\\\\|"), collapse = "|"),
                paste0(draftees$CLEAN_NAME, collapse = "|"),
                sep = "|"
              ),
            simplify = TRUE
          ) %>% 
            stri_remove_empty_na()
        }
    )
  
  containMocks <- lapply(
    X = data,
    FUN = function(x){
      length(x) > 10
    }
  ) %>% 
    unlist() %>% which()
  
  data[containMocks] %>% 
    lapply(
      X = .,
      FUN = combiner
    ) %>%
    do.call(what = rbind, args = .) %>% 
    as_tibble() %>% 
    mutate(
      user = x %>% html_elements(".profile-username a") %>% html_text() %>% .[containMocks]
    )
}

data <- 
  lapply(
    X = mockDrafts,
    FUN = xml2::read_html
  ) %>% 
  lapply(
    X = .,
    FUN = parser
  ) %>% 
  do.call(what = rbind, args = .) %>% 
  as.data.frame() %>% 
  unique()

colnames(data) <- 1:32
  
tables <- 
  apply(
    X = data, 
    MARGIN = 2, 
    FUN = function(x) {table(x) %>% prop.table() %>% as.data.frame() %>% arrange(Freq) %>% slice_tail(n = 3)}, 
    simplify = TRUE)

pickFreq <- 
  data.frame(
    Pick = rep(names(tables), sapply(tables, nrow)) %>% as.numeric(),
    Obs = do.call(rbind,tables)
  ) %>% 
  dplyr::rename(
    Name = Obs.x
  ) %>% 
  mutate(
    Name = if_else(Name == "", "EMPTY", Name %>% as.character())
  )

popular <- 
  pickFreq %>% 
  filter(!( Pick %>% is.na())) %>% 
  group_by(Pick) %>% 
  filter(Obs.Freq == max(Obs.Freq))

paste(popular$Pick, ". ", popular$Name, " (", (popular$Obs.Freq*100) %>% round(1), "%)", sep = "")  %>% 
  paste(collapse = "\n") %>% cat()


ggplot(pickFreq %>% filter(! (Pick %>% is.na()))) + 
  aes(
    x = Name,
    y = Obs.Freq) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(vars(Pick), scales = "free_x", ncol = 8) + 
  labs(x = "Player", y = "Top 3\nPlayers\n\nPercentage\nof\nMocks") +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.8)) + 
  scale_y_continuous(labels = scales::percent)

# 
# top10 <- googlesheets4::read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/1PW1KCjvd6jVW3fu3rbPu-xWCg4MJjYPX-iLqtTLm6gs/edit#gid=1571980039"
# ) %>% 
#   select(
#     contains("Pick a player")
#   )
# 
# colnames(top10) <- 1:10
# 
# tables <- 
#   apply(
#     X = top10, 
#     MARGIN = 2, 
#     FUN = function(x) {table(x) %>% prop.table() %>% as.data.frame() %>% arrange(Freq) %>% slice_tail(n = 3)}, 
#     simplify = TRUE)
# 
# pickFreq <- 
#   data.frame(
#     Pick = rep(names(tables), sapply(tables, nrow)) %>% as.numeric(),
#     Obs = do.call(rbind,tables)
#   ) %>% 
#   dplyr::rename(
#     Name = Obs.x
#   ) %>% 
#   mutate(
#     Name = if_else(Name == "", "EMPTY", Name %>% as.character())
#   )
# 
# 
# ggplot(pickFreq) + 
#   aes(
#     x = Name,
#     y = Obs.Freq) + 
#   geom_bar(stat = "identity", position = "stack") + 
#   facet_wrap(vars(Pick), scales = "free_x") + 
#   labs(x = "Player", y = "Top 3\nPlayers\n\nPercentage\nof\nMocks") +
#   theme(axis.text.x = element_text(angle = 20, hjust = 0.8)) + 
#   scale_y_continuous(labels = scales::percent)
# #   
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









  
