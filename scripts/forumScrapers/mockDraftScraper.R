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

url <- "https://simulationhockey.com/showthread.php?tid=120950"

mockDrafts <- 
  url %>% 
  c(
    .,
    paste(
      .,
      paste("&page=", 2:11, sep = ""),
      sep = ""
    )
  )

draftees <- 
  forumData %>% 
  filter(
    CLASS == "S63"
  )

combiner <- function(x){
  cleanData <- character(20)
  
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
  
  return(cleanData[1:20])
}

parser <- function(x){
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
                paste0(1:24, "\\.", collapse = "|"), 
                paste0(draftees$NAME, collapse = "|"),
                paste0(draftees$clean_name, collapse = "|"),
                sep = "|"
              ),
            simplify = TRUE
          )
        }
    ) %>% 
    .[
      lapply(
        X = .,
        FUN = function(x){
          ncol(x) > 10
        }
      ) %>% 
        unlist()
    ] %>% 
    lapply(
      X = .,
      FUN = combiner
    ) %>% 
    do.call(what = rbind, args = .)
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
  as.data.frame()

colnames(data) <- 1:20
  
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
  rename(
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
  labs(x = "Player", y = "Top 3\nPlayers\n\nPercentage\nof\nMocks") +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.8)) + 
  scale_y_continuous(labels = scales::percent)
  

myplots <- lapply(split(pickFreq,pickFreq$Pick), function(x){
  x$Name <- factor(x$Name, levels=x$Name[order(-x$Obs.Freq,decreasing=F)])
  
  p <- ggplot(x, aes(x = Name, y = Obs.Freq, width=0.75)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "") + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.75)) + 
    theme(
      axis.text.x = element_text(angle = 20, hjust = 0.8),
      plot.margin = margin(0.25, 0.5, -3, 0, "cm")
    )
})

library(cowplot)

do.call(
  plot_grid,
  c(
    myplots,
    align = "vh",
    ncol = 5,
    rel_heights = 0.25
  )
) + 
  draw_plot_label(
    label = 1:20
  )









  
