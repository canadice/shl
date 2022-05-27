require(rvest)
## Data processing
require(tidyr)
require(tibble)
require(dplyr)
require(stringr)
## Loading package that can talk to Google Sheets
require(googlesheets4)

url <- "https://simulationhockey.com/showthread.php?tid=45875"

### Reads the information
topic <- xml2::read_html(url)

content <- 
  topic %>% 
  rvest::html_nodes(".post_body") %>% 
  dplyr::nth(1) %>% 
  rvest::html_text2() %>% 
  stringr::str_split(pattern = "\n") %>% 
  unlist() %>% 
  stringr::str_squish() %>% 
  .[6:106] %>% 
  stringr::str_split(
    pattern = " - | -|- ",
    simplify = TRUE
  ) %>% 
  as_tibble() %>% 
  mutate(
    V2 = str_squish(V2),
    # V3 = str_squish(V3)
  )
  

googlesheets4::gs4_auth(path = ".secrets/client_secret.json")

write_sheet(
  data = content,
  ss = "https://docs.google.com/spreadsheets/d/1i6yWbsI3MO036E_4y95xqZFolsUn3oT5Sq6Jju51CSQ/edit#gid=2095187067",
  sheet = "draftResult"
)








