require(maps)

countries <- maps::map("world", plot = FALSE)$names
countries <- 
  append(
    c(
      "Montreal",
      "Montréal",
      "Québec",
      "QC",
      "Japan",
      "United States",
      "New Zealand",
      "Great Britain",
      "Korea",
      "Ireland",
      "Czechia"
    ),
    countries
  )

forumData$NATIONALITY <- 
  forumData$BIRTHPLACE %>% 
  # str_split(",") %>% 
  # lapply(
  #   FUN = last
  # ) %>% 
  # unlist() %>% 
  str_trim() %>% 
  str_extract_all(
    pattern = 
      paste(countries, collapse = "|"), 
    simplify = TRUE
    )

forumData <- 
  forumData %>% 
  relocate(
    NATIONALITY,
    .after = BIRTHPLACE
  )

tibble(NATIONALITY = forumData$NATIONALITY[,1]) %>% 
  group_by(NATIONALITY) %>% 
  summarize(n = n()) %>% 
  arrange(n %>% desc()) %>% 
  print(n = 22)


forumData$QC <- 
  forumData$BIRTHPLACE %>% 
  str_trim() %>% 
  str_detect(
    pattern = 
      paste(c("Québec", "QC", "Montreal", "Montebello", "L'Anse-Saint-Jean", "Beauceville", "Coaticook", "Drummondville", "Quebec", "Montréal", "Gatineau", "Granby", "Laval", "Normandin", "Rouyn-Noranda", "Saint-Jérôme", "Shawinigan", "Trois Rivieres"), collapse = "|")
  )

temp <- forumData %>% select(NAME, USER, BIRTHPLACE, `IIHF NATION`, QC, ACTIVE, TPE) %>% filter(QC)
write.csv(temp, file = "Québec Born Players.csv", row.names = FALSE)
