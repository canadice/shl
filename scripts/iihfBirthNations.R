require(maps)

countries <- map("world", plot = FALSE)$names
countries <- 
  append(
    countries, 
    c(
      "Japan",
      "United States",
      "New Zealand",
      "Great Britain",
      "Korea",
      "Ireland",
      "Czechia"
    )
  )

forumData$BirthNation <- 
  forumData$Birthplace %>% 
  str_extract_all(
    pattern = 
      paste(countries, collapse = "|"), 
    simplify = TRUE
    )

forumData <- 
  forumData %>% 
  relocate(
    BirthNation,
    .after = Birthplace
  )

table(
  forumData$BirthNation[,1]
) %>% 
  sort()

