#### Functions ####
point_calculator <- function(data, teamInfo, startDate, endDate, eliminationData)
{
  currentStandings <- 
    data %>% 
    filter(date <= endDate & date >= startDate) 
  
  if(nrow(currentStandings) > 0){
    # Creates current standings
    currentStandings <- 
      currentStandings %>%
      mutate(
        awayPoints = 
          case_when(
            awayScore > homeScore ~ 2,
            awayScore > homeScore & (overtime == 1 | shootout == 1) ~ 1,
            TRUE ~ 0),
        homePoints = 
          case_when(
            awayScore < homeScore ~ 2,
            awayScore < homeScore & (overtime == 1 | shootout == 1) ~ 1,
            TRUE ~ 0),
        awayROW =
          if_else(awayPoints == 2 & !(overtime == 1 | shootout == 1), 1, 0),
        homeROW =
          if_else(homePoints == 2 & !(overtime == 1 | shootout == 1), 1, 0)
      ) 
        
    
    away <- currentStandings %>%
      group_by(awayTeam) %>%
      summarise(
        points = sum(awayPoints), 
        gf = sum(awayScore, na.rm = TRUE), 
        ga = sum(homeScore, na.rm = TRUE), 
        gp = length(homeScore),
        ROW = sum(awayROW)
      ) %>%
      rename(team = awayTeam)
    
    home <- currentStandings %>%
      group_by(homeTeam) %>%
      summarise(
        points = sum(homePoints), 
        gf = sum(homeScore, na.rm = TRUE), 
        ga = sum(awayScore, na.rm = TRUE), 
        gp = length(homeScore),
        ROW = sum(homeROW)
      ) %>%
      rename(team = homeTeam)
    
    standings <- 
      rbind(
        away,
        home
      ) %>% 
      group_by(team) %>% 
      summarize(
        across(
          everything(),
          ~ sum(.x, na.rm = TRUE)
        )
      ) %>% 
      ungroup() %>% 
      mutate(
        maxPotPts = points + (66 - gp)*2,
        gDiff = gf - ga
      ) %>% 
      left_join(
        teamInfo,
        by = c("team" = "id")
      ) %>% 
      group_by(conference, division) %>% 
      mutate(
        divRank = frank(data.frame(-points, -ROW, -gDiff))
      ) %>% 
      ungroup() %>% 
      group_by(conference) %>% 
      mutate(
        wcRank = case_when(divRank < 4 ~ 99, TRUE ~ frank(data.frame(-points, -ROW, -gDiff))) %>% rank(),
        playoffTeam = if_else(divRank < 4 | wcRank < 3, TRUE, FALSE),
        eliminated = if_else(maxPotPts >= min(points[playoffTeam]), FALSE, TRUE)
      ) %>% 
      ungroup()
      
    
    eliminationData <- 
      data.frame(
        apply(
          eliminationData, 
          MARGIN = 1, 
          FUN = function(x){
            current <- which(standings$team %in% x["team"])
            if(!is.na(x["date"])){
              x
            } else if(length(current) == 0){
              c(x["team"], NA, NA, NA, NA, NA)
            } else if(standings$eliminated[current]){
              c(x["team"], standings$gp[current], standings$points[current], standings$ROW[current], standings$gDiff[current], endDate %>% as_date())
            } else {
              x
            }
          }) %>% 
          t(),
        stringsAsFactors = FALSE
        )
    
    colnames(eliminationData) <- c("team", "gp" ,"points", "ROW", "gDiff", "date")
  } else {
    
  } 
  return(list(standings = standings, elimination = eliminationData))
}

create <- function(schedule, teamInfo)
{
  eliminatedAt <- 
    list(
      standings = NA,
      elimination = 
        data.frame(
          team = unique(c(schedule$awayTeam, schedule$homeTeam)) %>% as.numeric(), 
          gp = NA, 
          points = NA, 
          ROW = NA,
          gDiff = NA, 
          date = NA, 
          stringsAsFactors = FALSE
        )
    )
    
  
  for(i in schedule$date){
    eliminatedAt <- 
      point_calculator(
        data = schedule, 
        teamInfo = teamInfo,
        startDate = min(schedule$date), 
        endDate = i, 
        eliminationData = eliminatedAt$elimination)
  }
  
  standingsAfterElimination <- 
    full_join(
      eliminatedAt$standings,
      eliminatedAt$elimination %>% 
        mutate(
          team = as.numeric(team)
        ),
      by = "team",
      suffix = c(" current", " elimination")
    ) %>% 
    mutate(
      across(
        c(`gp elimination`:`gDiff elimination`),
        ~ as.numeric(.x)
      )
    ) %>% 
    select(
      name,
      conference,
      division,
      GP = `gp current`,
      P = `points current`,
      gf,
      ga,
      GD = `gDiff current`,
      ROW = `ROW current`,
      `GP at Elimination` = `gp elimination`,
      `GD at Elimination` = `gDiff elimination`,
      `P at Elimination` = `points elimination`,
      `ROW at Elimination` = `ROW elimination`
    ) %>% 
    mutate(
      `P after Elimination` = P - `P at Elimination`,
      `ROW after Elimination` = ROW - `ROW at Elimination`,
      `GD after Elimination` = GD - `GD at Elimination`
    ) %>% 
    arrange(
      `P after Elimination` %>% desc(),
      `ROW after Elimination` %>% desc(),
      `GD after Elimination` %>% desc(),
      conference,
      division,
      P %>% desc(),
      ROW %>% desc,
      GD %>% desc()
    )
  
  return(standingsAfterElimination)
}
