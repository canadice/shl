require(shlrtools)
require(arsenal)
require(dplyr)
require(dbplyr)
require(DBI)
require(RSQLite)
require(ggplot2)
require(plotly)
require(reactable)

seasonELO <- function(season, league = 0, old_elo = NULL){
  
  leagueId <- league
  
  con <- dbConnect(SQLite(), "database/SHL_Database.db")
  
  teamData <-
    tbl(con, "teamInfo") %>%
    filter(
      leagueID == leagueId
    ) %>% 
    collect()
  
  dbDisconnect(con)
  
  schedule <- 
    shlrtools::gamesLoader(leagueID = leagueId, season = season, type = "regular") %>% 
    filter(
      type == "Regular Season",
      played == 1
    ) %>% 
    arrange(
      date
    ) %>% 
    mutate(
      homePoints = 
        case_when(
          homeScore > awayScore ~ 2,
          homeScore < awayScore & overtime == 1 ~ 1,
          TRUE ~ 0
        ),
      awayPoints = 
        case_when(
          homeScore < awayScore ~ 2,
          homeScore > awayScore & overtime == 1 ~ 1,
          TRUE ~ 0
        )
    ) 
  
  if(season == 53){
    elo <- 
      teamData %>%
      select(
        team,
        fhmID,
        Inaugural.Season
      ) %>%
      filter(
        team != "FA"
      ) %>%
      mutate(
        time = 0,
        ELO = ifelse( Inaugural.Season == season | season == 53, 1000, NA)
      )
  } else {
    elo <-
      old_elo %>%
      mutate(
        time = ifelse(Inaugural.Season == season & fhmID != 5, max(time), time)
      ) %>%
      mutate(
        ELO = ifelse(Inaugural.Season == season & time == max(time) & fhmID != 5, 1000, ELO)
      )
  }
  
  curSchedule <- 
    schedule 
  
  for(i in 1:nrow(curSchedule)){
  # for(i in 1:6){
    currentRank <- 
      elo %>%
      filter(
        fhmID %in% c(curSchedule$homeTeam[i], curSchedule$awayTeam[i])
      ) %>% 
      group_by(
        team
      ) %>% 
      filter(
        time == max(time)
      ) %>% 
      ungroup() %>% 
      mutate(
        fhmID = 
          factor(fhmID, 
                 levels = 
                   c(curSchedule$homeTeam[i], 
                     curSchedule$awayTeam[i]))
      ) %>% 
      arrange(
        fhmID
      ) %>% 
      select(
        ELO
      ) %>% 
      unlist()
    
    ## Calculating the expected score
    Q <- 10^(currentRank/480)

    ## Home team wins generally 57.5 percent of the time
    E <- Q / sum(Q) * c(1.15, 0.85)
    
    ## Calculating the observed score
    S <- curSchedule[i,c("homePoints", "awayPoints")] / 
      sum(curSchedule[i,c("homePoints", "awayPoints")])
    
    ## Calculates new rank
    K <- NA
    
    constantK <- c(12,20,28)
    
    if(currentRank[1] < 1200){
      K[1]  <- constantK[1] 
    } else if (currentRank[1] > 1400){
      K[1] <- constantK[3]
    } else {
      K[1] <- constantK[2]
    }
    
    if(currentRank[2] < 1200){
      K[2]  <- constantK[1] 
    } else if (currentRank[2] > 1400){
      K[2] <- constantK[3] 
    } else {
      K[2] <- constantK[2] 
    }
    
    newRank <- (currentRank + K * (S - E)) %>% unname()
    
    ## Summarizes the new data
    newData <- 
      data.frame(
        team = elo %>% filter(fhmID == curSchedule$homeTeam[i]) %>% select(team) %>% unique() %>% unlist(),
        fhmID = curSchedule$homeTeam[i],
        time = 
          elo %>% 
          filter(fhmID == curSchedule$homeTeam[i]) %>% 
          filter(time == max(time)) %>% 
          select(time) %>% 
          unlist() + 1,
        ELO = newRank[1]
      ) %>% 
      plyr::rbind.fill(
        data.frame(
          team = elo %>% filter(fhmID == curSchedule$awayTeam[i]) %>% select(team) %>% unique() %>% unlist(),
          fhmID = curSchedule$awayTeam[i],
          time = 
            elo %>% 
            filter(fhmID == curSchedule$awayTeam[i]) %>% 
            filter(time == max(time)) %>% 
            select(time) %>% 
            unlist() + 1,
          ELO = newRank[2]
        )
      ) %>% 
      mutate(
        Inaugural.Season = season
      )
    
    elo <- 
      elo %>% 
      plyr::rbind.fill(
        newData
      )
  }
  
  return(elo)
}
visualizeEloSMJHL <- function(ELO){
  con <- dbConnect(SQLite(), "database/SHL_Database.db")
  
  teamData <-
    tbl(con, "teamInfo") %>%
    collect() %>% 
    filter(
      team %in% ELO$team
    )
  
  dbDisconnect(con)
  
  ELO <- 
    ELO %>% 
    filter(!is.na(fhmID))
  
  seasons <- 
    data.frame(
      split = c(0, seq(51, 201, by = 50), seq(267, max(ELO$time), by = 66)),
      season = ELO %>% 
        filter(
          Inaugural.Season >= 53
        ) %>% 
        .$Inaugural.Season %>% 
        sort() %>% 
        unique()
    )
  
  p <- 
    ggplot(data = ELO) + aes(x = time, y = ELO, color = team) + 
    geom_line(linewidth = 1) + theme_bw() +
    scale_color_manual(
      "Team",
      values = 
        teamData %>% 
        filter(
          !is.na(fhmID)
        ) %>% 
        arrange(
          tolower(team)
        ) %>% 
        select(
          primary
        ) %>% 
        unlist() %>% 
        unname()
    ) +
    labs(
      x = "Time"
    ) + 
    geom_vline(
      xintercept = seasons$split
    ) +
    # geom_label(
    #   aes(
    #     x = split,
    #     y = 1200,
    #     label = season
    #   ),
    #   data = seasons
    # ) +
    scale_x_continuous(
      breaks = NULL
    ) + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) 
  
  p %>% 
    ggplotly() %>% 
    print()
  
  ELO %>% 
    group_by(team) %>% 
    filter(time == max(time), !is.na(fhmID)) %>% 
    ungroup() %>% 
    arrange(desc(ELO)) %>% 
    select(-time, -Inaugural.Season) %>% 
    mutate(
      ELO = round(ELO, 0)
    ) %>% 
    rename(Team = team) %>% 
    reactable::reactable(
      pagination = FALSE,
      fullWidth = FALSE,
      columns = 
        list(
          Team = 
            reactable::colDef(
              width = 150
            )  
        )
    ) %>% 
    print()
  
}

eloSMJHL <- seasonELO(season = 53, league = 1)
# eloSMJHL <- seasonELO(season = 55, league = 1, old_elo = eloSMJHL)
for (i in 54:71) {
  eloSMJHL <- seasonELO(season = i, league = 1, old_elo = eloSMJHL)  
}


save(eloSMJHL, file = "ELO.RData")

load("ELO.RData")
visualizeEloSMJHL(eloSMJHL)



