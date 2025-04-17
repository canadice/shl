library(backports)     # to revive the isFALSE() function for sim_slopes()
library(effects)       # for probing interactions
library(ggplot2)       # for data visualization
library(interactions)  # for probing/plotting interactions
library(lme4)          # for multilevel models
library(lmerTest)      # for p-values
library(psych)         # for describing the data
library(plyr)          # for data manipulation
require(dplyr)
require(shlrtools)


season <- 66

fhm8 <- 
  indStatsLoader(leagueID = 0, season = season)$player %>%
  do.call(what = data.frame, args = .) %>% 
  left_join(
    playerLoader(leagueID = 0, season = season)$player %>% 
      select(
        -(id:season),
        -(team:position)
      ),
    by = "name"
  ) %>%
  rbind.fill(
    indStatsLoader(leagueID = 0, season = season+1)$player %>% 
      do.call(what = data.frame, args = .)  %>% 
      left_join(
        playerLoader(leagueID = 0, season = season+1)$player %>% 
          select(
            -(id:season),
            -(team:position)
          ),
        by = "name"
      ) 
  ) %>% 
  mutate(
    positionGroup = 
      case_when(
        position %in% c("C", "RW", "LW") ~ "Forward",
        TRUE ~ "Defense"
        ),
    engine = "FHM8"
  )


fhm6 <- 
  indStatsLoader(leagueID = 0, season = season-1)$player %>%
  do.call(what = data.frame, args = .) %>% 
  left_join(
    playerLoader(leagueID = 0, season = season-1)$player %>% 
      select(
        -(id:season),
        -(team:position)
      ),
    by = "name"
  ) %>%
  rbind.fill(
    indStatsLoader(leagueID = 0, season = season-2)$player %>% 
      do.call(what = data.frame, args = .)  %>% 
      left_join(
        playerLoader(leagueID = 0, season = season-2)$player %>% 
          select(
            -(id:season),
            -(team:position)
          ),
        by = "name"
      ) 
  ) %>% 
  mutate(
    positionGroup = 
      case_when(
        position %in% c("C", "RW", "LW") ~ "Forward",
        TRUE ~ "Defense"
      ),
    engine = "FHM6"
  )

fullData <- 
  rbind.fill(
    fhm6,
    fhm8
  ) %>% 
  filter(
    appliedTPE > 500
  ) %>% 
  group_by(
    positionGroup,
    shootingAccuracy,
    engine
  ) %>% 
  mutate(
    n = n()
  )

ggplot(fullData) + aes(x = appliedTPE, y = points, color = engine) + 
  geom_point(size = 2, alpha = 0.5) + theme_bw() + 
  geom_smooth(method = "lm", se = FALSE, size = 2) + 
  facet_wrap(~ positionGroup) + 
  scale_y_continuous(limits = c(0, 110)) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual("ENGINE", values = c("dark orange", "black")) 

ggplot(fullData) + 
  aes(x = shootingAccuracy %>% as.factor(), y = (goals/shotsOnGoal)*100, fill = engine) + 
  geom_boxplot() + 
  labs(y = "Shooting Percentage (%)")+
  facet_wrap(positionGroup ~ engine) + 
  theme_bw() + 
  labs(x = "Shooting Accuracy") + 
  scale_fill_manual("ENGINE", values = c("dark orange", "black"))

ggplot(fullData %>% filter(engine == "FHM8")) + 
  aes(x = appliedTPE, fill = positionGroup, label = n) +
  geom_histogram() + 
  geom_text(aes(x = 1200, y = 25))+
  theme_bw() + 
  facet_wrap(positionGroup ~ shootingAccuracy %>% as.factor()) + 
  scale_fill_manual("Position", values = c("dark orange", "black")) 


modelData <- 
  fullData %>% 
  select(
    name,
    goals,
    assists,
    points,
    appliedTPE,
    positionGroup,
    engine,
    screening:bravery
  ) %>% 
  ungroup()

model <- 
  lm(
    points ~ appliedTPE*positionGroup,
    data = modelData
  )

options(scipen = 3)

summary(model)

resid(model) %>% plot()

model <- 
  lm(
    points ~ .,
    data = 
      modelData %>% 
      ungroup() %>% 
      select(
        points,
        screening:bravery
      )
  )

baseCoef <- coefficients(model)

model <- 
  lm(
    points ~ .,
    data = 
      modelData %>% 
      ungroup() %>% 
      select(
        points,
        screening:bravery,
        engine
      )
  )

confoundCoef <- coefficients(model)

baseCoef - confoundCoef[-25]

model <- 
  lm(
    points ~ .,
    data = 
      modelData %>% 
      ungroup() %>% 
      select(
        points,
        screening:bravery,
        engine,
        positionGroup
      )
  )

counfound2Coef <- coefficients(model)

baseCoef - confoundCoef[-c(25, 26)]

formula <- 
  as.formula(
    paste(
      "points ~ (",
      paste(modelData %>% select(screening:bravery) %>% colnames(), collapse = "+"),
      ")^2"
    )
  )

modelData <- 
  modelData %>% 
  mutate(
    appliedTPE_c = scale(appliedTPE, scale = FALSE),
    appliedTPE_std = scale(appliedTPE)
  ) %>% 
  mutate(
    pos_engine = paste(engine, positionGroup, sep = "-")
  ) %>% 
  group_by(name) %>% 
  mutate(
    t = 1:n()
  ) %>% 
  ungroup()

## Random intercept based solely on name

modelIntercept <- 
  lmer(
    formula = points ~ 1 + 
      (1|name),
    data = 
      modelData
  )

summary(modelIntercept)

plot(modelIntercept)

## Adding on fixed slopes

modelSlope <- 
  lmer(
    formula = points ~ 1 + appliedTPE_std +
      (1|name),
    data = modelData
  )

summary(modelSlope)

plot(modelSlope)

## Adding on random slopes

modelSlopeR <- 
  lmer(
    formula = points ~ 1 + appliedTPE_std +
      (1 + appliedTPE_std|name),
    data = modelData
  )

summary(modelSlopeR)

plot(modelSlopeR)

## Adding engine
model4 <- 
  lmer(
    formula = points ~ 1 + appliedTPE_std + engine + 
      (1 + appliedTPE_std|name), 
    data = modelData
  )

summary(model4)

plot(model4)
hist(resid(model4))

## Adding position
model5 <- 
  lmer(
    formula = points ~ 1 + (appliedTPE_std + engine) * positionGroup + 
      (1 + appliedTPE_std|name), 
    data = modelData
  )

summary(model5)

plot(model5)
qqnorm(resid(model5))
hist(resid(model5))

## Adding time
model6 <- 
  lmer(
    formula = points ~ 1 + (appliedTPE_std + engine) * positionGroup + t + 
      (1 + appliedTPE_std|name), 
    data = modelData
  )

summary(model6)

plot(model6)
qqnorm(resid(model6))
hist(resid(model6))


