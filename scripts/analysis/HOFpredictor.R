############################################################################
############################################################################
###                                                                      ###
###                   PREDICTING A HALL OF FAME CAREER                   ###
###                                                                      ###
###                          CREATED: 2021-05-10                         ###
###                        LAST EDIT: 2021-05-23                         ###
###                                                                      ###
############################################################################
############################################################################

### Loading packages

require(dplyr)
require(tidyr)
require(rvest)
require(stringr)
require(stringi)
require(fuzzyjoin)
require(rpart)
require(rpart.plot)
require(ggplot2)
require(magrittr)
require(purrr)


##---------------------------------------------------------------
##            Scraping players currently in the HoF             -
##---------------------------------------------------------------

hofPlayers <- 
  ## Taking from the HoF by draft class
  "https://simulationhockey.com/showthread.php?tid=113076" %>% 
  
  ## Reads the html
  read_html() %>% 
  
  ## Searches for the second part of the thread, i.e. the main body
  html_nodes("div#two") %>% 
  
  ## Takes the first post
  nth(1) %>% 
  
  ## Finds the body of the post
  html_nodes(".post_body") %>% 
  
  ## Converts the html to text
  html_text() %>% 
  
  ## Removes extra spaces from the text
  str_squish() %>% 
  
  ## Splits the text vector based on the draft class and number
  str_split(
    pattern = c("Season [0-9]+|HOF")
  ) %>% 
  
  ## Creates a long vector with strings
  ## All elements are from different draft classes
  unlist() %>% 
  
  ## Removes the last two strings as they come from non-players
  head(length(.)-2) %>% 
  
  ## Extract all letters and symbols used in names (-'.)
  ## As , is used to separate names, the resulting vector will have one string per name
  str_extract_all(
    pattern = "[A-z\\.\\'\\- ]+"
  ) %>% 
  unlist() %>% 
  
  ## Removes extra spaces
  str_squish() %>% 
  
  ## Removes empty elements in the vector
  stri_remove_empty() %>% 
  
  ## Converts to a data frame 
  as.data.frame() %>% 
  
  ## Renames the vector to specify their names
  rename(Name = ".") %>% 
  
  ## Adds a new variable that says that the player is in the HoF
  mutate(
    inHoF = TRUE
  ) 


##---------------------------------------------------------------
##                  Scraping awards for players                 -
##---------------------------------------------------------------

awards <- 
  ## Using the post with all awards summarized in code blocks from two posts
  "https://simulationhockey.com/showthread.php?tid=1715" %>%
  
  ## Reads html
  read_html() %>% 
  
  ## Extracts all the code blocks from all posts
  html_elements("code") %>% 
  
  ## Converts to text.
  html_text() %>% 
  
  ## Function that for every code block, extracts players and specifies which one is winner and which one is nominee
  lapply(
    X = .,
    FUN = function(x){
      x <- 
        ## Splits over all : or newline (\n)
        str_split(
          x, 
          pattern = "[\n:,]"
        ) %>% 
        
        ## Unlists only once
        unlist(recursive = FALSE) %>% 
        
        ## Indexing to only keep the winner name, and nominee names
        .[!str_detect(., pattern = "Winner|\\*")] %>% 
        
        ## Removes extra spaces
        str_squish() %>% 
        
        ## Removes empty elements
        stringi::stri_remove_empty() %>% 
        
        ## Converts to a tibble
        as_tibble() %>% 
        
        ## Adds a variable indicating if the player won or was nominated
        mutate(
          ## Winners have their team specified, ex. (MAN), nominees have not
          Winner =
            case_when(
              str_detect(value, "\\([A-Z]{2,4}") ~ "Winner",
              TRUE ~ "Nominee"
            ),
          
          ## Removes all other information from the name strings, so only the names are kept
          value = 
            str_remove_all(value, pattern = "\\([A-Z]{2,4}|\\)|\\(|Nominees") %>% 
            str_squish()
        ) %>% 
        
        ## Renames the variable to Player
        rename(
          Player = value
        )
    }
  ) %>% 
  
  ## The resulting list has all awards, the first four are team awards so are excluded
  .[5:21] %>% 
  
  ## For each award all nominees are split up to separate variables instead of being in one string
  lapply(
    X = .,
    FUN = function(x){
      
      ## Splits multiple players based on multiple ways written in the strings
      names <- 
        x$Player %>% 
        str_split(pattern = " and |&| /", simplify = TRUE)
      
      winners <- x$Winner
      
      ## Adds extra names to their respective variables
      ## Different awards have different number of nominees
      x <- 
        x %>% 
        mutate(
          Player =
            names[,1]
        )
      
      if(ncol(names) > 1){
        x <- 
          x %>% 
          add_row(
            Player =
              names[,2],
            Winner = 
              winners
          )  
      } 
      
      if(ncol(names) > 2){
        x <- 
          x %>% 
          add_row(
            Player =
              names[,3],
            Winner = 
              winners
          ) 
      }
      
      ## Cleans up everything with extra spaces and empty strings
      x %>% 
        mutate(
          Player =
            Player %>% 
            str_squish
        ) %>% 
        filter(
          Player != ""
        ) %>% 
        table()
    }
  )

### Changes names of the list to their respective rewards
names(awards) <-
  c(
    "Ron Mexico",
    "John McBride",
    "Scott Stevens",
    "Bojo Biscuit",
    "Ryan Jester",
    "Jeff Dar",
    "Aidan Richan",
    "Lance Uppercut",
    "Joe McKeil",
    "Damian Littleton",
    "Sarmad Khan",
    "Anton Razov",
    "Segei Karpotsov",
    "Jay McDonald",
    "Turd Ferguson",
    "Mike Honcho",
    "Vidrik Onoprienko"
  ) %>% 
  ## Removes the spaces for all names because a space produces errors later in the code
  str_remove(pattern = " ")

awards <- 
  awards %>% 
  lapply(
    X = seq_along(.),
    FUN = function(x, names, i){
      colnames(x[[i]]) <- 
        paste(names[i], colnames(x[[i]]), sep = "")
      
      x[[i]] %>% 
        as_tibble() %>% 
        pivot_wider(
          names_from = Winner, 
          values_from = n)
    },
    x = .,
    names = names(.)
  ) %>% 
  reduce(
    full_join,
    by = "Player"
  )

### Loading data sets
playerStatistics <- 
  read.csv2(
    "csv/history_skaters.csv",
    sep = ","
    ) %>% 
  arrange(
    Player.Name,
    Season
  ) %>% 
  stringdist_full_join(
    hofPlayers,
    by = c("Player.Name" = "Name"),
    max_dist = 1
  ) %>% 
  mutate(
    MinutesPlayed = MinutesPlayed %>% as.numeric(),
    PPMinutes = PPMinutes %>% as.numeric(),
    PKMinutes = PKMinutes %>% as.numeric()
  ) %>% 
  filter(
    Player.Name != "CPU Player",
    !(str_detect(Player.Name, pattern = "Computer")),
    LeagueId == 1
  ) 

### Aggregating career data
careerStatistics <- 
  playerStatistics %>% 
  group_by(
    Player.Name,
    isPlayoffs
  ) %>% 
  summarize(
    across(
      c(GamesPlayed:FaceoffWins, inHoF),
      sum
    )
  ) %>% 
  mutate(
    inHoF =
      case_when(
        inHoF > 0 ~ 1,
        TRUE ~ 0
      ) %>% 
      as.factor()
  ) %>% 
  stringdist_left_join(
    awards,
    by = c("Player.Name" = "Player"),
    max_dist = 1
  ) %>%
  mutate(
    across(`RonMexicoNominee`:`VidrikOnoprienkoNominee`, ~ ifelse(is.na(.x), 0, .x))
  ) %>% 
  rename(
    VidrikOnoprienkoWinner = VidrikOnoprienkoNominee
  ) %>%
  rowwise() %>% 
  mutate(
    nrNominations = 
      sum(
        c_across(
          contains("Nominee")
        )
      ),
    nrWins = 
      sum(
        c_across(
          contains("Winner")
        )
      )
  )

### Creates a model that visualizes the career data
careerVisualize <- function(statistic){
  data <- 
    careerStatistics %>% 
    do.call(what = rbind, args = .) %>% 
    .[,c("inHoF", statistic)]
  
  colnames(data) <- c("x", "y")
  
  ggplot(data) + aes(x = x, y = y) + 
    geom_violin(
      fill = "#52307c"
      ) +
    theme_bw() +
    scale_x_discrete(
      labels = 
        c(
          "No", 
          "Yes"
        )
      ) +
    labs(
      x = "In HoF?", 
      y = statistic
      ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray60"),
      panel.grid.minor.y = element_line(color = "gray75")
    ) +
    stat_summary(
      fun = median,
      geom = "point",
      shape = 21,
      size = 4,
      fill = "white"
    )+ 
    stat_summary(
      fun = function(x){
        quantile(
          x,
          probs = c(0.25)
        )
      },
      geom = "point",
      shape = 25,
      size = 3,
      fill = "white"
    )+ 
    stat_summary(
      fun = function(x){
        quantile(
          x,
          probs = c(0.75)
        )
      },
      geom = "point",
      shape = 24,
      size = 3,
      fill = "white"
    )
}

### Creates a function that evaluates the classification model
evaluateModel <- 
  function(
    model,
    newData,
    trueLabel
  ){
    predictions <- 
      predict(
        model,
        newdata = newData,
        type = "class"
      )
    
    confusionMatrix <- table(newData$inHoF, predictions)
    
    accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
    
    sensitivity <- diag(confusionMatrix) / rowSums(confusionMatrix)
    
    precision <- diag(confusionMatrix) / colSums(confusionMatrix)
    
    return(
      list(
        predictedClass = predictions,
        confusionMatrix = confusionMatrix,
        Accuracy = accuracy,
        Sensitivity = sensitivity,
        Precision = precision
      )
    )
  }


############################################################################
############################################################################
###                                                                      ###
###                              DATA SPLIT                              ###
###                                                                      ###
############################################################################
############################################################################


### Splits the data into a 70 percent training set and a 30 percent test set
### Cross-validation is used within the methods so the 70 percent training is split up further later on
set.seed(2021)

careerStatistics %<>%
  filter(
    isPlayoffs == 0
  ) %>% 
  mutate(
    dataSet = rbinom(n = n(), size = 1, prob = 0.70)
  ) %>% 
  group_by(dataSet) %>% 
  group_split(.keep = FALSE)

names(careerStatistics) <- c("Test", "Train")


############################################################################
############################################################################
###                                                                      ###
###                            VISUALIZATIONS                            ###
###                                                                      ###
############################################################################
############################################################################

careerVisualize("GamesPlayed")  
careerVisualize("Points")
careerVisualize("Hits")
careerVisualize("PenaltyMinutes")
careerVisualize("nrWins")
careerVisualize("nrNominations")


temp <- 
  careerStatistics %>% 
  do.call(what = rbind, args = .) %>% 
  group_by(inHoF) %>% 
  arrange(inHoF, nrWins) %>% 
  select(Player.Name, inHoF, nrWins, nrNominations)


###########################################################################
###########################################################################
###                                                                     ###
###                           CREATING MODELS                           ###
###                                                                     ###
###########################################################################
###########################################################################

##---------------------------------------------------------------
##                      Only career stats                       -
##---------------------------------------------------------------
careerStatFormula <- 
  as.formula(
    paste(
      "inHoF",
      paste(
        colnames(careerStatistics$Train)[3:37],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

careerStatModel <- 
  rpart(
    formula = careerStatFormula,
    data = careerStatistics$Train,
    control = 
      list(
        minsplit = 2,
        xval = 10,
        cp = 0,
        maxdepth = 100,
        maxcompete = 2,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(
  careerStatModel,
  tweak = 1.1
  )

careerStatisticsEvaluation <- 
  evaluateModel(
    model = careerStatModel,
    newData = careerStatistics$Train,
    trueLabel = careerStatistics$Train$inHoF
  )

careerStatistics$Train[careerStatisticsEvaluation$predictedClass == 0 & careerStatistics$Train$inHoF == 1,]
careerStatistics$Train[careerStatisticsEvaluation$predictedClass == 1 & careerStatistics$Train$inHoF == 0,]

##----------------------------------------------------------------
##                         Only awards                           - 
##----------------------------------------------------------------
awardsFormula <- 
  as.formula(
    paste(
      "inHoF",
      paste(
        colnames(careerStatistics$Train)[-c(1:39)],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

awardsModel <- 
  rpart(
    formula = awardsFormula,
    data = careerStatistics$Train,
    control = 
      list(
        minsplit = 2,
        xval = 10,
        cp = 0,
        maxdepth = 100,
        maxcompete = 2,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(
  awardsModel,
  tweak = 1.2
  )

awardsStatisticsEvaluation <- 
  evaluateModel(
    model = awardsModel,
    newData = careerStatistics$Train,
    trueLabel = careerStatistics$Train$inHoF
  )

careerStatistics$Train[awardsStatisticsEvaluation$predictedClass == 0 & careerStatistics$Train$inHoF == 1,]
careerStatistics$Train[awardsStatisticsEvaluation$predictedClass == 1 & careerStatistics$Train$inHoF == 0,]

##----------------------------------------------------------------
##                      Includes awards                          - 
##----------------------------------------------------------------
awardsCareerFormula <- 
  as.formula(
    paste(
      "inHoF",
      paste(
        colnames(careerStatistics$Train)[-c(1,2, 38:39)],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

awardsCareerModel <- 
  rpart(
    formula = awardsCareerFormula,
    data = careerStatistics$Train,
    control = 
      list(
        minsplit = 2,
        xval = 10,
        cp = 0,
        maxdepth = 100,
        maxcompete = 2,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(
  awardsCareerModel,
  tweak = 1.1
  )

awardsCareerStatisticsEvaluation <- 
  evaluateModel(
    model = awardsCareerModel,
    newData = careerStatistics$Train,
    trueLabel = careerStatistics$Train$inHoF
  )

awardsCareerStatisticsEvaluation <- 
  evaluateModel(
    model = awardsCareerModel,
    newData = careerStatistics$Test,
    trueLabel = careerStatistics$Test$inHoF
  )

careerStatistics$Train[awardsCareerStatisticsEvaluation$predictedClass == 0 & careerStatistics$Train$inHoF == 1,]
careerStatistics$Train[awardsCareerStatisticsEvaluation$predictedClass == 1 & careerStatistics$Train$inHoF == 0,]
careerStatistics$Test[awardsCareerStatisticsEvaluation$predictedClass == 1 & careerStatistics$Test$inHoF == 0,]

### Changes the loss matrix so that the classifier is worse if HoF is misclassified
## This matrix assumes that it costs 100 times as much to predict a HoF to a non-HoF
lossMatrix <- matrix(c(0, 10, 1, 0), nrow = 2)

lossMatrixModel <- 
  rpart(
    formula = awardsCareerFormula,
    data = careerStatistics$Train,
    parms = 
      list(
        loss = lossMatrix
      ),
    control = 
      list(
        minsplit = 2,
        xval = 10,
        cp = 0,
        maxdepth = 100,
        maxcompete = 2,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(
  lossMatrixModel,
  tweak = 1
)

lossMatrixModelEvaluation <- 
  evaluateModel(
    model = lossMatrixModel,
    newData = careerStatistics$Train,
    trueLabel = careerStatistics$Train$inHoF
  )

careerStatistics$Train[lossMatrixModel$predictedClass == 0 & careerStatistics$Train$inHoF == 1,]
careerStatistics$Train[lossMatrixModel$predictedClass == 1 & careerStatistics$Train$inHoF == 0,]


###########################################################################
###########################################################################
###                                                                     ###
###                   CALCULATING PER GAME STATISTICS                   ###
###                                                                     ###
###########################################################################
###########################################################################

careerStatisticsPerGame <- 
  careerStatistics %>% 
  lapply(
    X = .,
    FUN = function(x){
      x %>% 
      mutate(
        across(Goals:FaceoffWins, ~ .x/GamesPlayed)
      )    
    }
  )

awardsCareerPerGameFormula <- 
  as.formula(
    paste(
      "inHoF",
      paste(
        colnames(careerStatisticsPerGame$Train)[-c(1,2, 38:39)],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

awardsCareerPerGameModel <- 
  rpart(
    formula = awardsCareerPerGameFormula,
    data = careerStatisticsPerGame$Train,
    control = 
      list(
        minsplit = 2,
        xval = 10,
        cp = 0,
        maxdepth = 100,
        maxcompete = 2,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(
  awardsCareerPerGameModel,
  tweak = 1.1
)

awardsCareerPerGameStatisticsEvaluation <- 
  evaluateModel(
    model = awardsCareerPerGameModel,
    newData = careerStatisticsPerGame$Train,
    trueLabel = careerStatisticsPerGame$Train$inHoF
  )

awardsCareerPerGameStatisticsEvaluation <- 
  evaluateModel(
    model = awardsCareerPerGameModel,
    newData = careerStatisticsPerGame$Test,
    trueLabel = careerStatisticsPerGame$Test$inHoF
  )

careerStatisticsPerGame$Train[awardsCareerPerGameStatisticsEvaluation$predictedClass == 0 & careerStatisticsPerGame$Train$inHoF == 1,]
careerStatisticsPerGame$Train[awardsCareerPerGameStatisticsEvaluation$predictedClass == 1 & careerStatisticsPerGame$Train$inHoF == 0,]
careerStatisticsPerGame$Test[awardsCareerPerGameStatisticsEvaluation$predictedClass == 1 & careerStatisticsPerGame$Test$inHoF == 0,]

  














