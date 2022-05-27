
##############################################################################
##############################################################################
###                                                                        ###
###                           POSITION PREDICTOR                           ###
###                                                                        ###
###            A CLASSIFIER USED TO PREDICT THE PLAYER POSITION.           ###
###                  PREDICT BOJO BOX PLAYERS FROM S1-S41.                 ###
###  TRUE LABELS FOR TRAINING COME FROM S42-S56, MANUALLY LABELED BY LUKE  ###
###                                                                        ###
###                            AUTHOR: CANADICE                            ###
###                          CREATED: 2020-12-31                           ###
###                          LAST EDIT: 2021-02-06                         ###
###                                                                        ###
##############################################################################
##############################################################################

##----------------------------------------------------------------
##                  Loading required libraries                   -
##----------------------------------------------------------------

require(dplyr)
require(rpart)
require(rpart.plot)
require(keras)
## Use install_keras() the first time and follow the instructions posted
# install_keras()

### Sets the seed to be used for reproducible results
## Produces an error for some versions of Tensorflow
# use_session_with_seed(20200103, quiet = TRUE)


##----------------------------------------------------------------
##                  Setting the working directory                -
##----------------------------------------------------------------


## Not needed in some cases
# setwd("..")

##----------------------------------------------------------------
##                    Importing the data sets                    -
##      Data processing to aggregate for player and season       -
##----------------------------------------------------------------

data <- 
  
  ###  Import the player statistics data set
  
  read.csv(
    "csv/history_skaters.csv", 
    sep = ",", 
    header = TRUE
  ) %>% 
  
  mutate(
    FHMID = as.numeric(FHMID)
  ) %>%   
  
  ### Filter over the SHL as the Junior and IIHF might be more fluid in positions
  ### Also filters the player ID 1000028 which probably is a CPU player as they have 
  ### unreasonable high games played (300+ in one season)
  
  filter(
    LeagueId == 1,
    FHMID != 1000028
  ) %>% 
  
  ###  Aggregating over player ID and season
  
  group_by(
    FHMID,
    ## Group by season because of varying positions during a career
    Season
  ) %>% 
  
  summarize(
    across(
      GamesPlayed:FaceoffWins, 
      sum
    )
  ) %>% 
  
  ### Creating a relative statistic per game
  
  mutate(
    across(
      Goals:FaceoffWins,
      function(x) x/GamesPlayed
    )
  ) %>% 
  
  ###  Importing and joining the true positions                       
  
  left_join(
    read.csv(
      "csv/history_true_position.csv",
      sep = ",",
      header = TRUE
      ),
    by = c("FHMID" = "FHMIDS")
  ) %>% 
  
  rename(
    truePosition = Posistion.Real
  ) %>% 
  
  ### Filtering out the seasons where FHM was used, and the data available has changed
  
  filter(
    Season < 53
  ) %>% 
  
  ### Changing some true labels because of swapped positions during their career
  
  mutate(
    truePosition = 
      case_when(
        Name == "Anastasia O'Koivu" & Season < 52 ~ "D",
        Name == "GOD McZehrl" & Season < 50 ~ "D",
        TRUE ~ truePosition
      )
  ) %>% 
  
  mutate(
    truePosition = truePosition %>% factor()
  )

##-------------------------------------------------------------------
##  Splitting the data to training and test based on known labels   -
##-------------------------------------------------------------------

trainData <- 
  data %>% 
  filter(
    !is.na(truePosition)
  )

testData <-
  data %>% 
  filter(
    is.na(truePosition),
    ## Adds on a filter to remove the observations that have NA values
    ## These come from season 1-4 where some data extraction was done via box scores
    !is.na(PenaltyMajors),
    !is.na(FaceoffWins)
  )

##----------------------------------------------------------------
##                Training a decision tree model                 -
##----------------------------------------------------------------

position_formula <- 
  as.formula(
    paste(
      "truePosition",
      paste(
        colnames(data)[3:37],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

model <- 
  rpart(
    formula = position_formula,
    data = trainData,
    control = 
      list(
        minsplit = 2,
        xval = 5,
        cp = 0,
        maxdepth = 100,
        maxcompete = 0,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(model)

model$cptable

##----------------------------------------------------------------
##              Setting up a neural network model                -
##----------------------------------------------------------------

### Converts the y to numeric

trainData$truePosition <- 
  trainData$truePosition %>% 
  as.numeric() %>% 
  as.character()

### Converts the training data to a format Tensorflow/Keras understands, matrix form and "one-hot" encoding

xTrain <- 
  as.matrix(
    trainData[, 3:37]
  ) 

xTest <- 
  as.matrix(
    testData[,3:37]
  )
  
### Normalizes all input variables for training and test sets

xTrainScaled <- scale(xTrain)

xTestScaled <- 
  scale(
    xTest,
    center = 
      attr(
        xTrainScaled, "scaled:center"
      ),
    scale =
      attr(
        xTrainScaled, "scaled:scale"
      )
  ) 

### One-hot encoding of the response variable

yTrain <-
  to_categorical(
    trainData[, colnames(trainData) == "truePosition"] %>% as.matrix(),
    num_classes = NULL
  )[,2:3]

### Defining the architecture of the neural network

nnModel <- 
  keras_model_sequential() %>% 
  layer_dense(
    units = 20, 
    activation = "relu", 
    input_shape = ncol(xTrainScaled),
    use_bias = TRUE, 
    name = "First"
  ) %>% 
  layer_dense(
    units = 40, 
    activation = "relu",
    use_bias = TRUE, 
    name = "Second"
  ) %>% 
  layer_dense(
    units = 20, 
    activation = "relu",
    use_bias = TRUE, 
    name = "Third"
  ) %>% 
  ### This doesn't work as the temperature is hard coded
  layer_lambda(
    ## Tries to alter the temperature of the network
    f = function(x) x / 1,
    trainable = TRUE
  ) %>% 
  layer_dense(
    units = 2,
    activation = "softmax",
    name = "Output"
  ) %>% 
  
  ### Setting the structure of how the network should be trained
  
  compile(
    loss = "categorical_crossentropy",
    optimizer = 
      optimizer_sgd(
        lr = 0.01,
        momentum = 0.1
      ),
    metrics = c("accuracy")
  )


## Prints the summary of the model
# summary(nnModel)


##--------------------------------------------------------------------
##  Running a model fit to find the best model on the validation data  
##--------------------------------------------------------------------

history <- 
  nnModel %>% 
  fit(
    x = xTrainScaled, 
    y = yTrain, 
    epochs = 200, 
    batch_size = 50, 
    validation_split = 0.20,
    
    ### Allowing for the model to stop early given that the validation loss isn't improved
    callback = 
      callback_early_stopping(
        patience = 10,
        monitor = "val_loss"
      )
  ) %>% 
  
  ### Once a general enough model has been found, the weights are frozen from training
  
  freeze_weights() 

##---------------------------------------------------------------
##      Calibration of the temperature in the softmax layer     -
##---------------------------------------------------------------






##----------------------------------------------------------------
##            Fits the model to the calibrated model             -
##----------------------------------------------------------------

prediction <- 
  yTrain %>% 
  cbind(
    predict(
      nnModel,
      xTrainScaled,
      verbose = 1,
      batch_size = 50
      ) %>% 
      as.data.frame() %>% 
      rowwise() %>% 
      mutate(
        prob = pmax(V1, V2)
      ) %>% 
      select(
        prob
    ),
    predict_classes(
      nnModel,
      xTrainScaled,
      verbose = 1,
      batch_size = 50
      ) %>% 
      as.data.frame(
    ),
    trainData$Name,
    trainData$Season
  ) 

colnames(prediction) <- 
  c("ClassDefenceman", 
    "ClassForward", 
    "probability", 
    "predictedClassForward",
    "Name",
    "Season"
    )

### Checks the probabilities of predictions.

prediction %>% 
  group_by(
    ClassForward, 
    predictedClassForward
    ) %>% 
  summarize(
    mean = mean(probability),
    n = n()
    )

### Sorts out the misclassified observations

missedPredictions <- 
  prediction %>% 
  filter(ClassForward != predictedClassForward)


##---------------------------------------------------------------
##    Predicting the new observations on the unlabeled data     -
##---------------------------------------------------------------

predictionTest <- 
  cbind(
    predict(
      nnModel,
      xTestScaled,
      verbose = 1,
      batch_size = 50
    ) %>% 
      as.data.frame() %>% 
      rowwise() %>% 
      mutate(
        prob = pmax(V1, V2)
      ) %>% 
      select(
        prob
      ),
    predict_classes(
      nnModel,
      xTestScaled,
      verbose = 1,
      batch_size = 50
    ) %>% 
      as.data.frame(),
    testData$FHMID,
    testData$Season
  ) 

colnames(predictionTest) <- 
  c("probability", 
    "predictedClassForward",
    "FHMID",
    "Season"
  )

predictionTest %>% 
  group_by(
    predictedClassForward
  ) %>% 
  summarize(
    mean = mean(probability),
    n = n()
  )

# write.csv2(
#   x = predictionTest, 
#   row.names = FALSE,
#   file = "SHL Unlabeled predictions 20210206.csv")
# 
# write.csv2(
#   x = prediction %>% select(-ClassDefenceman), 
#   row.names = FALSE,
#   file = "SHL Labeled predictions 20210206.csv")
