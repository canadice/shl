## Research in https://hockey-graphs.com/2019/01/16/wins-above-replacement-history-philosophy-and-objectives-part-1/


############################################################################
############################################################################
###                                                                      ###
###         TOOL TO VISUALIZE DISTRIBUTION OF PLAYERS ATTRIBUTES         ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

boxscoreUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        div(
          id = ns("gameSelector"),
          radioButtons(
            inputId = ns("league"),
            label = "Select a league",
            choices = 
              c("SHL", "SMJHL"),
            selected = "SHL"
          ),
          numericInput(
            inputId = ns("season"),
            label = "Select a season",
            value = 60,
            min = 60,
            max = 60
          ),
          uiOutput(outputId = ns("gameList")),
          actionButton(
            inputId = ns("confirmGameChoice"),
            icon = icon("check-circle"),
            label = "Claim this game"
          )
        ),
        div(
          id = ns("fileUpload"),
          fileInput(
            inputId = ns("inputZip"),
            label = "Upload a file",
            buttonLabel = "Browse...",
            accept = ".zip",
            placeholder = "No file is uploaded",
            width = "100%"
          ),
          shinyjs::disabled(
            actionButton(
              inputId = ns("saveCurrent"),
              label = "Save the current game data"
            )  
          )  
        ) %>% 
          hidden()
      ),
      column(
        width = 8,
        uiOutput(
          outputId = ns("tabboxes")
        )
      )
    )
  )
}


## Backend for vizualizations
boxscoreSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      
      ##----------------------------------------------------------------
      ##          Informative text on top of each editable tab         -
      ##----------------------------------------------------------------
      
      editText <- "Please check the processed image, specifically any red or empty cells. 
      Double click on a cell to edit. After entering the new value you will be returned to the first tab.
      Repeat until the table is correct."
      
      
      ##---------------------------------------------------------------
      ##                      Creates dynamic UIs                     -
      ##---------------------------------------------------------------
      
      output$tabboxes <- renderUI({
        tabBox(
          width = NULL,
          selected = "Instructions",
          # The instructions
          if(is.null(reactives$bHome)){
            tabPanel(
              ""
            )
          }else{
            tabPanel(
              "Home\nBoxscore",
              h5(editText),
              DTOutput(
                outputId = session$ns("home")
              ) 
            )
          },
          if(is.null(reactives$bAway)){
            tabPanel(
              ""
            )
          }else{
            tabPanel(
              "Away\nBoxscore",
              h5(editText),
              DTOutput(
                outputId = session$ns("away")
              ) 
            )
          },
          tabPanel(
            "Instructions",
            withMathJax(
              includeMarkdown(
                "app-documents/mdBoxInstruction.md"
              )
            )
          )
        )
      })
      
      output$gameList <- renderUI({
        selectInput(
          inputId = session$ns("selectedGame"),
          label = "Select a game",
          choices = reactives$schedule %>% 
            filter(
              is.na(boxscore)
            ) %>% select(game)
        ) %>% withSpinner()
      })
      
      ##---------------------------------------------------------------
      ##                  Setup and functions created                 -
      ##---------------------------------------------------------------
      
      ## Writing data to Google Sheet for easier distribution
      drive_auth(email = FALSE, path = ".secrets/client_secret.json")
      gs4_auth(email = FALSE, path = ".secrets/client_secret.json")
      
      ## Function for matching strings in a DT
      styleContain <- function(string, color){
        JS(
          sprintf(
            "value === null || value.match(/\\b%s\\b/) !== null ? '' : '%s'", 
            string, 
            color
          )
        )
      }
      
      ## Sets the directory where tesseract training data is located
      tesspath <- paste0(getwd(), "/tessdata")
      tesseract_download("eng", datapath = tesspath)
      
      ## Creates a couple of whitelisted patterns to search for
      boxNumbers <-
        tesseract(
          "eng",
          options =
            list(
              tessedit_char_whitelist = ":. -0123456789"
            ),
          datapath = tesspath
        )
      
      numbers <-
        tesseract(
          "eng",
          options =
            list(
              tessedit_char_whitelist = "0123456789"
            ),
          datapath = tesspath
        )
      
      eng <- tesseract("eng", datapath = tesspath)
      
      letters <-
        tesseract(
          "eng",
          options =
            list(
              tessedit_char_whitelist =
                " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"
            ),
          datapath = tesspath
        )
      
      ## Function that reads specifically the two box scores
      boxScoreReader <- function(boxscoreImage){
        
        names <- 
          boxscoreImage %>% 
          image_crop(geometry = "300x536+10+310") %>% 
          # image_quantize(max = 2) %>% 
          image_reducenoise() %>% 
          image_resize("1500x") %>%
          image_contrast(sharpen = 1) %>% 
          ocr(engine = letters)
        
        boxscore <- 
          boxscoreImage %>% 
          image_crop(geometry = "850x536+530+310") %>% 
          image_resize("4000x") %>%
          image_reducenoise() %>%
          image_contrast(sharpen = 1) %>%   
          ocr(engine = boxNumbers)
        
        # cat(boxscore)
        
        # cat(names)
        
        pInfo <- 
          names %>% 
          str_split(pattern = "\n", simplify = TRUE) %>% 
          stri_remove_empty() %>% 
          str_split(pattern = " ", n = 2, simplify = TRUE) %>% 
          as_tibble(.name_repair = "unique") %>% 
          rename(
            POS = `...1`,
            NAME = `...2`
          )
        
        pData <- 
          boxscore %>% 
          str_split(pattern = "\n", simplify = TRUE) %>% 
          stri_remove_empty() %>% 
          str_split(pattern = " ") %>% 
          lapply(
            X = .,
            FUN = function(x){
              x <- 
                x %>% 
                t() %>% 
                data.frame() %>% 
                rename(
                  G = X1,
                  A = X2,
                  `+/-` = X3,
                  Shots = X4,
                  MissedSh = X5,
                  BlockedSh = X6,
                  PIM = X7,
                  Hit = X8,
                  TkA = X9,
                  GvA = X10,
                  Shifts = X11,
                  TOI = X12,
                  PPTOI = X13,
                  SHTOI = X14,
                  EVTOI = X15,
                  FOW = X16,
                  FOL = X17
                )
            }
          ) %>% 
          do.call(what = rbind.fill, args = .) %>% 
          select(-X18)
        
        DATA <- 
          pInfo %>% 
          cbind(
            pData
          )
        
        return(DATA)
      }
      
      ## Function that converts images to data
      imageReader <- function(pathToZip){
        zipPath <- pathToZip
        fileNames <- unzip(zipfile = zipPath, list = TRUE)
        
        if(all(c("top.png", "bottom.png", "home.png", "away.png")%in% fileNames$Name)){
          # Continue
        } else {
          stop("Screenshots are not named correctly.")
        }
        
        top <- 
          unzip(zipPath, fileNames$Name[4]) %>% 
          image_read()
        
        away <- 
          unzip(zipPath, fileNames$Name[1]) %>% 
          image_read()
        
        home <- 
          unzip(zipPath, fileNames$Name[3]) %>% 
          image_read()
        
        bottom <- 
          unzip(zipPath, fileNames$Name[2]) %>% 
          image_read()
        
        gInfo <- 
          top %>% 
          image_crop(geometry = "500x100+10+410") %>% 
          # image_quantize(max = 2) %>% 
          image_reducenoise() %>% 
          image_resize("1000x") %>%
          image_contrast(sharpen = 1) %>% 
          ocr(engine = eng) %>% 
          str_split(pattern = "\n", simplify = TRUE) %>% 
          stri_remove_empty() %>% 
          str_split(pattern = " ", n = 2, simplify = TRUE) %>% 
          as_tibble(.name_repair = "unique") %>% 
          rename(
            Stat = `...1`,
            Value = `...2`
          ) %>% 
          tibble::column_to_rownames(var = "Stat") %>% 
          t() %>% 
          as_tibble() %>% 
          mutate(
            Date = as.Date(Date %>% str_remove(pattern = "th|nd|st"), "%B %d,%Y")
          )
        
        tStats <- 
          top %>% 
          image_crop(geometry = "1200x100+580+410") %>% 
          image_reducenoise() %>% 
          image_resize("2000x") %>%
          image_contrast(sharpen = 1) %>% 
          ocr(engine = numbers) %>% 
          str_split(pattern = "\n", simplify = TRUE) %>% 
          stri_remove_empty() %>% 
          str_split(pattern = " ") %>% 
          lapply(
            X = .,
            FUN = function(x){
              x <- 
                x %>% 
                t() %>% 
                data.frame() %>% 
                select(
                  SHOTS = X1,
                  PIM = X2,
                  HITS = X3,
                  GvA = X4,
                  TkA = X5,
                  FOW = X6
                )
            }
          ) %>% 
          do.call(what = rbind.fill, args = .) %>% 
          mutate(
            team = c("away", "home")
          )
        
        boxHome <- boxScoreReader(home)
        boxAway <- boxScoreReader(away)
        
        gInfo$teams <- 
          top %>% 
          image_crop(geometry = "530x20+50+310") %>% 
          image_reducenoise() %>% 
          image_resize("2000x") %>%
          image_contrast(sharpen = 1) %>% 
          ocr(engine = letters) %>% 
          str_extract_all(pattern = teamInfo$team, simplify = TRUE) %>% 
          stri_remove_empty() %>% 
          list()
        
        list(
          gInfo = gInfo,
          tInfo = tStats,
          bHome = boxHome,
          bAway = boxAway
        ) %>% 
          return()
      }
      
      ##----------------------------------------------------------------
      ##                      Reactive environment                     -
      ##----------------------------------------------------------------
      
      ### In order to save the edit to the reactive data, a reactiveValues is needed
      reactives <- 
        reactiveValues(
          gInfo = NULL,
          tInfo = NULL,
          bHome = NULL,
          bAway = NULL,
          schedule = NULL
        )
      
      ### Observes new league and season for the corresponding schedule
      observe({
        reactives$schedule <- 
          read_sheet(
            ss = "https://docs.google.com/spreadsheets/d/1B_Vn-wP4pix1QYzHMQaCPMZ3ytWKN6tRExvm0EvOwsw/edit#gid=1420501627",
            sheet = paste0(input$league, input$season)
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                fhmID,
                abbr,
                league
              ) %>% 
              filter(
                league == input$league
              ),
            by = c("homeTeam" = "fhmID")
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                fhmID,
                abbr,
                league
              ) %>% 
              filter(
                league == input$league
              ),
            by = c("awayTeam" = "fhmID"),
            suffix = c("_home", "_away")
          ) %>% 
          mutate(
            game = paste(date, paste(abbr_away, abbr_home, sep = "@"), sep = ", ")
          ) 
      })
      
      ### Observes a selection of a new game
      observeEvent({
        input$confirmGameChoice
        },
        {
          # Write the selected data as pending
          
          reactives$schedule %>% 
            mutate(
              boxscore = if_else(game == input$selectedGame, "PENDING", "")
            ) %>% 
            write_sheet(
              ss = "https://docs.google.com/spreadsheets/d/1B_Vn-wP4pix1QYzHMQaCPMZ3ytWKN6tRExvm0EvOwsw/edit#gid=1420501627",
              sheet = paste0(input$league, input$season)
            )
          
        }
      )
      
      ### Observes new input files and processes the images
      observe({
        if(is.null(input$inputZip)){
          #Do nothing
        } else {
          progressBar <- 
            Progress$new(
              session,
              min = 1,
              max = 30)
          
          on.exit(progressBar$close())
          
          progressBar$set(message = 'The images are being analyzed!')
          
          for(i in 1:10){
            progressBar$set(value = i)           
            Sys.sleep(0.1)
          }
          
          processed <- imageReader(input$inputZip$datapath)
          
          for(i in 11:30){
            progressBar$set(value = i)    
            Sys.sleep(0.1)
          }
          
          reactives$gInfo <- processed$gInfo 
          reactives$tInfo <- processed$tInfo
          reactives$bHome <- processed$bHome
          reactives$bAway <- processed$bAway
          
          shinyjs::enable(id = "saveCurrent")  
        }
      })
      
      output$home <- renderDT({
        if(is.null(reactives$bHome)){
          NULL
        } else {
          
          reactives$bHome %>% 
            datatable(
              editable = "cell",
              rownames = FALSE,
              selection = "none",
              options = 
                list(
                  ordering = FALSE, 
                  ## Sets a scroller for the rows
                  scrollY = '600px',
                  ## Sets size of rows shown
                  scrollCollapse = TRUE,
                  ## Removes pages in the table
                  paging = FALSE,
                  ## Adds scrollable horizontal
                  scrollX = '600px',
                  # pageLength = 10,
                  # lengthMenu = c(10, 25, 50, 100),
                  dom = 't'
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "G",
                  "A",
                  "+/-",
                  "Shots",
                  "MissedSh",
                  "BlockedSh",
                  "PIM",
                  "Hit",
                  "TkA",
                  "GvA",
                  "FOW",
                  "FOL"
                ),
              backgroundColor = 
                styleInterval(
                  cuts = c(-3, 5),
                  values = c("red", NA, "red")
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "Shifts"
                ),
              backgroundColor = 
                styleInterval(
                  cuts = c(0, 40),
                  values = c("red", NA, "red")
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "TOI",
                  "PPTOI",
                  "SHTOI",
                  "EVTOI"
                ),
              backgroundColor = 
                styleContain(
                  ":",
                  "red"
                )
            )
        }
      })
      
      homeProxy <- dataTableProxy(session$ns("home"))
      
      observeEvent(
        input$home_cell_edit,
        {
          info = input$home_cell_edit
          str(info)
          i = info$row
          j = info$col + 1
          v = info$value
          
          reactives$bHome[i,j] <- isolate(DT::coerceValue(v, reactives$bHome[i, j]))
        }
      )
      
      output$away <- renderDT({
        if(is.null(reactives$bAway)){
          NULL
        } else {
          reactives$bAway %>% 
            datatable(
              editable = "cell",
              rownames = FALSE,
              selection = "none",
              options = 
                list(
                  ordering = FALSE, 
                  ## Sets a scroller for the rows
                  scrollY = '600px',
                  ## Sets size of rows shown
                  scrollCollapse = TRUE,
                  ## Removes pages in the table
                  paging = FALSE,
                  ## Adds scrollable horizontal
                  scrollX = '600px',
                  # pageLength = 10,
                  # lengthMenu = c(10, 25, 50, 100),
                  dom = 't'
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "G",
                  "A",
                  "+/-",
                  "Shots",
                  "MissedSh",
                  "BlockedSh",
                  "PIM",
                  "Hit",
                  "TkA",
                  "GvA",
                  "FOW",
                  "FOL"
                ),
              backgroundColor = 
                styleInterval(
                  cuts = c(-3, 5),
                  values = c("red", NA, "red")
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "Shifts"
                ),
              backgroundColor = 
                styleInterval(
                  cuts = c(0, 40),
                  values = c("red", NA, "red")
                )
            ) %>% 
            formatStyle(
              columns = 
                c(
                  "TOI",
                  "PPTOI",
                  "SHTOI",
                  "EVTOI"
                ),
              backgroundColor = 
                styleContain(
                  ":",
                  "red"
                )
            )
        }
      })
      
      awayProxy <- dataTableProxy(session$ns("away"))
      
      observeEvent(
        input$away_cell_edit,
        {
          info = input$away_cell_edit
          str(info)
          i = info$row
          j = info$col + 1
          v = info$value
          
          reactives$bAway[i,j] <- isolate(DT::coerceValue(v, reactives$bAway[i, j]))
        }
      )
      
      observeEvent(
        input$saveCurrentConfirm,
        {
          
          disable(id = "saveCurrentConfirm")
          
          fileName <- sprintf("boxscores/%s_%s.RData", paste(input$selectedGame, Sys.time()), digest::digest(reactives))

          data <- reactives$gInfo

          data$TeamInfo <- reactives$tInfo %>% list()

          data$BoxHome <- reactives$bHome %>% list()

          data$BoxAway <- reactives$bAway %>% list()

          save(data, file = fileName)

          googledrive::drive_upload(
            fileName,
            path = as_id("1Mfj6vO9aMIsaty_tsMEm8eChPgWYspXR")
            )
          
          reactives$schedule %>% 
            mutate(
              boxscore = if_else(game == input$selectedGame, "DONE", "")
            ) %>% 
            sheets_edit(
              ss = "https://docs.google.com/spreadsheets/d/1B_Vn-wP4pix1QYzHMQaCPMZ3ytWKN6tRExvm0EvOwsw/edit#gid=1420501627",
              sheet = paste0(input$league, input$season)
            )
          
          removeModal(session)
          enable(id = "saveCurrentConfirm")
        }
      )
      
      
      observeEvent(
        input$saveCurrent, 
        {
          showModal(
            modalDialog(
              tagList(
                "You will now upload the processed data to the cloud. Are you sure the data is correct?"
              ), 
              title="Create a file",
              footer = 
                tagList(
                  actionButton(
                    session$ns("saveCurrentConfirm"), 
                    "Confirm saving file"
                  ),
                  modalButton("Cancel")
                )
            )
          )
        }
      )
      
    }
  )
}

