#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(repos = list(CRAN = "https://cloud.r-project.org", myrepo = "https://github.com/canadice/shlrtools"))

### Packages that are used
{
  ## (Hopefully) installs and loads the shlrtools package
  # devtools::install_github("Canadice/shlrtools")
  require(shlrtools)
  
  ## API packages
  require(httr)
  require(jsonlite)
  
  ## Packages for reading urls
  require(rvest)
  require(RCurl)
  
  ## Data processing
  require(tidyr)
  
  if("plyr" %in% (.packages())){
      # Do nothing
  } else {
      require(plyr)  
  }
  
  require(dplyr)
  require(janitor)
  require(tibble)
  require(fuzzyjoin)
  require(purrr)
  require(Matrix)
  require(data.table)
  
  
  ## Visualizations
  require(ggplot2)
  require(ggnewscale)
  require(RColorBrewer)
  require(cowplot)
  require(ggpubr)
  require(png)
  require(grid)
  require(plotly)
  
  ## Fonts
  require(showtext)
  
  ## Packages for markdown
  require(markdown)
  
  # Packages for svg images
  require(magick)
  require(rsvg)
    
  ## Comparison packages
  require(arsenal)
  require(stringdist)
  
  ## Packages for image recognition
  # require(tesseract)
  
  ## Package for handling date and time
  require(lubridate)
  
  ## Packages for handling strings
  require(stringr)
  require(stringi)
  
  ## Loading package that can talk to Google Sheets
  require(googlesheets4)
  require(googledrive)
  
  ## Loading database related packages
  require(DBI)
  require(dbplyr)
  require(RSQLite)
  
  # Hash creation for saving files to Google Drive
  require(digest)
  
  ## Loading Shiny packages
  require(shiny)
  require(DT)
  require(reactable)
  require(knitr)
  require(ggmap)
  require(osmdata)
  require(kableExtra)
  require(janitor)
  require(usmap)
  # require(shinythemes)
  require(shinycssloaders)
  require(shinyjs)
  require(shinydashboard)
  require(dashboardthemes)
  
  ## For point and click dragging
  require(sortable)
  
  ## For futures and promises
  require(promises)
  require(future)
}

version <- "v2.4.4"

## Sets up that evaluating futures is done in parallell
plan(multisession)

##----------------------------------------------------------------
##                Loading fonts for visualizations               -
##----------------------------------------------------------------
# font_add_google(
#     "Roboto Mono",
#     family = "body",
#     regular.wt = 300,
#     bold.wt = 500)
# 
# font_add_google(
#     "Josefin Sans",
#     family = "title",
#     regular.wt = 300,
#     bold.wt = 500)
# 
# showtext_auto()

shlBlue <- "#2c6185"
shlOrange <- "#e08b46"
shlYellow <- "#F7D54D"
shlGray <- "#f3f3f3"
shlBlue1 <- "#6393BA"
shlBlue2 <- "#D2F5FF"

customLogo <- 
    shinyDashboardLogoDIY(
        boldText = "",
        mainText = tags$a(
            href='https://simulationhockey.com/',
            target="_blank",
            tags$img(src='shl_analytics_logo.png', height = "70"),
        ),
        badgeText = version,
        badgeTextColor = "white",
        badgeBackColor = shlOrange
    )

customTheme <- 
    shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "Tahoma"
        ,appFontColor = "#000000"
        ,primaryFontColor = "#FFFFFF"
        ,infoFontColor = "#000000"
        ,successFontColor = "#000000"
        ,warningFontColor = "#000000"
        ,dangerFontColor = "#FFFFFF"
        ,bodyBackColor = shlGray
        
        ### header
        ,logoBackColor = shlBlue1
        
        ,headerButtonBackColor = shlBlue1
        ,headerButtonIconColor = "#000000"
        ,headerButtonBackColorHover = shlOrange
        ,headerButtonIconColorHover = "#000000"
        
        ,headerBackColor = shlBlue1
        ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = shlBlue
        ,sidebarPadding = "0"
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = "5"
        ,sidebarMenuBorderRadius = 0
        
        ,sidebarShadowRadius = ""
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "#FFFFFF"
        
        ,sidebarSearchBackColor = "#FFFFFF"
        ,sidebarSearchIconColor = "#000000"
        ,sidebarSearchBorderColor = "#000000"
        
        ,sidebarTabTextColor = "#FFFFFF"
        ,sidebarTabTextSize = "14"
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "none"
        ,sidebarTabBorderWidth = "0"
        
        ,sidebarTabBackColorSelected = "#D1D1D1"
        ,sidebarTabTextColorSelected = "#000000"
        ,sidebarTabRadiusSelected = "0px"
        
        ,sidebarTabBackColorHover = shlOrange
        ,sidebarTabTextColorHover = "#000000"
        ,sidebarTabBorderStyleHover = "none solid none none"
        ,sidebarTabBorderColorHover = "#000000"
        ,sidebarTabBorderWidthHover = "6"
        ,sidebarTabRadiusHover = "0px"
        
        ### boxes
        ,boxBackColor = "#FFFFFF"
        ,boxBorderRadius = "5"
        ,boxShadowSize = "none"
        ,boxShadowColor = ""
        ,boxTitleSize = "18"
        ,boxDefaultColor = shlBlue
        ,boxPrimaryColor = shlBlue
        ,boxInfoColor = "#CCCCCC"
        ,boxSuccessColor = shlBlue1
        ,boxWarningColor = shlOrange
        ,boxDangerColor = "#000000"
        
        ,tabBoxTabColor = "#EDEDED"
        ,tabBoxTabTextSize = "14"
        ,tabBoxTabTextColor = shlBlue
        ,tabBoxTabTextColorSelected = "#000000"
        ,tabBoxBackColor = "#EDEDED"
        ,tabBoxHighlightColor = shlBlue
        ,tabBoxBorderRadius = "5"
        
        ### inputs
        ,buttonBackColor = shlOrange
        ,buttonTextColor = "#000000"
        ,buttonBorderColor = "#000000"
        ,buttonBorderRadius = "5"
        
        ,buttonBackColorHover = shlBlue
        ,buttonTextColorHover = "#FFFFFF"
        ,buttonBorderColorHover = "#000000"
        
        ,textboxBackColor = "#FFFFFF"
        ,textboxBorderColor = "#000000"
        ,textboxBorderRadius = "5"
        ,textboxBackColorSelect = "#B3D1E6"
        ,textboxBorderColorSelect = "#000000"
        
        ### tables
        ,tableBackColor = "#F7F7F7"
        ,tableBorderColor = "#B3D1E650"
        ,tableBorderTopSize = "0"
        ,tableBorderRowSize = "0"
    )


##----------------------------------------------------------------
##          Loading the API functions and some data sets         -
##----------------------------------------------------------------

source("https://raw.githubusercontent.com/canadice/shl/main/scripts/API/apiSetup.R")
# Internal sourcing
# source("../scripts/API/apiSetup.R")

fileSources <- c("app-documents")

## Loads and runs RMarkdown files
rmdFiles <- 
    sapply(
        X = fileSources,
        FUN = function(x){
            list.files(path = x, pattern = ".Rmd$") %>% 
                paste(x, ., sep = "/")
        },
        simplify = TRUE,
        USE.NAMES = FALSE
    ) %>% 
    unlist() %>% 
    .[str_detect(., pattern = ".Rmd")]

sapply(rmdFiles, rmarkdown::render, quiet = T, output_dir = "app-documents")

## Loads files
sapply(
    X = fileSources,
    FUN = function(x){
        files <- list.files(path = x, pattern = ".R$")
        
        sapply(
            X = paste(x, files, sep = "/"),
            FUN = source
        )
    }
)


##---------------------------------------------------------------
##                  Defining the user interface                 -
##---------------------------------------------------------------

ui <- 
    dashboardPage(
        title = "SHL Analytics",
        dashboardHeader(
            title = customLogo,
            tags$li(
              tags$head(
                tags$link(
                  rel = "icon", 
                  type = "image/png", 
                  href = "favicon.png"),
                tags$title("SSL Analytics")
              ),
              class = "dropdown",
              tags$head(
                  ## HTML code so that a href link inherits the text color, not the link color
                  tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}")),
                  
                  ## Increases the size of the logo box at the top left
                  tags$style(".main-header {max-height: 80px}"),
                  tags$style(".main-header .logo {height: 80px}"),
                  tags$style(".main-header .logo {width: 300px}"),
                  
                  ## Changes the margin of the sidebar
                  tags$style(".main-header .navbar {margin-left: 300px}"),
                  tags$style(type="text/css", "text {font-family: sans-serif, courier}"),
                  
              )
            )
        ),
        dashboardSidebar(
            width = 300,
            # Adjust the sidebar in accordance with the higher header
            tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
            sidebarMenu(
                id = "tabs",
                
                #################################################################
                ##                           Welcome                           ##
                #################################################################
                
                menuItem(
                    "Welcome",
                    tabName = "welcome",
                    selected = TRUE
                ),
                
                #################################################################
                ##                     Menu of Trackers                        ##
                #################################################################
                
                menuItem(
                    "Trackers",
                    tabName = "Test",
                    icon = icon("clipboard"),
                    ###### Draft Class Tracker
                    menuSubItem(
                        "Draft Class Tracker",
                        tabName = "trackerClass"
                    ),
                    ###### Casino Tracker
                    menuSubItem(
                        "Casino Tracker",
                        tabName = "trackerCasino"
                    ),
                    ###### Position Tracker
                    menuSubItem(
                        "Position Tracker",
                        tabName = "trackerPosition"
                    ),
                    ###### Tank Standings Tracker
                    menuSubItem(
                      "Tank Standings Tracker",
                      tabName = "trackerTankStandings"
                    ),
                    menuSubItem(
                      "Team Tracker", 
                      href = "https://portal.simulationhockey.com/teams"
                    )
                ),
                
                #################################################################
                ##                       Menu of IIHF                          ##
                #################################################################
                
                menuItem(
                    "IIHF",
                    icon = icon("globe"),
                    ###### IIHF Eligibility Tracker
                    menuSubItem(
                        "IIHF Eligibility",
                        tabName = "trackerIIHF"
                    ),
                    menuSubItem(
                        "IIHF Rankings",
                        tabName = "rankingIIHF"
                    ),
                    menuSubItem(
                      "IIHF Transfer Limits",
                      tabName = "limitsIIHF"
                    )
                ),
                
                #################################################################
                ##                   Menu of Visualizations                    ##
                #################################################################
                
                menuItem(
                    "Visualizations",
                    icon = icon("palette"),
                    ###### Player Attribute Visualizer
                    menuSubItem(
                        "Player Attributes",
                        tabName = "visAttributes"
                    ),
                    # ###### Stat Cards
                    # menuSubItem(
                    #     "Stat Cards",
                    #     tabName = "visStatCards"
                    # ),
                    ###### Player Similarities
                    menuSubItem(
                        "Player Similarities",
                        tabName = "visSimilarity"
                    )
                ),
                
                #################################################################
                ##                       Menu of Careers                       ##
                #################################################################
                
                menuItem(
                    "Careers",
                    icon = icon("address-card"),
                    menuSubItem(
                        "Skater Career",
                        tabName = "careerSkater"
                    ),
                    menuSubItem(
                        "Goalie Career",
                        tabName = "careerGoalie"
                    ),
                    menuSubItem(
                      "Career Data",
                      tabName = "careerData"
                    ),
                    menuSubItem(
                        "League Records",
                        tabName = "careerRecords"
                    )
                ),
                
                #################################################################
                ##                        Menu of Tools                        ##
                #################################################################
                menuItem(
                  "Audit Tool", 
                  icon = icon("clipboard-check"),
                  tabName = "toolAudit"
                ),
                
                menuItem(
                    "Tools",
                    icon = icon("tools"),
                    menuSubItem(
                        "Sim Scheduling Tool",
                        icon = icon("calendar"),
                        tabName = "toolSchedule"
                    ),
                    menuSubItem(
                      "League Scheduling Tool",
                      icon = icon("calendar"),
                      tabName = "toolLeagueSchedule"
                    ),
                    menuSubItem(
                        "Regression Tool",
                        icon = icon("procedures"),
                        tabName = "toolRegression"
                    ),
                    menuSubItem(
                        "Draft Lottery Tool",
                        icon = icon("dice"),
                        tabName = "toolDraftLottery"
                    )
                ),
                ##################################################################
                ##                        Link to Github                        ##
                ##################################################################
                
                menuItem(
                    "Github", 
                    icon = icon("github"),
                    href = "https://github.com/canadice/shl"
                ),
                menuItem(
                    "Changelog", 
                    icon = icon("file-invoice"),
                    tabName = "changelog"
                )
            )
        ),
        dashboardBody(
            customTheme,
            useShinyjs(),
            ### Specifies a custom color for value and info boxes
            tags$style(".small-box.bg-orange { background-color: #e08b46 !important; color: #000000 !important; }"),
            tabItems(
                tabItem(
                    "welcome",
                    welcome_ui(id = "welcome")
                ),
                tabItem(
                    "trackerClass",
                    titlePanel(
                        h1("Draft Class Tracker", align = "center")
                    ),
                    draftClassUI(id = "playersUI")
                ),
                tabItem(
                    "trackerCasino",
                    titlePanel(
                        h1("Casino Tracker", align = "center")
                    ),
                    casinoUI(id = "casinoUI")
                ),
                tabItem(
                    "trackerPosition",
                    titlePanel(
                        h1("Position Tracker", align = "center")
                    ),
                    posTrackerUI(id = "posTrackerUI")
                ),
                tabItem(
                    "trackerIIHF",
                    titlePanel(
                        h1("IIHF Eligibility", align = "center")
                    ),
                    iihfUI(id = "iihfUI")
                ),
                tabItem(
                    "trackerTeam",
                    titlePanel(
                        h1("Team Tracker", align = "center")
                    ),
                    teamUI(id = "teamUI")
                ),
                tabItem(
                  "trackerTankStandings",
                  titlePanel(
                    h1("Tank Standings Tracker", align = "center")
                  ),
                  tankStandingsUI(id = "tankStandings")
                ),
                tabItem(
                    "rankingIIHF",
                    titlePanel(
                        h1("IIHF Rankings", align = "center")
                    ),
                    rankingIIHFUI(id = "rankingIIHFUI")
                ),
                tabItem(
                  "limitsIIHF",
                  titlePanel(
                    h1("IIHF Transfer Limits", align = "center")
                  ),
                  limitsIIHFUI(id = "limitsIIHF")
                ),
                tabItem(
                    "visAttributes",
                    titlePanel(
                        h1("Visualization of Player Attributes", align = "center")
                    ),
                    radarUI(id = "radarUI")
                ),
                # tabItem(
                #     "visStatCards",
                #     titlePanel(
                #         h1("Stat Card", align = "center")
                #     ),
                #     playerCardUI(id = "pCardUI")
                # ),
                tabItem(
                    "visSimilarity",
                    titlePanel(
                        h1("Player similarity using multidimensional scaling", align = "center")
                    ),
                    mdsUI(id = "simUI")
                ),
                tabItem(
                  "toolLeagueSchedule",
                  leagueScheduleUI(id = "leagueSchedule")
                ),
                tabItem(
                    "careerSkater",
                    titlePanel(
                        h1("Skater Career Card", align = "center")
                    ),
                    careerUI(id = "careerUI")
                ),
                tabItem(
                    "careerGoalie",
                    titlePanel(
                        h1("Goalie Career Card", align = "center")
                    ),
                    careerGoalieUI(id = "careerGoalieUI")
                ),
                tabItem(
                  "careerData",
                  titlePanel(
                    h1("Career Data", align = "center")
                  ),
                  careerDataUI(id = "careerData")
                ),
                tabItem(
                    "careerRecords",
                    titlePanel(
                        h1("League Records", align = "center")
                    ),
                    careerRecordsUI(id = "careerRecordsUI")
                ),
                tabItem(
                    "toolSchedule",
                    titlePanel(
                        h1("Scheduling Tool", align = "center")
                    ),
                    scheduleUI(id = "scheduleUI")
                ),
                tabItem(
                  "toolAudit",
                  titlePanel(
                    h1("Audit Tool", align = "center")
                  ),
                  auditUI(id = "toolAudit")
                ),
                tabItem(
                  "toolRegression",
                  titlePanel(
                      h1("Regression Tool", align = "center")
                  ),
                  regressionUI(id = "regressionUI")
                ),
                tabItem(
                    "toolDraftLottery",
                    titlePanel(
                        h1("Draft Lottery Tool", align = "center")
                    ),
                    draftLotteryUI(id = "draftLotteryUI")
                ),
                tabItem(
                    "changelog",
                    changelogUI(id = "changelog")
                )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    loadedModuleCasino <- reactiveVal(FALSE)
    loadedModuleIIHF <- reactiveVal(FALSE)
    loadedModuleTeam <- reactiveVal(FALSE)
    loadedModuleDraftLottery <- reactiveVal(FALSE)
    loadedModulePosTracker <- reactiveVal(FALSE)
    loadedModuletoolSchedule <- reactiveVal(FALSE)
    loadedModuletoolRegression <- reactiveVal(FALSE)
    loadedModulevisStatCards <- reactiveVal(FALSE)
    loadedModulevisSimilarity <- reactiveVal(FALSE)
    loadedModuletoolAudit <- reactiveVal(FALSE)
    loadedModuleleagueSchedule <- reactiveVal(FALSE)
    
    
    ##---------------------------------------------------------------
    ##          Loading each of the different backend sites         -
    ##---------------------------------------------------------------
    radarSERVER(id = "radarUI")
    
    draftClassSERVER(id = "playersUI")
    
    careerCardSERVER(id = "careerUI") 
    
    careerGoalieSERVER(id = "careerGoalieUI")
    
    careerRecordsSERVER(id = "careerRecordsUI")
    
    iihfSERVER(id = "iihfUI") 
    
    careerDataSERVER(id = "careerData")
    
    limitsIIHFSERVER(id = "limitsIIHF")
    
    tankStandingsServer(id = "tankStandings")
    
    ### Only run the module once the menu is clicked to fasten start time
    observeEvent(input$tabs,{
        ## Checks which menu tab has been selected and whether the module has already been loaded
        if(input$tabs == "trackerCasino" & !loadedModuleCasino()){
            
            loadedModuleCasino(TRUE)
            
            casinoSERVER(id = "casinoUI")
            
        } else if(input$tabs=="rankingIIHF" & !loadedModuleIIHF()){
            
            loadedModuleIIHF(TRUE)
            
            rankingIIHFSERVER(id = "rankingIIHFUI")
            
        } else if(input$tabs == "trackerTeam" & !loadedModuleTeam()){
            
            loadedModuleTeam(TRUE)
            
            teamSERVER(id = "teamUI")
            
        } else if(input$tabs == "toolDraftLottery" & !loadedModuleDraftLottery()){
          
          loadedModuleDraftLottery(TRUE)
          
          draftLotterySERVER(id = "draftLotteryUI")
          
        } else if(input$tabs == "trackerPosition" & !loadedModulePosTracker()){
          
          loadedModulePosTracker(TRUE)
          
          posTrackerSERVER(id = "posTrackerUI")
          
        } else if(input$tabs == "toolSchedule" & !loadedModuletoolSchedule()){
          
          loadedModuletoolSchedule(TRUE)
          
          scheduleSERVER(id = "scheduleUI")
          
        } else if(input$tabs == "toolRegression" & !loadedModuletoolRegression()){
          
          loadedModuletoolRegression(TRUE)
          
          regressionSERVER(id = "regressionUI")
          
        } else if(input$tabs == "visStatCards" & !loadedModulevisStatCards()){
          
          loadedModulevisStatCards(TRUE)
          
          playerCardSERVER(id = "pCardUI")
          
        } else if(input$tabs == "visSimilarity" & !loadedModulevisSimilarity()){
          
          loadedModulevisSimilarity(TRUE)
          
          mdsSERVER(id = "simUI")
          
        } else if(input$tabs == "toolAudit" & !loadedModuletoolAudit()){
          
          loadedModuletoolAudit(TRUE)
          
          auditSERVER(id = "toolAudit")
          
        } else if(input$tabs == "toolLeagueSchedule" & !loadedModuleleagueSchedule()){
          
          loadedModuleleagueSchedule(TRUE)
          
          leagueScheduleSERVER(id = "leagueSchedule")
          
        }  
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ### Sets the url for each tab
    observeEvent(input$tabs,{
      ## Writes a different url based on the tab
      newURL <- paste0(
        session$clientData$url_protocol,
        "//",
        session$clientData$url_hostname,
        ":",
        session$clientData$url_port,
        session$clientData$url_pathname,
        "#",
        input$tabs
      )
      updateQueryString(newURL, mode = "replace", session)
    })
    
    observe({
      currentTab <- sub("#", "", session$clientData$url_hash)
      if(!is.null(currentTab)){
        updateTabItems(session, "tabs", selected = currentTab)
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
