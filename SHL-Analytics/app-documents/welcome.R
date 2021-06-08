
############################################################################
############################################################################
###                                                                      ###
###                  APPLICATION FOR THE WELCOME SCREEN                  ###
###                                                                      ###
############################################################################
############################################################################

welcome_ui <- function(id){
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Welcome text
    withMathJax(
      includeMarkdown(
        "app-documents/welcome.md"
      )
    )
  )
}