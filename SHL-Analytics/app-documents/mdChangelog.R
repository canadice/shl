
############################################################################
############################################################################
###                                                                      ###
###                  APPLICATION FOR THE WELCOME SCREEN                  ###
###                                                                      ###
############################################################################
############################################################################

changelogUI <- function(id){
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Welcome text
    withMathJax(
      includeMarkdown(
        "app-documents/mdChangelog.md"
      )
    )
  )
}