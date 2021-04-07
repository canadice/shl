
#################################################################
##                          API Set-up                         ##
##                     Created: 2021-04-07                     ##
##                    Last edit: 2021-04-07                    ##
#################################################################

### Packages that are used
## API packages
require(httr)
require(jsonlite)

## Data processing
require(tidyr)
require(dplyr)

### Creates functions that are used multiple times 
readAPI <- function(url, ...){
  temp <- 
    url %>% 
    # Gets the API information, the ... allows for specific queries with query = list()
    GET(...) 
  
  temp$content %>% 
    # Extracts the data
    rawToChar() %>% 
    # Converts it from JSON to a data frame
    fromJSON() %>% 
    return()
}



