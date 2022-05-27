## Loads the API Setup
source("scripts/API/apiSetup.R")

limits <- 
  forumData %>% 
  dplyr::group_by(IIHF.Nation) %>% 
  dplyr::filter(
    TPE >= 425
  ) %>% 
  dplyr::summarize(
    n = n()
  ) %>% 
  dplyr::mutate(
    `Transfer In` = 
      dplyr::case_when(
        n < 36 ~ 3,
        n < 46 ~ 2,
        TRUE ~ 1
      ),
    `Transfer Out` = 
      dplyr::case_when(
        n < 26 ~ 0,
        n < 36 ~ 1,
        n < 46 ~ 2,
        n < 66 ~ 3,
        TRUE ~ 4
      )
  )

