
require(arsenal)
require(stringdist)

forumMissing <- 
  forumAttributes$name[!((forumAttributes$name) %in% (indexAttributes$name))] %>% sort()
indexMissing <- 
  indexAttributes$name[!((indexAttributes$name) %in% (forumAttributes$name))] %>% sort()

matches <- 
  stringdistmatrix(
    forumMissing,
    indexMissing
  )

forumMatches <- 
  data.frame(
    forum = forumMissing,
    index = 
      indexMissing[
        apply(
          matches, 
          MARGIN = 1, 
          FUN = function(x){
            (x == min(x)) %>% which() %>% min()
          }
          )
        ]
  )

indexMatches <- 
  data.frame(
    forum = indexMissing,
    index = 
      forumMissing[
        apply(
          matches, 
          MARGIN = 2, 
          FUN = function(x){
            (x == min(x)) %>% which() %>% min()
          }
        )
      ]
  )
  
temp <- 
  comparedf(
    forumAttributes %>% 
      arrange(name) %>% 
      mutate(
        across(
          where(is.numeric),
          ~ as.integer(.x)
        )
      ) %>% 
      select(-team),
    indexAttributes %>% 
      arrange(name) %>% 
      select(-team),
    by = "name"
  ) %>% 
  summary() %>% 
  .$diffs.table %>% 
  arrange(name) %>% 
  rename_with(
    ~ str_replace(.x, pattern = ".x", replacement = ".Forum")
  ) %>% 
  rename_with(
    ~ str_replace(.x, pattern = ".y", replacement = ".Index")
  )



























