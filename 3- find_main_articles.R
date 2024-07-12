require(tidyverse)
require(slider)
require(stringr)

load("philo_bib_fix.RData")
load("philo_cite_with_jp.RData")

authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

authadjust_short <- function(x){
  str_to_title(str_extract(x,".+(?=,)"))
}


article_years <- philo_bib_fix |>
  as_tibble() |>
  select(id, year)

article_title <- philo_bib_fix |>
  as_tibble() |>
  select(id, author = auth, art_title, journal) 

citation_tibble <- philo_cite_with_jp |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year)

main_article_ids <- c()

for(year in (1956:2020)){
  
  topper <- 1
  all_cited <- tribble(~old, ~n)
  
  while(nrow(all_cited) < 9){
    
    
    most_cited <- citation_tibble |>
      filter(old_year == year) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    recent_cited <- citation_tibble |>
      filter(old_year == year, new_year >= pmax(2013,((year+2022.1)/2))) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    orig_cited <- citation_tibble |>
      filter(old_year == year, new_year <= pmin(year+10,((year+2022)/2))) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    all_cited <- bind_rows(most_cited, recent_cited, orig_cited) |>
      distinct(old)
    
    all_cited <- citation_tibble |>
      right_join(all_cited, by = "old") |>
      group_by(old) |>
      tally() |>
      mutate(old_year = year)
    
    topper <- topper + 1
  }
  main_article_ids <- bind_rows(
    main_article_ids,
    slice_max(all_cited, n, n=9, with_ties = FALSE)
  ) 
}

main_article_ids <- main_article_ids |>
  ungroup() |>
  slice_max(n, n = 603, with_ties = FALSE)

name_fix <- c(
  'Bonjour' = 'BonJour',
  'Mcdowell' = 'McDowell',
  'Mcmullin' = 'McMullin',
  'Mcgee' = 'McGee',
  'Wedgewood' = 'Wedgwood',
  'Mcginn' = 'McGinn',
  'Mcgrath' = 'McGrath',
  'Mckinsey' = 'McKinsey',
  'Macfarlane' = 'MacFarlane',
  'Castaneda' = 'Castañeda',
  'Hajek' = 'Hájek',
  'Vaninwagen' = 'van Inwagen',
  'Vanfraassen' = 'van Fraassen',
  'D\'arms' = 'D\'Arms',
  'Godfreysmith' = 'Godfrey-Smith',
  'Vangelder' = 'van Gelder',
  'Derosset' = 'deRosset'
)

main_bib <- main_article_ids |>
  left_join(philo_bib_fix, by = c("old" = "id")) |>
  select(-longcite)