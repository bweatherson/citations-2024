require(tidyverse)

setwd("~/Documents/citations-2024")

worklist <- list.files(pattern="^[0-9](.*)RData", recursive=TRUE) # This picks out RData files that start with digit
# If you don't put the 0-9, it cycles over master_bib and master_cite themselves
# If you don't have the ^ caret at start, it includes jphil files. The ^ means digit at start
# If you don't put the RData, you get the ones I'm trying to exclude, e.g., with lower case d in Rdata
#load("master_bib.RData")
#load("master_cite.RData")
master_bib <- c()
master_cite <- c()


authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}


for (f in worklist){
  load(f)
  new_refs <- stage_08 %>%
#    filter(length(refs) > 0) %>%
    select(id, refs) %>%
    unnest_longer(refs) %>%
    filter(!str_detect(refs, "\\.")) %>%
    unnest_longer(id)
  new_bib <- stage_08 %>%
    mutate(journal = str_to_title(journal),
           art_title = str_to_title(art_title)) %>%
    rowwise() %>%
    mutate(auth = case_when(length(author) == 1 ~ authadjust(author[1]),
                            length(author) == 2 ~ paste0(authadjust(author[1]), " and ", authadjust(author[2])),
                            TRUE ~ paste0(authadjust(author[1]), ", ", authadjust(author[2]), ", et al")
    )) %>%
    mutate(longcite = paste(
      auth, art_title, journal, bib, sep = ", "
    )) %>%
    mutate(year = as.numeric(str_sub(bib, -4))) %>%
    mutate(shortcite = paste(
      str_to_title(str_extract(author[1],".+(?=,)")),
      year,
      sep = " ")) |>
    select(id, longcite, shortcite, author, year, journal, art_title, auth) %>%
    unnest_longer(id) %>%
    rowwise() %>%
    mutate(firstauth = str_to_title(
      str_extract(
        unlist(
          author[1]
        ),".+(?=,)")))
  master_bib <- master_bib %>% 
    bind_rows(new_bib)
  master_cite <- master_cite %>% 
    bind_rows(new_refs)
}



save(master_bib, file = "master_bib.RData")
save(master_cite, file = "master_cite.RData")
