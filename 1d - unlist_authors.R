# Unlist philo_bib
require(tidyverse)


load("philo_bib.RData")

philo_bib_fix_1 <- philo_bib |>
  filter(journal != "Nous" | !str_detect(longcite, "Suppl") | str_detect(longcite, "Sp. Iss."))

authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

authadjust_short <- function(x){
  str_to_title(str_extract(x,".+(?=,)"))
}

philo_bib_fix_1a <- philo_bib_fix_1 |>
  mutate(new_author = list(c("temp", "temper"))) |>
  ungroup() 

for (rownum in 1:nrow(philo_bib_fix_1a)){
  philo_bib_fix_1a$new_author[rownum] = list(c(sapply(philo_bib_fix_1a$author[rownum],authadjust)))
}

philo_bib_fix_1b <- philo_bib_fix_1a |>
  rowwise() |>
  mutate(bibtex_author = paste(unlist(new_author), collapse = " and ")) |>
  mutate(display_author = knitr::combine_words(new_author)) |>
  ungroup()

philo_bib_fix_1 <- philo_bib_fix_1b

philo_bib_fix_1 <- philo_bib_fix_1 |>
  ungroup() |>
  rowwise() |>
  mutate(graph_auth =  case_when(author[1] == "Pohlhaus, Gaile, Jr." ~ "Gaile Pohlhaus Jr.",
                                 author[1] == "STALNAKE.RC" ~ "R Stalnaker",
                                 length(author) == 1 ~ authadjust(author[1]),
                                 length(author) == 2 ~ paste0(authadjust(author[1]), " and ", authadjust(author[2])),
                                 length(author) == 3 ~ paste0(authadjust(author[1]), ", ", authadjust(author[2]), ", and ", authadjust(author[3])),
                                 TRUE ~ paste0(authadjust(author[1]), ", et al"))) |>
  mutate(short_auth = case_when(author[1] == "Pohlhaus, Gaile, Jr." ~ "Pohlhaus Jr.",
                                author[1] == "STALNAKE.RC" ~ "Stalnaker",
                                length(author) == 1 ~ authadjust_short(author[1]),
                                length(author) == 2 ~ paste0(authadjust_short(author[1]), " and ", authadjust_short(author[2])),
                                TRUE ~ paste0(authadjust_short(author[1]), " et al"))) |>
  ungroup()

philo_bib_fix <- philo_bib_fix_1

save(philo_bib_fix, file = "philo_bib_fix.RData")
save(philo_bib_fix_1, file = "philo_bib_unlisted.RData")
