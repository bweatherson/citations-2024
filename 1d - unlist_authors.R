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


philo_bib_fix <- philo_bib_fix_1

save(philo_bib_fix, file = "philo_bib_fix.RData")
save(philo_bib_fix_1, file = "philo_bib_unlisted.RData")
