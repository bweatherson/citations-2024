require(tidyverse)
require(bib2df)
load("philo_bib_fix.RData")
#load("master_bib.RData")




bibtexgen <- philo_bib_fix |>
  mutate(full_auth = str_replace_all(full_auth, ", ", " and ")) |>
  mutate(full_auth = str_replace_all(full_auth, "and and", "and")) |>
  select(BIBTEXKEY = id, YEAR = year, JOURNAL = journal, end_of_longcite, AUTHOR = full_auth, TITLE = art_title) |>
  mutate(BIBTEXKEY = str_replace(BIBTEXKEY, ":", "")) |>
  filter(BIBTEXKEY != "WOS000221743500007") |>
  filter(grepl("[0-9]$", end_of_longcite)) |>
  filter(grepl("^[0-9]", end_of_longcite)) |>
  filter(str_length(end_of_longcite) >= 8) |>
#  arrange(end_of_longcite) |>
  mutate(end_of_longcite = str_replace_all(end_of_longcite, " ", " ")) |>
#  arrange(BIBTEXKEY) |>
#  mutate(temp = str_count(end_of_longcite, " ")) |>
#  arrange(temp) |>
  separate_wider_delim(end_of_longcite, names = c("VOLUME", "NUMBER", "PAGES"), delim = " ", too_few = "align_start", too_many = "drop") |>
  filter(!is.na(PAGES)) |>
  filter(grepl("[0-9]$", PAGES)) |>
  filter(grepl("[0-9]$", VOLUME)) |>
  filter(grepl("^\\(", NUMBER)) |>
  mutate(NUMBER = str_sub(NUMBER, start = 2, end = str_length(NUMBER) - 2)) |>
  mutate(CATEGORY = "ARTICLE")

df2bib(bibtexgen, file = "autobib.bib")
