require(tidyverse)


bibtexgen <- philo_bib_fix |>
  left_join(
    select(
      master_bib, id, author
    ), by = "id") |>
  select(BIBTEXKEY = id, YEAR = year, JOURNAL = journal, end_of_longcite, AUTHOR = author, TITLE = art_title) |>
  filter(grepl("[0-9]$", end_of_longcite)) |>
  filter(grepl("^[0-9]", end_of_longcite)) |>
  filter(str_length(end_of_longcite) >= 8) |>
  arrange(end_of_longcite) |>
  separate_wider_delim(end_of_longcite, names = c("VOLUME", "NUMBER", "PAGES"), delim = " ", too_few = "align_start", too_many = "drop") |>
  filter(!is.na(PAGES)) |>
  filter(grepl("[0-9]$", PAGES)) |>
  filter(grepl("[0-9]$", VOLUME)) |>
  filter(grepl("^\\(", NUMBER)) |>
  mutate(NUMBER = str_sub(NUMBER, start = 2, end = str_length(NUMBER) - 2)) |>
  mutate(CATEGORY = "ARTICLE") |>
#  mutate(AUTHOR = str_to_title(AUTHOR)) |>
  mutate(AUTHOR = str_replace_all(
    AUTHOR,
    name_fix
  )) 

#df2bib(bibtexgen, file = "autobib.bib")
