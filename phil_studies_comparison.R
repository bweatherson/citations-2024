# Phil Studies
require(tidyverse)

load("~/Documents/citations-2024/philo_cite_with_jp.RData")
load("~/Documents/citations-2024/philo_bib_fix.RData")
year_journal <- philo_bib_fix |>
  select(id, year, journal)

active_cites <- philo_cite_with_jp |>
  rename(old = refs, new = id) |>
  left_join(year_journal, by = c("old" = "id")) |>
  rename(old_year = year, old_journal = journal) |>
  left_join(year_journal, by = c("new" = "id")) |>
  rename(new_year = year, new_journal = journal) |>
  filter(new_year >= 1980, new_year <= 2019) |>
  mutate(decade = 
           paste0(
             floor(
               new_year/10
             ) * 10, 
             "s"
           )
           ) |>
  mutate(citing_journal = 
           case_when(
             new_journal == "Philosophical Studies" ~ "PS",
             TRUE ~ "NPS"
           )
  )

cites_by_decade <- active_cites |>
  ungroup() |>
  group_by(old, decade, citing_journal) |>
  tally(name = "citations") |>
  pivot_wider(
    id_cols = c(old, decade),
    names_from = citing_journal,
    values_from = citations
  ) |>
  replace_na(list(NPS = 0, PS = 0)) |>
  mutate(all_cites = NPS + PS) |>
  ungroup() |>
  group_by(decade) |>
  slice_max(order_by = all_cites, n = 20, with_ties = FALSE) |>
  mutate(PS_prop = round(PS/all_cites, 3)) |>
  arrange(decade, PS_prop) |>
  left_join(philo_bib_fix, by = c("old" = "id"))