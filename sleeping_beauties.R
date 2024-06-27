# This looks for papers with long gaps between citations
# The slice at the second last line picks out papers with particularly large number of citations to lag-time 
# The papers citing Logic for Equivocators are interesting; feels more female than most of philosophy

require(tidyverse)
cited_twenty_times <- citation_tibble |>
  filter(old_year >= 1976, new_year <= 2022) |>
  group_by(old) |>
  tally(name = "cites") |>
  filter(cites >= 20)

lagger <- citation_tibble |>
  filter(old %in% cited_twenty_times$old) |>
  ungroup() |>
  group_by(old) |>
  arrange(old, new_year) |>
  mutate(sepper = new_year - lag(new_year)) |>
  ungroup() |>
  arrange(desc(sepper)) |>
  left_join(cited_twenty_times, by = "old") |>
  slice(1, 7, 8, 34, 45) |>
  left_join(philo_bib, by = c("old" = "id"))

lewis_equivocators <- citation_tibble |>
  filter(old == "WOS:A1982PN18900005") |>
  left_join(philo_bib, by = c("new" = "id"))

