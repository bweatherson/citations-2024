# Most cited articles

require(tidyverse)

ps_articles <- philo_bib_fix |>
  filter(journal == "Philosophical Studies")

most_cite <- philo_cite_fix |>
  filter(refs %in% ps_articles$id) |>
  group_by(refs) |>
  tally(name = "citations") |>
  slice_max(order_by = citations, n = 50) |>
  rename(id = refs) |>
  left_join(ps_articles, by = "id") |>
  rename(wos_id = id)

save(most_cite, file = "most_cite.RData")