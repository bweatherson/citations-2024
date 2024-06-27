load("philo_bib.RData")
require(tidyverse)

journals_summary <- philo_bib |>
  group_by(journal) |>
  summarise(articles = n(), first_year = min(year), last_year = max(year))