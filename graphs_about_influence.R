# A bunch of graphs about ages of citations, and attempts to see what is happening in the 1980s

require(tidyverse)
require(slider)
require(stringr)
require(knitr)

load("philo_bib.RData")
load("philo_cite.RData")

article_years <- philo_bib |>
  as_tibble() |>
  select(id, year)

article_title <- philo_bib |>
  as_tibble() |>
  select(id, author, art_title, journal) 

article_journal_year <- philo_bib |>
  select(id, year, journal) 

citation_tibble <- philo_cite |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year) |>
  filter(old_year >= 1976, old_year <= 2022, new_year >= 1976, new_year <= 2022)

first_year <- citation_tibble |>
  ungroup() |>
  group_by(old, old_year) |>
  summarise(first_cite = min(new_year), .groups = "drop")

first_year |>
  filter(old_year >= 1976, old_year <= 2015) |>
  ungroup() |>
  group_by(old_year) |>
  summarise(age = mean(first_cite - old_year)) |>
  ggplot(aes(x = old_year, y = age)) + 
  theme_minimal() + 
  geom_line() +
  labs(x = "Year of Original Publication", y = "Mean Age at First Citation")

first_year |>
  filter(first_cite <= 2022, first_cite >= 1982) |>
  ungroup() |>
  group_by(first_cite) |>
  summarise(age = mean(first_cite - old_year)) |>
  ggplot(aes(x = first_cite, y = age)) + 
  theme_minimal() + 
  geom_line() +
  labs(x = "Year", y = "Mean Age of Articles Cited for First Time")

citation_tibble |>
  left_join(select(first_year, old, first_cite), by = "old") |>
  mutate(is_new = new_year == first_cite) |>
  group_by(new_year) |>
  summarise(are_new = mean(is_new)) |>
  filter(new_year >= 1984) |>
  ggplot(aes(x = new_year, y = are_new)) + 
  theme_minimal() + 
  geom_line() +
  labs(x = "Year", y = "Proportion of Citations to Previously Uncited Articles")

citation_tibble |>
  mutate(old_enough = (new_year - old_year) >= 10) |>
  mutate(young_enough = (new_year - old_year) <= 15) |>
  mutate(decade_ago = old_enough * young_enough) |>
  filter(new_year >= 1991) |>
  group_by(new_year) |>
  summarise(decade_old = mean(decade_ago)) |>
  ggplot(aes(x = new_year, y = decade_old)) + 
  theme_minimal() + 
  geom_line() +
  labs(x = "Year", y = "Proportion of Citations to 10-15 year old Articles")

citation_tibble |>
  group_by(new_year) |>
  summarise(citations = n()) |>
  left_join(philo_bib |> group_by(year) |> summarise(articles = n()), by = c("new_year" = "year")) |>
  mutate(mean_cites = citations/articles) |>
  filter(new_year >= 1985) |>
  ggplot(aes(x = new_year, y = mean_cites)) + 
  theme_minimal() + 
  geom_line() +
  labs(x = "Year", y = "Mean Citations per Article (to indexed journals)")

cites_to_journal_years <- citation_tibble |>
  left_join(select(article_journal_year, id, journal), by = c("old" = "id")) |>
  group_by(journal, old_year, new_year) |>
  summarise(cites = n(), .groups = "drop")

all_cites_per_year <- citation_tibble |>
  group_by(new_year) |>
  summarise(all_cites = n())

period_min <- 5
period_max <- 10

eligible_cited_articles <- crossing(
  journal = summarise(philo_bib |> group_by(journal))$journal,
  new_year = (1976 + period_max): 2022) |>
  left_join(
    philo_bib |>
      group_by(journal, year) |>
      summarise(articles = n(), .groups = "drop") |>
      mutate(rolling = slider::slide_dbl(articles, sum, .before = period_max - period_min)) |>
      mutate(new_year = year + period_min), by = c("journal", "new_year")
  )

cites_to_journal_years |>
  filter(journal %in% c("Journal Of Philosophy", "Philosophical Review", "Mind", "Nous")) |>
  filter(new_year - old_year >= period_min, new_year - old_year <= period_max, new_year >= 1976 + period_max) |>
  group_by(journal, new_year) |>
  summarise(sum_cites = sum(cites), .groups = "drop") |>
  left_join(all_cites_per_year, by = "new_year") |>
  left_join(select(eligible_cited_articles, journal, new_year, rolling), by = c("journal", "new_year")) |>
  mutate(mean_cites_journal = sum_cites/rolling) |>
  ggplot(aes(x = new_year, y = mean_cites_journal, color = journal, group = journal)) + 
  geom_line() + 
  theme_minimal() +
  xlab("Citing Year") +
  ylab("Mean Cites for Articles published 5-10 years previously") +
  scale_y_log10()
  