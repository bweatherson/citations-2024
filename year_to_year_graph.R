long_cites_test <- citation_tibble |>
  filter(old %in% long_article_ids$old, new_year >=1980, new_year <= 2022) |>
  ungroup() |>
  group_by(old, new_year) |>
  tally(name="year_cites") |>
  ungroup() |>
  complete(old, new_year, fill = list(year_cites = 0)) |>
  pivot_wider(id_cols = "old", names_from = "new_year", values_from = "year_cites") |>
  left_join(article_title, by = c("old" = "id")) |>
  arrange(-`1990`) |>
  select(old, author, art_title, everything()) |>
  mutate(preced = `1985` + `1986` + `1987` + `1988` + `1989`) |>
  arrange(-preced) |>
  select(old, author, art_title, preced, `1990`, everything())
  
year_to_year <- function(end_year){
citation_tibble |>
  ungroup() |>
  group_by(old_year, new_year) |>
  filter(new_year <= 2022) |>
  filter(old_year <= 2018) |>
  filter(old_year >= 1976) |>
  tally() |>
  left_join(
    philo_bib |>
      group_by(year) |>
      tally(name = "articles") |>
      select(old_year = year, articles),
    by = "old_year"
  ) |>
  mutate(mean = n/articles) |>
  filter(new_year >= end_year - 4, new_year <= end_year, old_year <= end_year -5) |>
  ggplot(aes(x = old_year, y = mean, group = as.factor(new_year), colour = as.factor(new_year))) + 
    geom_line() +
    xlab("Cited Year") +
    ylab("Mean Citations") +
    labs(colour = "Citing Year") +
    theme_minimal()
}

year_to_year(2022)