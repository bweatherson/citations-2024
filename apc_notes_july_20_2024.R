find_normal_year <- year_by_year_with_effect |>
  filter(old_year != 1956) |>
  mutate(deviation = log(cite_ratio/mean_effect)^2) |>
  group_by(old_year) |>
  summarise(deviation = sum(deviation)) |>
  arrange(deviation)

sample_old <- 1991
sample_new <- 1998
num_of_num <- nrow(filter(citation_tibble, old_year == sample_old, new_year == sample_new))
den_of_num <- nrow(filter(active_philo_bib, year == sample_old))
num_of_den <- nrow(filter(citation_tibble, old_year >= sample_new - typical_high, old_year <= sample_new - typical_low, new_year == sample_new))
den_of_den <- nrow(filter(active_philo_bib, year >= sample_new - typical_high, year <= sample_new - typical_low))
num_of_cite <- num_of_num / den_of_num
den_of_cite <- num_of_den / den_of_den
overall_cite <- num_of_cite / den_of_cite

temp <- citation_tibble |>
  group_by(old) |>
  summarise(cites = n()) |>
  arrange(desc(cites)) |>
  mutate(cumulative = cumsum(cites)) |>
  rowid_to_column()