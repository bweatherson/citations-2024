init_year <- 1978
chart_list <- 15

change_finder <- function(init_year, chart_list){

early_list <- citation_tibble |>
  filter(old_year >= init_year, old_year <= init_year + 4) |>
  filter(new_year >= old_year, new_year <= old_year + 9) |>
  group_by(old) |>
  tally(name = "early_cites") |>
  arrange(old) |>
  slice_max(early_cites, n = chart_list, with_ties = FALSE) |>
  left_join(active_philo_bib, by = c("old" = "id"))

late_list <- citation_tibble |>
  filter(old_year >= init_year, old_year <= init_year + 4) |>
  filter(new_year >= 2017, new_year <= 2021) |>
  group_by(old) |>
  tally(name = "late_cites") |>
  arrange(old) |>
  slice_max(late_cites, n = chart_list, with_ties = FALSE) |>
  left_join(active_philo_bib, by = c("old" = "id"))

combined_list <- inner_join(
  select(early_list, old, early_cites),
  select(late_list, old, late_cites),
  by = "old") |>
  left_join(active_philo_bib, by = c("old" = "id"))

nrow(combined_list)
}

change_tibble <- crossing(old_year = 1960:2007, chart_length = 2:5 * 5) |>
  mutate(overlap = 0)

for (i in 1:nrow(change_tibble)){
  change_tibble$overlap[i] <- change_finder(change_tibble$old_year[i], change_tibble$chart_length[i])
  
}

ggplot(change_tibble, aes(x = old_year, y = overlap)) +
  geom_point() +
  facet_wrap(~chart_length, scales = "free") +
  geom_smooth()