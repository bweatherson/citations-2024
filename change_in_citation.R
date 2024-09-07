# Compare citations at time to recent citations

# First some general purpose setup
require(tidyverse)
require(slider)
require(stringr)
require(lsa)

load("philo_bib_fix.RData")
load("philo_cite_with_jp.RData")

start_year <- 1960
end_year <- 2022
window <- 0
min_data <- 5

active_philo_bib <- philo_bib_fix |>
  filter(year >= start_year, year <= end_year)

authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

authadjust_short <- function(x){
  str_to_title(str_extract(x,".+(?=,)"))
}

article_years <- active_philo_bib |>
  as_tibble() |>
  select(id, year)

citation_tibble <- philo_cite_with_jp |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year) |> # The next lines are new - restricting attention to 1966-end_year
  filter(new_year <= end_year, new_year >= start_year, old_year >= start_year, old_year <= end_year)

all_early_cites <- citation_tibble |>
  filter(new_year - old_year <= 5)

all_late_cites <- citation_tibble |>
  filter(new_year >= 2012)

summ_early_cites <- all_early_cites |>
  ungroup() |>
  group_by(old, old_year) |>
  summarise(e_cites = n(), .groups = "drop")

summ_late_cites <- all_late_cites |>
  ungroup() |>
  group_by(old, old_year) |>
  summarise(l_cites = n(), .groups = "drop")

combined_cites <- summ_early_cites |>
  full_join(summ_late_cites, by = c("old", "old_year")) |>
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) |>
  mutate(
    e_score = e_cites + l_cites/1000,
    l_score = l_cites + e_cites/1000
  ) |>
  group_by(old_year) |>
  mutate(
    e_rank = min_rank(-e_score),
    l_rank = min_rank(-l_score),
    o_rank = pmin(e_rank, l_rank)
  ) |>
  ungroup() |>
  arrange(o_rank, -e_score, -l_score) |>
  group_by(old_year) |>
  mutate(f_rank = row_number()) |>
  filter(f_rank <= 30) |>
  ungroup() |>
  left_join(philo_bib_fix, by = c("old" = "id", "old_year" = "year"))

cos_scores <- tribble(~year, ~score)

for (year in 1966:2012){
  the_articles <- combined_cites |>
    filter(old_year == year)
  the_cos <- cosine(the_articles$e_cites, the_articles$l_cites) |> as.numeric()
  cos_scores <- cos_scores |>
    add_row(year = year, score = the_cos) |>
    as_tibble()
}

ggplot(cos_scores, aes(x = year, y = score)) + geom_point()

all_cites <- combined_cites |>
  ungroup() |>
  group_by(old_year) |>
  summarise(all_e_cites = sum(e_cites),
            all_l_cites = sum(l_cites))

old_to_new <- combined_cites |>
  filter(e_rank <= 10) |>
  ungroup() |>
  group_by(old_year) |>
  summarise(the_cites = sum(l_cites)) |>
  filter(old_year <= 2016)

ggplot(old_to_new, aes(x = old_year, y = the_cites)) + geom_point() + geom_smooth()