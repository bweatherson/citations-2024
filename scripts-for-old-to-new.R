# Compare citations at time to recent citations

# First some general purpose setup
require(tidyverse)
require(slider)
require(stringr)
require(lsa)

load("philo_bib_fix.RData")
load("philo_cite_with_jp.RData")

start_year <- 1965
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
  filter(new_year <= end_year, new_year >= start_year, old_year >= start_year, old_year <= end_year) |>
  mutate(range_year = floor((old_year)/5)*5) |>
  filter(old_year <= 2014)

all_early_cites <- citation_tibble |>
  filter(new_year - range_year <= 9)

all_late_cites <- citation_tibble |>
  filter(new_year >= 2020)

tally_late_cites <- all_late_cites |>
  ungroup() |>
  group_by(old) |>
  tally() |>
  arrange(desc(n))

summ_early_cites <- all_early_cites |>
  ungroup() |>
  group_by(old, old_year, range_year) |>
  summarise(e_cites = n(), .groups = "drop")

summ_late_cites <- all_late_cites |>
  ungroup() |>
  group_by(old, old_year, range_year) |>
  summarise(l_cites = n(), .groups = "drop")

combined_cites <- summ_early_cites |>
  full_join(summ_late_cites, by = c("old", "old_year", "range_year")) |>
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) |>
  mutate(
    e_score = e_cites + l_cites/1000,
    l_score = l_cites + e_cites/1000
  ) |>
  group_by(range_year) |>
  mutate(
    e_rank = min_rank(-e_score),
    l_rank = min_rank(-l_score),
    o_rank = pmin(e_rank, l_rank)
  ) |>
  ungroup() |>
  arrange(o_rank, -e_score, -l_score) |>
  group_by(range_year) |>
  mutate(f_rank = row_number()) |>
  filter(f_rank <= 100) |>
  ungroup() |>
  left_join(philo_bib_fix, by = c("old" = "id", "old_year" = "year"))

still_standing <- combined_cites |>
  filter(e_rank <= 20) |>
  select(e_cites, l_cites, auth, old_year, range_year, art_title, journal, everything()) |>
  arrange(range_year, desc(e_cites))

still_standing_by_range <- still_standing |>
  filter(l_cites >= 16) |>
  ungroup() |>
  group_by(range_year) |>
  tally()

median_new_cites_of_classics <- combined_cites |>
  ungroup() |>
  group_by(range_year) |>
  filter(e_rank <= 20) |>
  summarise(cites = median(l_cites))

all_still_standing <- c()
all_median <- c()

for (i in 4:0){
  new_citation_tibble <- citation_tibble |>
    mutate(range_year = floor((old_year-i)/5)*5+i)
  all_early_cites <- new_citation_tibble |>
    filter(new_year - range_year <= 9)
  
  all_late_cites <- new_citation_tibble |>
    filter(new_year >= 2020)
  
  tally_late_cites <- all_late_cites |>
    ungroup() |>
    group_by(old) |>
    tally() |>
    arrange(desc(n))
  
  summ_early_cites <- all_early_cites |>
    ungroup() |>
    group_by(old, old_year, range_year) |>
    summarise(e_cites = n(), .groups = "drop")
  
  summ_late_cites <- all_late_cites |>
    ungroup() |>
    group_by(old, old_year, range_year) |>
    summarise(l_cites = n(), .groups = "drop")
  
  combined_cites <- summ_early_cites |>
    full_join(summ_late_cites, by = c("old", "old_year", "range_year")) |>
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) |>
    mutate(
      e_score = e_cites + l_cites/1000,
      l_score = l_cites + e_cites/1000
    ) |>
    group_by(range_year) |>
    mutate(
      e_rank = min_rank(-e_score),
      l_rank = min_rank(-l_score),
      o_rank = pmin(e_rank, l_rank)
    ) |>
    ungroup() |>
    arrange(o_rank, -e_score, -l_score) |>
    group_by(range_year) |>
    mutate(f_rank = row_number()) |>
    filter(f_rank <= 100) |>
    ungroup() |>
    left_join(philo_bib_fix, by = c("old" = "id", "old_year" = "year"))
  
  still_standing <- combined_cites |>
    filter(e_rank <= 20) |>
    select(e_cites, l_cites, auth, old_year, range_year, art_title, journal, everything()) |>
    arrange(range_year, desc(e_cites))
  
  still_standing_by_range <- still_standing |>
    filter(l_cites >= 16) |>
    ungroup() |>
    group_by(range_year) |>
    tally()
  
  median_new_cites_of_classics <- combined_cites |>
    ungroup() |>
    group_by(range_year) |>
    filter(e_rank <= 20) |>
    summarise(cites = median(l_cites))
  
  all_still_standing <- bind_rows(all_still_standing, still_standing_by_range)
  all_median <- bind_rows(all_median, median_new_cites_of_classics)
}

ggplot(all_still_standing, aes(x=range_year, y = n)) + geom_point()
ggplot(all_median, aes(x=range_year, y = cites)) + geom_point()
