ct_sum <- citation_tibble |>
  group_by(old_year, new_year) |>
  tally(name = "citations") |>
  ungroup()

ct_all <- citation_tibble |>
  group_by(new_year) |>
  tally(name = "all_citations")

ct_other <- ct_sum |>
  left_join(ct_all, by = "new_year") |>
  mutate(other_citations = all_citations - citations) |>
  select(old_year, new_year, other_citations)

art_sum <- nrow(active_philo_bib)

pub_sum <- active_philo_bib |>
  group_by(year) |>
  tally(name = "articles") |>
  mutate(other_articles = art_sum - articles)

ct_full <- ct_sum |>
  mutate(age = new_year - old_year) |>
  select(-new_year) |>
  complete(old_year, age, fill = list(citations = 0)) |>
  mutate(new_year = old_year + age) |>
  select(old_year, new_year, age, citations) |>
  left_join(ct_all, by = "new_year") |>
  replace_na(list(all_citations = 0)) |>
  mutate(other_citations = all_citations - citations) |>
  left_join(pub_sum, by = c("new_year" = "year")) |>
  replace_na(list(articles = 0, other_articles = 0)) |>
  rename(new_articles = articles) |>
  select(-other_articles) |>
  left_join(pub_sum, by = c("old_year" = "year")) |>
  replace_na(list(articles = 0, other_articles = 0)) |>
  rename(old_articles = articles) |>
  select(-other_articles) |>
  filter(old_year >= 1956,
         new_year >= 1956,
         old_year <= 2022,
         new_year <= 2022)

age_factor <- tibble(
  age = -4:66, factor = 1
) 
  
for(step in 1:1000){
avail_articles <- ct_full |>
  left_join(age_factor, by = "age") |>
  mutate(adj_articles = old_articles * factor) |>
  filter(old_year != new_year) |>
  group_by(new_year) |>
  summarise(avail = sum(adj_articles))

age_factor <- ct_full |>
  left_join(avail_articles, by = "new_year") |>
  filter(other_citations > 0, old_articles > 0, avail > 0, new_year >= 1962) |>
  left_join(age_factor, by = "age") |>
  mutate(cite_ratio = 
           (
              citations / (old_articles * factor)
           ) 
         /
           (
             other_citations / avail
           )) |>
  group_by(age) |>
  summarise(factor = mean(cite_ratio))
}