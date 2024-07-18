outbound_citations <- left_join(
  articles_per_year,
  citations_per_year,
  by = c("old_year" = "new_year")
) |>
  mutate(outbound_rate = citations/articles) |>
  mutate(outbound = round(outbound_rate, 2))

outbound_citations_plot <- outbound_citations |>
  ggplot(aes(x = old_year, y = outbound)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Outbound citations per indexed articles")

outbound_citations_plot

age_tibble <- citation_tibble |>
  mutate(age = new_year - old_year) |>
  filter(age == -4 | age == 66) |>
  left_join(select(philo_bib_fix, id, art_title), by = c("old" = "id")) |>
  rename(old_title = art_title) |>
  left_join(select(philo_bib_fix, id, art_title), by = c("new" = "id")) |>
  rename(new_title = art_title)
  
find_normal_year <- year_by_year_with_effect |>
  filter(old_year <= 2002) |>
  filter(new_year - old_year <= 20) |>
  filter(old_year > 1956) |>
  mutate(deviation = log(cite_ratio/mean_effect)^2) |>
  group_by(old_year) |>
  summarise(deviation = sum(deviation)) |>
  arrange(deviation)

the_old <- 1988
the_new <- 1991

old_articles <- filter(articles_per_year, old_year == the_old)$articles[1]
available_articles <- filter(articles_per_year, old_year == the_new)$available[1]
new_citations <- nrow(
  filter(
    citation_tibble,
    new_year == the_new
  )
)
old_to_new_citations <- nrow(
  filter(
    citation_tibble,
    new_year == the_new,
    old_year == the_old
  )
)
base_new_rate <- new_citations/available_articles
old_to_new_rate <- old_to_new_citations/old_articles
focus_citation_rate <- old_to_new_rate/base_new_rate

age_effect_tibble |>
  filter(old_year == the_old, new_year >= 1985, new_year <= 2015) |>
  ggplot(aes(x = new_year, y = cite_ratio)) +
  geom_point(size = 0.3) +
  facet_wrap(~old_year, ncol = 1) +
  xlab(element_blank()) +
  ylab("Citation Ratio")

max_ratio_finder <- age_effect_tibble |>
  group_by(old_year) |>
  summarise(maxrat = max(cite_ratio))