require(tidyverse)
require(slider)
require(stringr)

load("philo_bib.RData")
load("philo_cite.RData")


article_years <- philo_bib |>
  as_tibble() |>
  select(id, year)

article_title <- philo_bib |>
  as_tibble() |>
  select(id, author, art_title) 

citation_tibble <- philo_cite |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year)

# Graph of articles per year
philo_bib |>
  group_by(year) |>
  tally() |>
  ggplot(aes(x = year, y = n)) +
    theme_minimal() +
    geom_line() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.direction = "vertical") +
    labs(x = element_blank(),
         y = element_blank(),
         title = "Number of Articles in Database Each Year") +
  scale_x_continuous(breaks = 197:202 * 10, minor_breaks = 393:404*5) 

# Graph of citations per year
citation_tibble |>
  group_by(new_year) |>
  tally() |>
  ggplot(aes(x = new_year, y = n)) +
  theme_minimal() +
  geom_line() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.direction = "vertical") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Number of Citations in Database Each Year") +
  scale_x_continuous(breaks = 197:202 * 10, minor_breaks = 393:404*5) 

article_years_summary <- article_years |>
  ungroup() |>
  group_by(year) |>
  tally(name = "yearly_articles") |>
  mutate(cumulative_articles = cumsum(yearly_articles))

mean_citations <- citation_tibble |>
  group_by(new_year) |>
  tally(name = "yearly_citations") |>
  left_join(article_years_summary, by = c("new_year" = "year")) |>
  mutate(mean_citations = yearly_citations/cumulative_articles) |>
  filter(new_year <= 2022)

ggplot(mean_citations, aes(x = new_year, y = mean_citations)) +
  theme_minimal() +
  geom_line() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.direction = "vertical") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Mean number of citations articles get each year",
       subtitle = "Number of citations in a year divided by articles published up to that year") +
  scale_x_continuous(breaks = 197:202 * 10, minor_breaks = 393:404*5) 

citation_tibble |>
  mutate(age = new_year - old_year) |>
  filter(new_year <= 2022) |>
  group_by(old_year, age) |>
  tally() |>
  ungroup() |>
  filter(old_year >=1992, old_year <= 2007) |>
  mutate(new_year = old_year + age) |>
  left_join(select(
    mean_citations, 
    new_year,
    mean_citations), by = "new_year") |>
  left_join(article_years_summary, by = c("old_year" = "year")) |>
  ggplot(aes(x = age, y = n/yearly_articles)) +
  theme_minimal() +
  geom_line() +
  labs(x = "Age",
       y = element_blank(),
       title = "How many citations articles get as they age") +
  facet_wrap(vars(old_year)) +
  geom_line(aes(x = age, y = mean_citations), color = "grey80", alpha = 50) +
  geom_line(aes(x = age, y = n/yearly_articles - mean_citations), color = "red", alpha = 50)