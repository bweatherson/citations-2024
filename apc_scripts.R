# A Bunch of scripts for Age, Period, Cohort Studies

# First, we'll duplicates some stuff from find_main_articles.R, without generating the tables of highly cited articles
require(tidyverse)
require(slider)
require(stringr)

load("philo_bib_fix.RData")
load("philo_cite.RData")

start_year <- 1956
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

citation_tibble <- philo_cite |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year) |> # The next lines are new - restricting attention to 1966-end_year
  filter(new_year <= end_year, new_year >= start_year, old_year >= start_year, old_year <= end_year)



# Now a tibble of how many times articles in year x are cited in year y

year_in_year_out <- citation_tibble |>
  group_by(old_year, new_year) |>
  tally(name = "citations") |> # Now add the 'missing' pairs
  ungroup() |>
  complete(old_year, new_year, fill = list(citations = 0))

# Tibble for raw citation age

raw_age_tibble <- citation_tibble |>
  mutate(age = new_year - old_year) |>
  group_by(age) |>
  tally(name = "count")

raw_age_plot <- raw_age_tibble |>
  ggplot(aes(x = age, y = count)) +
  geom_point() + # Using geom_line makes it not obvious how many points there are, because it is *so* straight
  xlab('Age of citation') +
  ylab('Number of citations')

raw_age_plot

# There are enough at 0 for me to count same year as 'available'. I wasn't initially going to do that, but it's 2.2 
# Now how many articles published each year, and the cumulative total, i.e., the 'available' articles

# Tibble for number of publications each year, and cumulative, or 'available'

articles_per_year <- active_philo_bib |>
  rename(old_year = year) |>
  group_by(old_year) |>
  tally(name = "articles") |>
  mutate(available = cumsum(articles))

articles_per_year_plot <- articles_per_year |>
  ggplot(aes(x = old_year, y = articles)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Number of indexed articles")

articles_per_year_plot

# Same for citations

citations_per_year <- citation_tibble |>
  group_by(new_year) |>
  tally(name = "citations") 

citations_per_year_plot <- citations_per_year |>
  ggplot(aes(x = new_year, y = citations)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Citations to indexed articles")

citations_per_year_plot

# Citations per available article

citation_rate_per_year <- citations_per_year |>
  left_join(articles_per_year, by = c("new_year" = "old_year")) |>
  mutate(mean_cites = citations/available)

citation_rate_per_year_plot <- citation_rate_per_year |>
  ggplot(aes(x = new_year, y = mean_cites)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Average number of citations to already published indexed articles")

citation_rate_per_year_plot

# How many articles each year are never cited 

list_of_cited_articles <- citation_tibble |> group_by(old) |> tally() |> arrange(old)

never_cites <- active_philo_bib |>
  arrange(id) |>
  anti_join(list_of_cited_articles, by = c("id" = "old")) |>
  group_by(year) |>
  tally(name = "never_cited") |>
  rename(old_year = year)

never_cites_graph <- never_cites |>
  ggplot(aes(x = old_year, y = never_cited)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Number of uncited articles each year")

never_cites_graph

sum(never_cites$never_cited)

percent_uncited <- never_cites |>
  left_join(articles_per_year, by = "old_year") |>
  mutate(uncited_ratio = never_cited/articles)

percent_uncited_plot <- percent_uncited |>
  ggplot(aes(x = old_year, y = uncited_ratio)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Proportion of uncited articles each year") +
  ylim(c(0,1))

percent_uncited_plot

# Plot how often articles are cited - final graph is log on both dimensions, and some jitter added

article_times_cited <- citation_tibble |>
  group_by(old) |>
  tally(name = "citations")

count_of_citations <- article_times_cited |>
  ungroup() |>
  group_by(citations) |>
  tally(name = "number_of_articles")

count_of_citations_plot <- count_of_citations |>
  ggplot(aes(x = citations, y = number_of_articles)) +
  xlab("Number of times cited") +
  ylab("Number of articles") +
  scale_x_log10() +
  scale_y_log10() +
  geom_jitter(height = 0.05)

count_of_citations_plot

# Same for number of outbound citations in each article

article_times_citing <- citation_tibble |>
  group_by(new) |>
  tally(name = "citations")

count_of_citations_out <- article_times_citing |>
  ungroup() |>
  group_by(citations) |>
  tally(name = "number_of_articles")

count_of_citations_out_plot <- count_of_citations_out |>
  ggplot(aes(x = citations, y = number_of_articles)) +
  xlab("Number of outbound citations") +
  ylab("Number of articles") +
  scale_y_log10() +
  geom_jitter(height = 0.05)

count_of_citations_out_plot

# Find the outliers

most_cited_articles <- article_times_cited |>
  slice_max(order_by = citations, n = 10) |>
  left_join(select(philo_bib_fix, id, full_cite), by = c("old" = "id"))


most_citing_articles <- article_times_citing |>
  slice_max(order_by = citations, n = 10) |>
  left_join(select(philo_bib_fix, id, full_cite), by = c("new" = "id"))

# Compare mean per year to mean per available

age_effect_tibble <- year_in_year_out |>
  filter(old_year >= start_year, old_year <= end_year + 1 - min_data - window, new_year >= start_year) |>
  left_join(articles_per_year, by = "old_year") |>
  select(-available) |>
  left_join(select(
    citation_rate_per_year, new_year, mean_cites
  ), by = "new_year") |>
  mutate(cite_ratio = citations/(articles * mean_cites))

age_effect_tibble_plot <- age_effect_tibble |>
  filter(old_year >= start_year, old_year <= end_year + 1 - min_data - window, new_year >= start_year) |>
  ggplot(aes(x = new_year, y = cite_ratio)) +
  geom_point(size = 0.5) +
  facet_wrap(~old_year, ncol = 6) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 12))

age_effect_tibble_plot

age_effect_grouped <- age_effect_tibble |>
  filter(new_year >= old_year) |>
  filter(new_year <= old_year + end_year - start_year + 1 - window - min_data) |>
  mutate(the_age = new_year - old_year) |>
  group_by(the_age) |>
  summarise(mean_effect = mean(cite_ratio))

age_effect_tibble_adj <- age_effect_tibble |>
  filter(new_year >= old_year) |>
  filter(new_year <= old_year + old_year + end_year - start_year + 1 - window - min_data) |>
  mutate(the_age = new_year - old_year) |>
  left_join(age_effect_grouped, by = "the_age")

age_effect_grouped_plot <- age_effect_grouped |>
  ggplot(aes(x = the_age, y = mean_effect)) +
  geom_point()

age_effect_grouped_plot

age_effect_everything_plot <- age_effect_tibble_adj |>
  ggplot(aes(x = the_age, y = cite_ratio, color = as.factor(old_year))) +
  geom_jitter(aes(size=(old_year==1973 | old_year ==1985)), alpha = 1) +
  scale_size_manual(values=c(0.3,3)) +
  xlab("Age of cited articles") +
  ylab("Citation ratio") +
  geom_line(aes(x = the_age, y = mean_effect), color = "black") +
  theme_minimal() +
  theme(legend.position = "none")

age_effect_everything_plot
  
year_by_year_with_effect <- year_in_year_out |>
  filter(new_year >= old_year) |>
  filter(new_year <= old_year + end_year - start_year + 1 - window - min_data) |>
  filter(old_year >= start_year, old_year <= end_year - window - min_data + 1, new_year >= start_year + min_data) |>
  mutate(the_age = new_year - old_year) |>
  left_join(age_effect_grouped, by = "the_age") |>
  left_join(
    select(
      age_effect_tibble, old_year, new_year, cite_ratio
    ), by = c("old_year", "new_year")
  ) |>
  mutate(surplus = cite_ratio - mean_effect) |>
  arrange(-surplus)

# The next one calculates the difference between each year and the average. 
# But this has odd effects at the periphery, and compares each year to something it is part of.
# Below, in yiyo_extended, I try to work out what happens when each year is compared to the other years
# This is more work because you have to calculate the 'other years' value again each time

year_by_year_average <- year_by_year_with_effect |>
  group_by(old_year) |>
  summarise(mean_surplus = mean(surplus))

year_by_year_average_plot <- year_by_year_average |>
  ggplot(aes(x = old_year, y = mean_surplus)) +
  geom_point()

year_by_year_average_plot

yiyo_extended <- year_in_year_out |>
  filter(old_year >= start_year,
         new_year >= start_year + min_data,
         old_year <= end_year + 1 - min_data - window,
         new_year >= old_year + window) |> # First remove years where we don't have 5 years to compare, or 5 data points
  mutate(age = new_year - old_year) |>
  filter(age <= end_year - start_year + 1 - min_data - window) |> # Again, only looking at things where there are five comparisons
  left_join(
    select(
      articles_per_year, 
      old_year,
      articles),
    by = "old_year" 
    ) |> # How many articles were published in old_year  
  left_join(
    select(
      articles_per_year, 
      old_year,
      available),
    by = c("new_year" = "old_year")
    ) |> # How many articles were out at new_year
  ungroup() |>
  group_by(new_year) |>
  mutate(cites_that_year = sum(citations)) |>
  ungroup() |>
  mutate(old_year_cite_rate = citations/articles) |>
  mutate(other_cite_rate = (cites_that_year - citations)/(available-articles)) |>
  mutate(surplus = old_year_cite_rate/other_cite_rate) |>
  group_by(age) |>
  mutate(age_effect = mean(surplus)) |>
  ungroup() |>
  mutate(age_adj_surplus = surplus - age_effect)
  
yiyo_summary <- yiyo_extended |>
  ungroup() |>
  group_by(old_year) |>
  summarise(cohort_effect = mean(age_adj_surplus)) |>
  arrange(cohort_effect)

yiyo_summary |> ggplot(aes(x = old_year, y = cohort_effect)) + geom_point()

year_to_mean <- function(x){
  yiyo_extended |>
    filter(old_year == x) |>
    ggplot(aes(x = age, y = surplus)) + 
    geom_point() +
    geom_line(aes(x = age, y = age_effect))
}

year_to_mean(1980)
