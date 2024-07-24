require(tidyverse)
require(slider)
require(stringr)

load("philo_bib_fix.RData")
load("philo_cite_with_jp.RData")

start_year <- 1956
end_year <- 2021
#window <- 5
min_data <- 5
#avail_cap <- 10

# New attempt
# Two categories: available and typical
# Available means published before citing article
# Typical means published 3-10 years before citing article
# The 3 is because weird things have happened with recent cites in recent years

typical_low <- 3
typical_high <- 10

active_philo_bib <- philo_bib_fix |>
  filter(year >= start_year, year <= end_year)

active_philo_cite <- philo_cite_with_jp 

article_years <- active_philo_bib |>
  as_tibble() |>
  select(id, year)

citation_tibble <- active_philo_cite |>
  as_tibble() |>
  rename(new = id, old = refs) |>
  left_join(article_years, by = c("old" = "id")) |>
  rename(old_year = year)  |>
  left_join(article_years, by = c("new" = "id")) |>
  rename(new_year = year) |>
  filter(old_year >= start_year,
         new_year <= end_year,
         old_year >= start_year,
         new_year <= end_year) |>
  add_count(old, name = "cite_count")

# # This next bit is to filter out highly cited articles
# # Obviously not for main text, but useful for some purposes
# 
# max_cites <- 39
# active_philo_bib <- active_philo_bib |>
#   anti_join(filter(citation_tibble, cite_count > max_cites), by = c("id" = "old"))
# 
# citation_tibble <- citation_tibble |>
#   filter(cite_count <= max_cites)

# Now a tibble of how many times articles in year x are cited in year y

year_in_year_out <- citation_tibble |>
  group_by(old_year, new_year) |>
  tally(name = "citations") |> # Now add the 'missing' pairs
  ungroup() |>
  complete(old_year, new_year, fill = list(citations = 0)) 

citations_in_typical_year <- year_in_year_out |>
  mutate(age = new_year - old_year) |>
  filter(age >= typical_low, age <= typical_high) |>
  group_by(new_year) |>
  summarise(typical_citations = sum(citations)) 

citations_in_available_year <- year_in_year_out |>
  mutate(age = new_year - old_year) |>
  filter(age >= 0) |>
  group_by(new_year) |>
  summarise(available_citations = sum(citations)) 

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

# I'm going to count the 'typical' articles as those published between 3 and 10 years before the citing year
# The 'available' articles are those published before the time

# Tibble for number of publications each year, and cumulative, or 'available'

articles_per_year <- active_philo_bib |>
  rename(old_year = year) |>
  group_by(old_year) |>
  tally(name = "articles") |>
  mutate(available = cumsum(articles)) |>
  mutate(typical_articles = slide_dbl(articles, sum, .before  = typical_high) - slide_dbl(articles, sum, .before = typical_low - 1))

articles_per_year_plot <- articles_per_year |>
  ggplot(aes(x = old_year, y = articles)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Number of indexed articles")

available_plot <- articles_per_year |>
  ggplot(aes(x = old_year, y = available)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Number of available indexed articles")

typical_plot <- articles_per_year |>
  ggplot(aes(x = old_year, y = typical_articles)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Number of typical indexed articles")

# Same for citations

all_citations_per_year <- citation_tibble |>
  group_by(new_year) |>
  tally(name = "citations") 

all_citations_per_year_plot <- all_citations_per_year |>
  ggplot(aes(x = new_year, y = citations)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Citations to indexed articles")

available_citations_per_year <- citation_tibble |>
  filter(new_year >= old_year) |>
  group_by(new_year) |>
  tally(name = "citations") 

available_citations_per_year_plot <- available_citations_per_year |>
  ggplot(aes(x = new_year, y = citations)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Citations to available indexed articles")

typical_citations_per_year <- citation_tibble |>
  filter(new_year >= old_year + typical_low, new_year <= old_year + typical_high) |>
  group_by(new_year) |>
  tally(name = "citations") 

typical_citations_per_year_plot <- typical_citations_per_year |>
  ggplot(aes(x = new_year, y = citations)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Citations to indexed articles from typical years")


# Outbound citations

outbound_citations <- left_join(
  articles_per_year,
  all_citations_per_year,
  by = c("old_year" = "new_year")
) |>
  mutate(outbound_rate = citations/articles) |>
  mutate(outbound = round(outbound_rate, 2))

outbound_citations_plot <- outbound_citations |>
  ggplot(aes(x = old_year, y = outbound)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Outbound citations per indexed articles")





# Citations per typical article

available_citation_rate_per_year <- available_citations_per_year |>
  left_join(articles_per_year, by = c("new_year" = "old_year")) |>
  #filter(new_year >= start_year + typical_high) |>
  left_join(citations_in_available_year, by = "new_year") |>
  mutate(mean_cites = available_citations/available)

available_citation_rate_per_year_plot <- available_citation_rate_per_year |>
  ggplot(aes(x = new_year, y = mean_cites)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Annual citation rate of available articles.")

typical_citation_rate_per_year <- typical_citations_per_year |>
  left_join(articles_per_year, by = c("new_year" = "old_year")) |>
  #filter(new_year >= start_year + typical_high) |>
  left_join(citations_in_typical_year, by = "new_year") |>
  mutate(mean_cites = typical_citations/typical_articles)

typical_citation_rate_per_year_plot <- typical_citation_rate_per_year |>
  ggplot(aes(x = new_year, y = mean_cites)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Annual citation rate of typical articles.")

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
  ylab("Number of uncited articles published each year.")

never_cited_total <- sum(never_cites$never_cited)

percent_uncited <- never_cites |>
  left_join(articles_per_year, by = "old_year") |>
  mutate(uncited_ratio = never_cited/articles)

percent_uncited_plot <- percent_uncited |>
  ggplot(aes(x = old_year, y = uncited_ratio)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Proportion of uncited articles each year") +
  ylim(c(0,1))

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

# Find the outliers

most_cited_articles <- article_times_cited |>
  slice_max(order_by = citations, n = 10) |>
  left_join(select(philo_bib_fix, id, full_cite), by = c("old" = "id")) |>
  select(Article = full_cite, Citations = citations)


most_citing_articles <- article_times_citing |>
  slice_max(order_by = citations, n = 10) |>
  left_join(select(philo_bib_fix, id, full_cite), by = c("new" = "id")) |>
  select(Article = full_cite, `Articles Cited` = citations)



# Citations between years
ct_sum <- citation_tibble |>
  group_by(old_year, new_year) |>
  tally(name = "citations") |>
  ungroup()

# All citations to typical articles in a year
ct_all <- citation_tibble |>
  filter(new_year >= old_year + typical_low, new_year <= old_year + typical_high) |>
  group_by(new_year) |>
  tally(name = "typical_citations")

age_effect_tibble <- year_in_year_out |>
  filter(old_year >= start_year, old_year <= end_year + 1 - min_data, new_year >= start_year + typical_high) |>
  filter(new_year >= old_year) |>
  left_join(select(articles_per_year, old_year, articles), by = "old_year") |>
  left_join(select(articles_per_year, old_year, typical_articles), by = c("new_year" = "old_year")) |>
  left_join(ct_all, by = "new_year") |> 
  mutate(age = new_year - old_year) |>
  mutate(cite_ratio = (citations/articles)/(typical_citations/typical_articles))

age_effect_tibble_plot <- age_effect_tibble |>
  filter(old_year >= start_year + 1, old_year <= end_year - min_data, new_year >= start_year) |>
  ggplot(aes(x = new_year, y = cite_ratio)) +
  geom_point(size = 0.25) +
  facet_wrap(~old_year, ncol = 6) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 12))

max_ratio_finder <- age_effect_tibble |>
  group_by(old_year) |>
  summarise(maxrat = max(cite_ratio)) |>
  ggplot(aes(x = old_year, y = maxrat)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Maximum citation ratio") +
  ylim(c(0, 3))

age_at_max_ratio <- age_effect_tibble |>
  group_by(old_year) |>
  filter(cite_ratio == max(cite_ratio))

age_at_max_ratio_plot <- age_at_max_ratio |>
  filter(old_year <= 2016, old_year >= start_year + typical_high - 2) |>
  ggplot(aes(x = old_year, y = age)) +
  geom_point() +
  xlab(element_blank()) + 
  ylab("Age at maximum citation ratio") +
  scale_y_continuous(limits = c(0, 14), breaks = 1:4 * 4)
           

age_effect_grouped <- age_effect_tibble |>
  filter(new_year >= old_year) |>
  filter(new_year <= old_year + end_year - start_year + 1 - min_data) |>
  mutate(age = new_year - old_year) |>
  group_by(age) |>
  summarise(mean_effect = mean(cite_ratio))

age_effect_tibble_adj <- age_effect_tibble |>
  mutate(age = new_year - old_year) |>
  filter(age <= end_year - start_year - min_data) |>
  left_join(age_effect_grouped, by = "age")

age_effect_grouped_plot <- age_effect_grouped |>
  ggplot(aes(x = age, y = mean_effect)) +
  geom_point() +
  xlab("Article age") +
  ylab("Mean citation ratio")

age_effect_everything_plot <- age_effect_tibble_adj |>
  filter(old_year >= 1975, old_year != 1973) |>
  ggplot(aes(x = age, y = cite_ratio, color = as.factor(old_year))) +
  geom_jitter(size = 0.5, alpha = 0.7) +
  # geom_jitter(aes(size=(old_year==2008 | old_year == 1985), shape = (old_year==2008)), alpha = 1) +
  #  geom_jitter(aes(size=(old_year %in% c(1978, 1980, 1985, 1987)), alpha = 1)) +
  # scale_size_manual(values=c(0.3,2)) +
  xlab("Age of cited articles") +
  ylab("Citation ratio") +
  geom_line(aes(x = age, y = mean_effect), color = "black") +
  geom_point(aes(x = age, y = mean_effect), color = "black", size = 0.4) +
  theme(legend.position = "none")

year_by_year_with_effect <- year_in_year_out |>
  filter(new_year >= old_year) |>
  filter(new_year <= end_year) |>
  filter(old_year >= start_year, old_year <= end_year - min_data + 1, new_year >= start_year + typical_high) |>
  mutate(age = new_year - old_year) |>
  filter(age <= end_year - start_year - min_data) |>
  left_join(age_effect_grouped, by = "age") |>
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
  geom_point()  +
  geom_smooth() +
  xlab(element_blank()) +
  ylab("Mean annual citations above average") +
  scale_y_continuous(labels = scales::percent)

# year_by_year_average_plot + geom_smooth()

effect_by_age_average <- function(early, late){
  age_effect_tibble |>
    filter(age >= early, age <= late) |>
    #    add_count(old_year, name = "data_points") |>
    #    filter(data_points >= min_data) |>
    group_by(old_year) |>
    summarise(mean_ratio = mean(cite_ratio)) |>
    ggplot(aes(x = old_year, y = mean_ratio)) +
    geom_point() +
    geom_smooth() +
    xlab(element_blank()) +
    ylab(element_blank()) +
    labs(title = case_when(
      early == late ~ paste0("Citation ratio at age ", early),
      TRUE ~ paste0("Mean citation ratio from ages ",early," to ",late)))
}

effect_by_age_facet <- function(early, late){age_effect_tibble |>
    filter(age>= early, age <= late) |>
    ggplot(aes(x = old_year, y = cite_ratio)) +
    geom_point() + geom_smooth() +
    facet_wrap(~age, ncol = 4)
}

year_to_mean_plot <- function(the_year){
  age_effect_tibble_adj |>
    filter(old_year == the_year) |>
    ggplot(aes(x = age, y = cite_ratio)) +
    geom_point(size = 2, alpha = 1, color = hcl(h = (the_year-1975)*(360/43)+15, l = 65, c = 100)) +
    # geom_jitter(aes(size=(old_year==2008 | old_year == 1985), shape = (old_year==2008)), alpha = 1) +
    #  geom_jitter(aes(size=(old_year %in% c(1978, 1980, 1985, 1987)), alpha = 1)) +
    # scale_size_manual(values=c(0.3,2)) +
    xlab("Age of cited articles") +
    ylab("Citation ratio") +
    geom_line(aes(x = age, y = mean_effect), color = "black") +
    geom_point(aes(x = age, y = mean_effect), color = "black", size = 0.4) +
    theme(legend.position = "none")
}