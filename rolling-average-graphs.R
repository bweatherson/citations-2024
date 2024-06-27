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

for(year in (1978:2015)){

topper <- 1
all_cited <- tribble(~old, ~n)

while(nrow(all_cited) < 5){
  
  
most_cited <- citation_tibble |>
  filter(old_year == year) |>
  group_by(old) |>
  tally() |>
  ungroup() |>
  slice_max(n, n=topper)

recent_cited <- citation_tibble |>
  filter(old_year == year, new_year >= 2013) |>
  group_by(old) |>
  tally() |>
  ungroup() |>
  slice_max(n, n=topper)

orig_cited <- citation_tibble |>
  filter(old_year == year, new_year <= year + 10) |>
  group_by(old) |>
  tally() |>
  ungroup() |>
  slice_max(n, n=topper)

all_cited <- bind_rows(most_cited, recent_cited, orig_cited) |>
  distinct(old, .keep_all = TRUE)

topper <- topper + 1
}

one_old_year <- citation_tibble |>
  filter(old %in% all_cited$old) |>
  group_by(old, new_year) |>
  tally(name = "cites") |>
  ungroup() |>
  complete(old, new_year, fill = list(cites = 0)) |>
  ungroup() |>
  group_by(old) |>
  arrange(old, new_year) |>
  mutate(rolling = slider::slide_dbl(cites, mean, .before = 4)) |>
  left_join(article_title, by = c("old" = "id")) |>
  ungroup() |>
  rowwise() |>
  mutate(graph_auth =  case_when(length(author) == 1 ~ authadjust(author[1]),
                                 length(author) == 2 ~ paste0(authadjust(author[1]), " and ", authadjust(author[2])),
                                 TRUE ~ paste0(authadjust(author[1]), ", et al"))) |>
  mutate(graph_cite = paste0(
    graph_auth,
    ", \"",
    art_title,
    "\""
  )) |>
  mutate(graph_cite = str_replace_all(
    graph_cite,
    c(
      'Derosset' = 'deRosset',
      'J Macfarlane' = 'John MacFarlane',
      'Macfarlane' = 'MacFarlane',
      'Norms Of Assertion (Knowledge Norm Of Assertion, Kna)' = 'Norms of Assertion',
      'S Street,' = 'Sharon Street,',
      'AM Smith' = 'Angela M Smith',
      'N Kolodny' = 'Niko Kolodny',
      'P Hieronymi' = 'Pamela Hieronymi',
      'C Travis' = 'Charles Travis',
      'D Pitt' = 'David Pitt',
      'J Pryor' = 'James Pryor',
      'Derose' = 'DeRose',
      'G.e. Moore And John Mcdowell' = 'G.E. Moore and John McDowell',
      'Mcgrath' = 'McGrath',
      'Trust? (Epistemic Considerations)' = 'Trust?',
      'D\'arms' = 'D\'Arms',
      'Emotions (Ethics, Propriety, Correctness)' = 'Emotions',
      'Antidotes (Reply To David Lewis)' = 'Antidotes',
      'Mind (Active Externalism)' = 'Mind',
      'Clay (Problem In Philosophy, Ontology)' = 'Clay',
      '\'Intrinsic\' (Properties, Philosophy)' = '\'Intrinsic\'',
      'Finkish Dispositions + Refutation Of Simple Conditional Analysis' = 'Finkish Dispositions',
      'Dimensionalism (Persistence Through Time, Doctrine Of Temporal Parts, Perdurance)' = 'Dimensionalism',
      'Frankfurt Attack On The Principle-Of-Alternative-Possibilities' = 'Frankfurt\'s Attack on the Principle of Alternative Possibilities',
      'Sense, Nonsense And The Senses, An Inquiry Into The Powers Of The Human Mind + 1994 Dewey Lectures At Columbia-University, Lecture 1 - The Antinomy Of Reason' = 'The Antinomy Of Reason',
      'Chance And Credence - Humean Supervenience Debugged' = 'Humean Supervenience Debugged',
      'Mckinsey' = 'McKinsey',
      '2 Modelings ' = 'Two Modelings ',
      'Moral Realism + A Form Of Ethical Naturalism' = 'Moral Realism',
      'Actualism + The Exploration And Defense Of Actualism' = 'Actualism',
      'Mcgee' = 'McGee',
      'Vanfraassen' = 'van Fraassen',
      'Putnam Paradox' = 'Putnam\'s Paradox',
      '2 Distinctions' = 'Two Distinctions',
      '3 Theses' = 'Three Theses',
      'Saints + Implications For Moral-Philosophy' = 'Saints',
      'What Is Equality .2. Equality Of Resources' = 'What is Equality? Part 2: Equality of Resources',
      '2 Notions' = 'Two Notions'
    )
  )) |>
  select(old, new_year, cites, rolling, graph_cite) 


ggplot(one_old_year, aes(x = new_year, y = rolling, color = graph_cite)) +
  theme_minimal() +
  geom_line() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.direction = "vertical") +
  labs(x = element_blank(),
       y = element_blank(),
       title = paste0("Widely cited articles from ", year),
       subtitle = "Rolling five year average of citations in 100 philosophy journals") +
  scale_x_continuous(breaks = 198:202 * 10, minor_breaks = 396:404*5) 

ggsave(filename = paste0("rolling_citation_graphs/",year,".svg"), bg = "white")
} 
