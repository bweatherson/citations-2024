require(tidyverse)
require(igraph)

load("philo_bib.RData")
load("philo_cite.RData")

set.seed(14071789)

modern_philo_bib <- philo_bib %>% 
  filter(year >= 1998, year < 2023)

extended_philo_bib <- philo_bib %>% 
  filter(year >= 1968, year < 2023)

modern_philo_cite <- philo_cite %>% 
  filter(id %in% modern_philo_bib$id) %>% 
  filter(refs %in% extended_philo_bib$id)

cite_counts <- modern_philo_cite %>%
  group_by(refs) %>%
  tally() 

extended_philo_bib <- extended_philo_bib %>% 
  left_join(cite_counts, by = c("id" = "refs")) %>%
  mutate_at("n", ~replace(., is.na(.), 0))

high_cited <- cite_counts %>% 
  filter(n >= 0) %>%
  arrange(-n) %>% 
  rowid_to_column(var = "myid")

mycodes <- high_cited %>% 
  select(myid, refs)

extended_philo_bib <- extended_philo_bib %>% 
  left_join(mycodes, by = c("id" = "refs")) %>%
  mutate_at("myid", ~replace(., is.na(.), 10^6)) %>% 
  arrange(myid) %>% 
  rowid_to_column("xxx") %>% 
  mutate(myid = xxx) %>% 
  select(-xxx) %>% 
  select(myid, everything()) %>% 
  rename(cite_count = n)

id_list <- extended_philo_bib %>% 
  select(myid, id)

citations_myid <- modern_philo_cite %>% 
  left_join(id_list, by = "id") %>% 
  rename(article = myid) %>% 
  left_join(id_list, by = c("refs" = "id")) %>% 
  rename(citation = myid) %>% 
  select(article, citation)

has_two_refs <- citations_myid %>%
  filter(citation %in% high_cited$myid) %>% 
  group_by(article) %>% 
  tally() %>% 
  filter(n > 1)

full_cocitation <- citations_myid %>%
  filter(citation %in% high_cited$myid) %>%
  filter(article %in% has_two_refs$article) %>% 
  group_by(article) %>%
  arrange(article, citation) %>%
  do(data.frame(t(combn(.$citation, 2))))

graph_input <- full_cocitation %>%
  group_by(X1, X2) %>%
  tally() %>%
  rename(weight = n) 

graph_input_short <- graph_input %>% filter(weight >= 3)

the_graph <- graph_from_data_frame(graph_input_short, directed = FALSE)

the_clusters <- cluster_infomap(the_graph)

the_categories <- membership(the_clusters) %>%
  enframe() %>%
  mutate(myid = as.numeric(name)) %>% 
  select(-name) %>% 
  left_join(extended_philo_bib, by = "myid") %>%
  arrange(-cite_count, value) 

the_categories$value <- as.numeric(the_categories$value)

category_sum <- the_categories %>%
  mutate(new = case_when(year < 1998 ~ 0,
                         TRUE ~ 1)) %>% 
  group_by(value) %>% 
  tally(wt=cite_count * new) %>%
  arrange(-n) %>%
  rowid_to_column("cat_num") %>% 
  rename(cat_size = n)

#good_categories <- c(4:87, 89, 99, 101, 102, 110, 119, 136)
#good_categories <- 1:2000

the_categories <- the_categories %>% 
  left_join(category_sum, by = "value") %>% 
  select(-value) %>% 
  select(myid, cat_num, longcite, year, journal, cite_count, cat_size, everything()) %>% 
  filter(is.na(cat_num) == FALSE) 

reverse_cocitation <- full_cocitation %>% 
  select(article, X2 = X1, X1 = X2)

double_cocitation <- bind_rows(full_cocitation, reverse_cocitation) %>% ungroup()

original_categories <- the_categories %>% 
  select(myid, cat_num)

r2_cocites <- double_cocitation %>% 
  filter(!X1 %in% the_categories$myid, X2 %in% the_categories$myid) %>% 
  left_join(original_categories, by = c("X2" = "myid")) %>% 
  group_by(X1) %>% 
  add_tally() %>% 
  filter(n >= 3) %>% 
  group_by(X1, cat_num) %>% 
  tally(name = "cocites") %>% 
  filter(cocites >= 2) %>% 
  group_by(X1) %>% 
  add_count(wt = cocites) %>% 
  mutate(s = cocites/n) %>% 
  select(myid = X1, cat_num, s)

r1_cocites <- original_categories %>% 
  mutate(s= 1)

round_two <- bind_rows(r1_cocites, r2_cocites) %>% 
  arrange(myid) %>% 
  left_join(extended_philo_bib, by = "myid")

round_two_short <- round_two %>% 
  select(myid, cat_num, s)

two_way_cite <- modern_philo_cite %>% 
  rename(refs = id, id = refs) %>% 
  bind_rows(modern_philo_cite) %>% 
  rename(X1 = id, X2 = refs) %>% 
  left_join(id_list, by = c("X1" = "id")) %>% 
  select(X1 = myid, X2) %>% 
  left_join(id_list, by = c("X2" = "id")) %>% 
  select(X1, X2 = myid) %>% 
  filter(!X1 %in% round_two$myid, X2 %in% round_two$myid) %>% 
  arrange(X1) %>% 
  right_join(round_two_short, by = c("X2" = "myid")) %>% 
  group_by(X1) %>% 
  add_count(wt = s) %>% 
  mutate(v = s/n) %>% 
  group_by(X1, cat_num) %>% 
  summarise(s = sum(v)) %>% 
  left_join(extended_philo_bib, by = c("X1" = "myid")) %>% 
  rename(myid = X1)

round_three <- bind_rows(round_two, two_way_cite) %>% 
  filter(year >= 1998) %>% 
  filter(year < 2023) %>% 
  arrange(-cite_count * s) %>% 
  mutate(longcite = str_replace(longcite, "-", "&#8209;")) %>% 
  mutate(longcite = str_replace(longcite, ": ", ":&nbsp;")) %>% 
  mutate(longcite = str_replace(longcite, " \\(", "&nbsp;\\("))


early_papers <- bind_rows(round_two, two_way_cite) %>% 
  filter(year < 1998) %>% 
  arrange(-cite_count * s)

category_years <- round_three %>% 
  group_by(cat_num) %>% 
  summarise(n = sum(s), y = weighted.mean(year, s), c = crossprod(cite_count, s)) %>% 
  arrange(-c) %>% 
  mutate(y_rank = row_number(-y)) %>% 
  mutate(mean_cite = c / n) %>% 
  mutate(mean_cite_rank = row_number(-mean_cite)) %>% 
  mutate(cat_class = case_when(c > 2250 ~ 1,
                               c > 500 ~ 2,
                               TRUE ~ 3)) %>% 
  group_by(cat_class) %>% 
  mutate(class_y_rank = row_number(-y))

champions <- the_categories |>
  group_by(cat_num) |>
  filter(cat_size > 416) |>
  slice(1:5)