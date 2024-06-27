require(tidyverse)
require(igraph)

set.seed(14071789)

# Next few lines are just setup and are slow, so are commented for now
#load("~/Documents/citations/philo_cite.RData")
#load("~/Documents/citations/philo_bib.RData")
philo_bib <- philo_bib |> as_tibble()
philo_cite <- philo_cite |> as_tibble()

philo_cite_years <- philo_cite |>
  select(cited_article = refs, citing_article = id) |>
  left_join(philo_bib, by = c("cited_article" = "id")) |>
  select(cited_article, citing_article, cited_year = year) |>
  left_join(philo_bib, by = c("citing_article" = "id")) |>
  select(cited_article, citing_article, cited_year, citing_year = year) |>
  filter(cited_year >= 1968, cited_year <= 2022, citing_year >= 1998, citing_year <= 2022) |>
  mutate(cited_group = floor((cited_year-3)/5) - 398)
  
cite_count_by_group <- philo_cite_years |>
  group_by(cited_group) |>
  tally()

cite_count_by_year <- philo_cite_years |>
  group_by(citing_year) |>
  tally()

cite_count_by_article <- philo_cite_years |>
  group_by(cited_article) |>
  tally() |>
  filter(n >= 30)

philo_high_cite <- philo_cite_years |>
  filter(cited_article %in% cite_count_by_article$cited_article)

citing_count_by_article <- philo_high_cite |>
  group_by(citing_article) |>
  tally() |>
  arrange(-n) |>
  filter(n >= 8)

philo_graph_generator <- philo_high_cite |>
  filter(citing_article %in% citing_count_by_article$citing_article) |>
  select(cited_article, citing_article) |>
  pivot_longer(cols = everything())

philo_graph <- graph(philo_graph_generator$value, directed = FALSE)

cite_cluster <- cluster_infomap(philo_graph)

cluster_membership <- membership(cite_cluster) %>%
  enframe() %>%
  as_tibble() |>
  select(cited_article = name, cluster = value) |>
  filter(cited_article %in% philo_high_cite$cited_article) |>
  left_join(select(philo_bib, id, longcite), by = c("cited_article" = "id")) |>
  left_join(cite_count_by_article, by = "cited_article") |>
  group_by(cluster) |>
  mutate(cluster_cites = sum(n))

cluster_membership_short <- cluster_membership |>
  select(cited_article, cluster)

philo_cite_years <- philo_cite_years |>
  left_join(cluster_membership_short, by ="cited_article")

all_cite_by_year <- philo_cite_years |>
  group_by(citing_year) |>
  tally(name = "all_cites")

cluster_cite_by_year <- philo_cite_years |>
  group_by(cluster, citing_year) |>
  tally(name = "cluster_cites") |>
  ungroup() |>
  left_join(all_cite_by_year, by = "citing_year") |>
  mutate(ratio = cluster_cites / all_cites)


top_clusters <- cluster_membership |>
  ungroup() |>
  group_by(cluster) |>
  summarise(cites = sum(n)) |>
  arrange(-cites) |>
  filter(cites >= 1000)


cluster_samples <- cluster_membership |>
  filter(cluster %in% top_clusters$cluster) |>
  arrange(-n) |>
  group_by(cluster) |>
  slice_max(order_by = n, n = 4, with_ties = FALSE) |>
  ungroup() |>
  arrange(cluster) |>
  select(cluster, longcite, cluster_cites)

top_cluster_cite_by_year <- cluster_cite_by_year |>
  filter(cluster %in% top_clusters$cluster)

the_graph <- ggplot(top_cluster_cite_by_year, aes(citing_year, ratio)) +
                      geom_point() +
                      geom_smooth(method = "lm", formula = "y ~ x") +
                      facet_wrap(~cluster, ncol=4) +
                      labs(x = "Year", y = "Proportion of Cites")

# # Up - 3, 26, 36
# # Down - 4, 9, 13
# # Hum - 7, 11
# 
# up_articles <- cluster_membership |>
#   filter(cluster %in% c(3, 26, 36)) |>
#   arrange(-n) |>
#   group_by(cluster) |>
#   slice_max(order_by = n, n = 8, with_ties = FALSE) |>
#   ungroup() |>
#   select(cited_article, cluster) |>
#   left_join(philo_cite_years, by = c("cited_article", "cluster")) |>
#   group_by(cited_article, cluster, citing_year) |>
#   tally(name = "annual_cites") |>
#   left_join(cite_count_by_year, by = "citing_year") |>
#   select(-n) |>
#   ungroup() |>
#   complete(nesting(cited_article, cluster), citing_year, fill = list(annual_cites = 0)) |>
#   left_join(philo_bib, by = c("cited_article" = "id")) |>
#   left_join(all_cite_by_year, by = "citing_year") |>
#   mutate(ratio = annual_cites / all_cites) |>
#   arrange(cluster, cited_article, citing_year) |>
#   mutate(header = paste0(cluster,". ", shortcite))
# 
# up_graph <- ggplot(up_articles, aes(citing_year, ratio)) +
#   geom_point() +
#   geom_smooth(method = "loess", formula = "y ~ x") +
#   facet_wrap(~header, ncol=4) +
#   labs(x = "Year", y = "Proportion of Cites")
# 
# up_graph
# 
# 
# down_articles <- cluster_membership |>
#   filter(cluster %in% c(4, 9, 13)) |>
#   arrange(-n) |>
#   group_by(cluster) |>
#   slice_max(order_by = n, n = 8, with_ties = FALSE) |>
#   ungroup() |>
#   select(cited_article, cluster) |>
#   left_join(philo_cite_years, by = c("cited_article", "cluster")) |>
#   group_by(cited_article, cluster, citing_year) |>
#   tally(name = "annual_cites") |>
#   left_join(cite_count_by_year, by = "citing_year") |>
#   select(-n) |>
#   ungroup() |>
#   complete(nesting(cited_article, cluster), citing_year, fill = list(annual_cites = 0)) |>
#   left_join(philo_bib, by = c("cited_article" = "id")) |>
#   left_join(all_cite_by_year, by = "citing_year") |>
#   mutate(ratio = annual_cites / all_cites) |>
#   arrange(cluster, cited_article, citing_year) |>
#   mutate(header = paste0(cluster,". ", shortcite))
# 
# down_graph <- ggplot(down_articles, aes(citing_year, ratio)) +
#   geom_point() +
#   geom_smooth(method = "loess", formula = "y ~ x") +
#   facet_wrap(~header, ncol=4) +
#   labs(x = "Year", y = "Proportion of Cites")
# 
# down_graph
# 
# hump_articles <- cluster_membership |>
#   filter(cluster %in% c(7, 11)) |>
#   arrange(-n) |>
#   group_by(cluster) |>
#   slice_max(order_by = n, n = 8, with_ties = FALSE) |>
#   ungroup() |>
#   select(cited_article, cluster) |>
#   left_join(philo_cite_years, by = c("cited_article", "cluster")) |>
#   group_by(cited_article, cluster, citing_year) |>
#   tally(name = "annual_cites") |>
#   left_join(cite_count_by_year, by = "citing_year") |>
#   select(-n) |>
#   ungroup() |>
#   complete(nesting(cited_article, cluster), citing_year, fill = list(annual_cites = 0)) |>
#   left_join(philo_bib, by = c("cited_article" = "id")) |>
#   left_join(all_cite_by_year, by = "citing_year") |>
#   mutate(ratio = annual_cites / all_cites) |>
#   arrange(cluster, cited_article, citing_year) |>
#   mutate(header = paste0(cluster,". ", shortcite))
# 
# hump_graph <- ggplot(hump_articles, aes(citing_year, ratio)) +
#   geom_point() +
#   geom_smooth(method = "loess", formula = "y ~ x") +
#   facet_wrap(~header, ncol=4) +
#   labs(x = "Year", y = "Proportion of Cites")
# 
# hump_graph


