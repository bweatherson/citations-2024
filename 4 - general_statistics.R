# General statistics

load("philo_bib_fix.RData")

start_year <- 1956
end_year <- 2022

journal_intro <- philo_bib_fix |>
  group_by(journal) |> 
  summarise(Articles = n(), `First Year` = min(year), `Most Recent Year` = max(year)) |>
  rename(Journal = journal)

articles_per_year_plot <- philo_bib_fix |>
  group_by(year) |>
  filter(year >= start_year, year <= end_year) |>
  tally() |>
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  
  labs(x = element_blank(),
       y = element_blank(),
       title = "Number of Articles in Database Each Year") +
  scale_x_continuous(breaks = 190:202 * 10, minor_breaks = 380:404*5) 

citations_per_available_articles_plot <- citation_tibble |>
  group_by(new_year) |>
  filter(new_year >= start_year, new_year <= end_year) |>
  tally() |>
  ggplot(aes(x = new_year, y = n)) +
  geom_line() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.direction = "vertical") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Number of Citations in Database Each Year") +
  scale_x_continuous(breaks = 190:202 * 10, minor_breaks = 380:404*5) 

main_bib_journals <-   main_bib |>
  group_by(journal) |>
  tally(name = "Articles") |>
  arrange(-Articles) |>
  rename(Journal = journal)

main_bib_recent_journals <- main_bib |>
  filter(old_year >= 2006) |>
  group_by(journal) |>
  tally(name = "Articles") |>
  arrange(-Articles) |>
  rename(Journal = journal)

rank_table_generator <- function(year){
  year_cites <- citation_tibble |>
    filter(old_year == year) |>
    group_by(old) |>
    tally(name = "all") |>
    ungroup() |> 
    left_join (citation_tibble |>
                 filter(old_year == year, new_year >= pmax(2013,((year+2022.1)/2))) |>
                 group_by(old) |>
                 tally(name = "late") |>
                 ungroup(),
               by = "old") |>
    left_join (citation_tibble |>
                 filter(old_year == year, new_year <= pmin(year+10,((year+2022)/2))) |>
                 group_by(old) |>
                 tally(name = "early") |>
                 ungroup(),
               by = "old") |>
    mutate(early = replace_na(early, 0)) |>
    mutate(late = replace_na(late, 0))
  
  year_cites |>
    add_column(all_rank = min_rank(-year_cites$all)) |>
    add_column(early_rank = min_rank(-year_cites$early)) |>
    add_column(late_rank = min_rank(-year_cites$late)) |>
    filter(old %in% main_bib$old) |>
    left_join(main_bib, by = "old") |>
    select(Article = short_auth, `Citations` = all, `Citation Rank` = all_rank, `Early Citations` = early, `Early Rank` = early_rank, `Late Citations` = late, `Late Rank` =late_rank) |>
    arrange(-Citations)
}

the_code <- c()

for (the_year in start_year:end_year){
  # Widely Cited Articles subsection
  yearly_articles <- nrow(main_bib |> filter(old_year == the_year))
  the_code <- c(the_code,
                "\n## ",
                the_year,
                " {#sec-s",
                the_year,
                "}\n\n### Widely Cited Articles {-}\n\n")
  temp <- filter(main_bib, old_year == the_year) |> rename(Articles = full_cite) |> select(Articles)
  for (i in 1:yearly_articles){
    the_code <- c(the_code, i, ". ", temp$Articles[i], "\n")
  }
  
  # Citation Count Section
  the_code <- c(the_code,
                "\n### Citation Count {-#sec-count-",
                the_year,
                "}\n\n",
                "\`\`\`{r}\n",
                "#| label: tbl-citation-count-",
                the_year,
                "\n",
                "#| tbl-cap: Citation count for widely cited articles from ",
                the_year,
                ".",
                "\n\n",
                "year_cites <- rank_table_generator(",the_year,")\n",
                "kable(select(year_cites, Article, All = Citations, Early = \`Early Citations\`, Late = \`Late Citations\`))\n",
                "\`\`\`",
                "\n\n")
  
  # Citation Rank Section
  the_code <- c(the_code,
                "\n### Citation Rank {-#sec-rank-",
                the_year,
                "}\n\n",
                "\`\`\`{r}\n",
                "#| label: tbl-citation-rank-",
                the_year,
                "\n",
                "#| tbl-cap: Citation rank for widely cited articles from ",
                the_year,
                ".",
                "\n\n",
                "kable(select(year_cites, Article, Overall = \`Citation Rank\`, Early = \`Early Rank\`, Late = \`Late Rank\`))\n",
                "\`\`\`",
                "\n\n")
  
  # Spaghetti Graph
  the_code <- c(the_code,
                "\n### Citation Trends {-#sec-trends-",
                the_year,
                "}\n\n",
                "\`\`\`{r}\n",
                "#| label: fig-citation-spaghetti-",
                the_year,
                "\n",
                "#| fig-cap: Rolling five year average of citation frequency for widely cited articles from ",
                the_year,
                ".",
                "\n",
                "#| cache: TRUE",
                "\n\n",
                "rolling_spaghetti_graph(",
                the_year,
                ")\n",
                "\`\`\`",
                "\n\n"
  )
  
  # Facet Graph
  the_code <- c(the_code,
                "\`\`\`{r}\n",
                "#| label: fig-citation-facet-",
                the_year,
                "\n",
                "#| fig-cap: Faceted version of @fig-citation-spaghetti-",
                the_year,
                ".",
                "\n",
                "#| cache: TRUE",
                "\n\n",
                "rolling_facet_graph(",
                the_year,
                ")\n",
                "\`\`\`",
                "\n\n\\newpage\n\n"
  )
}

cat(the_code, sep = "", file = "_section-2.qmd")