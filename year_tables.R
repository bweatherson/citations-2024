# Generate tables for each year
year <- 1992

year_cites <- citation_tibble |>
  filter(old_year == year) |>
  group_by(old) |>
  tally(name = "all") |>
  ungroup() |> 
  left_join (citation_tibble |>
      filter(old_year == year, new_year >= 2013) |>
      group_by(old) |>
      tally(name = "late") |>
      ungroup(),
    by = "old") |>
  left_join (citation_tibble |>
               filter(old_year == year, new_year <= year + 10) |>
               group_by(old) |>
               tally(name = "early") |>
               ungroup(),
             by = "old") |>
  mutate(early = replace_na(early, 0)) |>
  mutate(late = replace_na(late, 0))

year_cites <- year_cites |>
  add_column(all_rank = min_rank(-year_cites$all)) |>
  add_column(early_rank = min_rank(-year_cites$early)) |>
  add_column(late_rank = min_rank(-year_cites$late)) |>
  filter(old %in% main_bib$old) |>
  left_join(main_bib, by = "old") |>
  select(graph_cite, short_auth, all, all_rank, early, early_rank, late, late_rank)
  

