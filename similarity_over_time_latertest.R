# Find cosine of citation patterns between years
# Requires having long_bib and citation_tibble already loaded
# OK, so the weird thing is the dip right at the start
# This is earlier than the break in the Tilberg data set, but only by a little; still it is a bit earlier
# It isn't just an artefact of the method; if you rerun the whole study knocking off the first 10 years, the trend line is a straight line up
# That's what should happen; as the 0s go away from the data set
# I think the 1980s had a really small impact on the journals; or, at least, the peak did
# Though note that the result holds if we do 9 per year, or 25, or even 100
# It's actually more pronounced at 100! I don't know what's happening here.
# If you go all the way to the start it is more normal, because (obviously) the early years are really random


require(lsa)

long_article_ids <- c()

for(year in (1976:2015)){
  
  topper <- 1
  all_cited <- tribble(~old, ~n)
  
  while(nrow(all_cited) < 100){
    
    
    most_cited <- citation_tibble |>
      filter(old_year == year) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    recent_cited <- citation_tibble |>
      filter(old_year == year, new_year >= pmax(2013,((year+2022.1)/2))) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    orig_cited <- citation_tibble |>
      filter(old_year == year, new_year <= pmin(year+10,((year+2022)/2))) |>
      group_by(old) |>
      tally() |>
      ungroup() |>
      slice_max(n, n=topper)
    
    all_cited <- bind_rows(most_cited, recent_cited, orig_cited) |>
      distinct(old, .keep_all = TRUE) |>
      mutate(old_year = year)
    
    topper <- topper + 1
  }
  long_article_ids <- bind_rows(
    long_article_ids,
    slice_max(all_cited, n, n=100, with_ties = FALSE)
  )
}

long_cites <- citation_tibble |>
  filter(old %in% long_article_ids$old, old_year >= 1986, new_year >=1990, new_year <= 2022) |>
  ungroup() |>
  group_by(old, new_year) |>
  tally(name="year_cites") |>
  ungroup() |>
  complete(old, new_year, fill = list(year_cites = 0)) |>
  pivot_wider(id_cols = "old", names_from = "new_year", values_from = "year_cites") |>
  select(-old) 

cos_years <- cosine(as.matrix(long_cites)) |> 
  as.data.frame() |>
  rownames_to_column(var = "year1") |>
  pivot_longer(cols = !year1, names_to = "year2", values_to = "cos_value") |>
  mutate(year1 = as.numeric(year1), year2 = as.numeric(year2))

cos_graph <- ggplot(cos_years, aes(x = year1, y = year2, value = cos_value)) +
  geom_tile(aes(fill = cos_value))

cos_graph

cos_delayed <- cos_years |>
  filter(year2 >= 1995, year1 >= year2 - 5, year1 <= year2 - 1) |>
  group_by(year2) |>
  summarise(past_sim_score = mean(cos_value)) |>
  ggplot(aes(x = year2, y = past_sim_score)) + theme_minimal() + geom_smooth() + geom_line()

cos_delayed