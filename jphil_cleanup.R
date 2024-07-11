jphil_cleanup <- jphil_total |> 
  select(-doi) |>
  unnest(citedTitle, keep_empty = TRUE) |>
  mutate(year = as.numeric(year)) |>
  mutate(page = as.numeric(page)) |>
  left_join(filter(articles, journal == "Journal of Philosophy"), by = c("year" = "year", "page" = "fpage"))

jphil_the_articles <- articles |>
  filter(journal == "Journal of Philosophy", year >= 1971, year <= 1974)

jphil_count <- jphil_the_articles |>
  group_by(year, auth1) |>
  tally() |>
  ungroup()

jphil_missing <- jphil_cleanup |>
  filter(is.na(document))