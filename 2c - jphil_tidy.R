require(tidyverse)

load("philo_bib_fix_without_jphil.RData")

load("jphil_total.RData")

jphil_total <- jphil_total |>
  unnest(citedTitle, keep_empty = TRUE) |>
  select(-doi, -citedWork) %>%
  mutate(newid = str_extract(uid, ".+(?=\\.)")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, "\\. ","")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, "\\.","")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, ",",""))

jphil_total <- jphil_total %>%
  rowwise() |>
  mutate(thename = strsplit(citedAuthor, " ")) %>%
  mutate(citedAuthor = paste0(thename[2], " ", thename[1])) %>%
  select(-thename) %>%
  mutate(citedAuthor = str_to_title(citedAuthor))


name_corrected <- read_csv("jphilauthors.csv")

jphil_total <- full_join(jphil_total, name_corrected, by = "citedAuthor") 

load("final_articles_from_lda.RData")
target_articles <- articles |>
  filter(journal == "Journal of Philosophy", year >= 1971, year <= 1974) |>
  mutate(journal = "Journal Of Philosophy") |>
  mutate(title = tools::toTitleCase(title)) |>
  slice(-87) |> # Clears out an unneeded second pub by an author in a year
  mutate(short_auth = case_when(
    auth1 == "Hector-Neri Castaneda" ~ "Castaneda",
    auth1 == "David Woodruff Smith" ~ "Woodruff Smith",
    auth1 == "Bas C. Van Fraassen" ~ "van Fraassen",
    TRUE ~ str_extract(auth1, '[^ ]+$')
  )) |>
  mutate(end_of_longcite = paste0(
    vol,
    " (",
    iss,
    "): ",
    fpage,
    "-",
    lpage
  )) |>
  select(-ID, -type, -lang, -lpage, -length, -citation, -lastlet, -adjlpage) |>
  rename (id = document) |>
  mutate(shortcite = paste0(short_auth, " ", year)) |>
  mutate(author = case_when(
    is.na(auth2) ~ auth1,
    is.na(auth3) ~ paste0(auth1, " and ", auth2),
    TRUE ~ paste0(auth1, " et al.")
  )) |>
  ungroup() |>
  select(-auth1, -auth2, -auth3, -auth4) |>
  rename(art_title = title) |>
  mutate(longcite = paste0(
    author,
    ", ",
    art_title,
    " _Journal of Philosophy_ (",
    year,
    ") ",
    end_of_longcite
  )) |>
  mutate(firstauth = short_auth) |>
  mutate(auth = author) |>
  select(-author) |>
  select(-vol, -iss, -fpage, -authall) |>
  select(end_of_longcite, id, shortcite, longcite, year, journal, art_title, auth, firstauth, everything()) |>
  mutate(graph_auth = case_when(
    auth == "Hector-Neri Castaneda" ~ "H-N Castaneda",
    auth == "Judith Jarvis Thomson" ~ "JJ Thomson",
    TRUE ~ paste0(
      str_sub(auth, 1, 1),
      " ",
      short_auth
    )
  )) |>
  mutate(graph_cite = paste0(
    graph_auth,
    ", \"",
    art_title,
    ",\""
  )) |>
  mutate(full_cite = paste0(graph_auth, 
                            " (",
                            year,
                            ") \"",
                            art_title,
                            ",\" _",
                            journal,
                            "_ ",
                            end_of_longcite,
                            ".")) |>
  mutate(cite_without_year = paste0(graph_auth, 
                                    " \"",
                                    art_title,
                                    ",\" _",
                                    journal,
                                    "_ ",
                                    end_of_longcite)) |>
  mutate(short_auth = case_when(
    id == "	10.2307_2024902" ~ "Lewis, Counterparts",
    TRUE ~ short_auth
  ))

philo_bib_fix_7 <- philo_bib_fix_6 |>
  bind_rows(target_articles)

philo_bib_fix <- philo_bib_fix_7
save(philo_bib_fix, file = "philo_bib_fix.RData")



jphil_to_phil <- jphil_total |>
  mutate(newid = case_when(
    str_sub(newid, 1,3) == "WOS" ~ newid,
    TRUE ~ paste0("WOS:",newid)
  )) |>
  inner_join(
    select(philo_bib_fix, id),
    by = c("newid" = "id")
  ) |>
  mutate(join_code = case_when(
    authname == "J Kim" & year == 1973 & (page == 0 | page == 17 | page == 217 | page == 222) ~ "Kim Causation",
    authname == "J Kim" & year == 1973 & (page == 570 | page == 571) ~ "Kim CC",
    authname == "JJ Thomson" & year == 1971 & (page == 115 | page == 120) ~ "Thomson Killing",
    authname == "JJ Thomson" & year == 1971 & (page == 771 | page == 774 | page == 781) ~ "Thomson Action",
    authname == "R Rorty" & year == 1972 & (page == 203 | page == 207 | page == 214) ~ "Rorty Functionalism",
    authname == "R Rorty" & year == 1972 & (page != 203 & page != 207 & page != 214) ~ "Rorty Lost",
    newid == "WOS:A1984TQ70900005" ~ "T Clarke 1972",
    newid == "WOS:A1992JZ70800007" ~ "B O'Shaughnessy 1973",
    newid == "WOS:A1986E325200012" ~ "B O'Shaughnessy 1973",
    newid == "WOS:A1983RU85000002" ~ "D Parfit 1971",
    newid == "WOS:A1984SU40700004" ~ "D Parfit 1971",
    TRUE ~ paste0(authname, " ", year)
  )) |>
  filter(authname != "W Quine") |>
  filter(authname != "S Kripke") |>
  filter(authname != "G Harman") |>
  filter(authname != "M Frede") |>
  filter(authname != "K Arrow") |>
  filter(authname != "J Kenneth") |>
  filter(!newid %in% c("WOS:000382179600002", 
                       "WOS:A1990CT76000003", 
                       "WOS:A1979HM08800004", 
                       "WOS:000236974200007", 
                       "WOS:000179825500001",
                       "WOS:A1994NF00600010",
                       "WOS:A1988P395800002",
                       "WOS:A1982NX22100001",
                       "WOS:A1983QK43500003",
                       "WOS:A1986A665800006",
                       "WOS:A1975AP38800005",
                       "WOS:A1980JL79900003"
  ))

target_articles <- target_articles |>
  mutate(join_code = case_when(
    id == "10.2307_2025312" ~ "Kim CC",
    id == "10.2307_2025096" ~ "Kim Causation",
    id == "10.2307_2024950" ~ "Thomson Action",
    id == "10.2307_2025335" ~ "Thomson Killing",
    id == "10.2307_2025059" ~ "Rorty Lost",
    id == "10.2307_2024888" ~ "Rorty Functionalism",
    TRUE ~ paste0(graph_auth, " ", year)
  ))

jphil_to_phil <- jphil_to_phil |>
  inner_join(target_articles, by = "join_code") |>
  rename(refs = id) |>
  rename(id = newid) |>
  select(id, refs)

load("philo_cite_fix.RData")

philo_cite_with_jp <- philo_cite_fix |>
  bind_rows(jphil_to_phil) |>
  distinct(id, refs)

save(philo_cite_with_jp, file = "philo_cite_with_jp.RData")