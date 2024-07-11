require(tidyverse)

# This requires having built philo_bib_fix in fix_philo_bib.R, but *not* having run this first
# The binding at the end will create duplicates if you run this twice


jphil_total <- c()

load("~/Documents/citations-2024/j_phil_backups/jphil_7187-1619a.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_16-19b.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_11-15a.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_11-15b.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_8797.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_06-10.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_01-05.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_98-00.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_2021a.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_2021b.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations-2024/j_phil_backups/jphil_2020_2022.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)



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
                            end_of_longcite)) |>
  mutate(cite_without_year = paste0(graph_auth, 
                                    " \"",
                                    art_title,
                                    ",\" _",
                                    journal,
                                    "_ ",
                                    end_of_longcite))

philo_bib_fix_7 <- philo_bib_fix_6 |>
  bind_rows(target_articles)

philo_bib_fix <- philo_bib_fix_7
#save(philo_bib_fix, file = "philo_bib_fix.RData")



jphil_to_phil <- jphil_total |>
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
    TRUE ~ paste0(authname, " ", year)
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
  left_join(target_articles, by = "join_code")

problem_cases <- jphil_to_phil |>
  filter(is.na(end_of_longcite)) |>
  arrange(year.x, page)

jphil_bib <- jphil_total %>%
  mutate(shortcite = str_extract(authname, "(?=\\s).*")) %>%
  mutate(shortcite = str_replace(shortcite, "\\s", "")) %>%
  mutate(shortcite = paste0(shortcite, " ", year)) %>%
  arrange(shortcite) %>%
  distinct(shortcite, .keep_all = TRUE) %>%
  select(shortcite, authname, year) %>%
  rowid_to_column("newcode") %>%
  mutate(newcode = as.character(newcode)) %>%
  mutate(newcode = str_pad(newcode, width = 4, side = "left", pad = "0")) %>%
  mutate(newcode = paste0("jphil",newcode))

jphil_cites <- jphil_total %>%
  mutate(shortcite = str_extract(authname, "(?=\\s).*")) %>%
  mutate(shortcite = str_replace(shortcite, "\\s", "")) %>%
  mutate(shortcite = paste0(shortcite, " ", year)) %>%
  left_join(jphil_bib, by = "shortcite") %>%
  select(id = newid, refs = newcode)

save(jphil_bib, file = "jphil_bib.RData")
save(jphil_cites, file = "jphil_cites.RData")

