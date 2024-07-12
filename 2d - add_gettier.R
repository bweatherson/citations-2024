require(tidyverse)

load("philo_bib_fix.RData")

gettier_cites <- read.csv("gettier_cites.csv") |> 
  as_tibble() |>
  inner_join(philo_bib_fix, by = c("UT..Unique.WOS.ID." = "id")) |>
  select(id = `UT..Unique.WOS.ID.`) |>
  distinct(id) |>
  mutate(refs = "gettier1963")

gettier_bib <- tibble(
  id = "gettier1963",
  journal = "Analysis",
  year = 1963,
  art_title = "Is Justified True Belief Knowledge?",
  end_of_longcite = "23 (6): 121-123",
  auth = "Edmund L. Gettier",
  firstauth = "Gettier",
  graph_auth = "E Gettier",
  short_auth = "Gettier"
) |>
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
  mutate(shortcite = "Gettier 1963")

load("philo_cite_with_jp.RData")

philo_cite_with_jp <- philo_cite_with_jp |>
  bind_rows(gettier_cites) |>
  distinct(id, refs)

save(philo_cite_with_jp, file = "philo_cite_with_jp.RData")

philo_bib_fix <- philo_bib_fix |>
  bind_rows(gettier_bib) |>
  distinct(id, .keep_all = TRUE)

save(philo_bib_fix, file = "philo_bib_fix.RData")