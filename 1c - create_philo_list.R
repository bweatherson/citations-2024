require(tidyverse)
load("master_bib.RData")
load("master_cite.RData")
load("journal_list.RData") # Must create this manually

philo_bib <- master_bib %>%
  filter(journal %in% philo_list)

philo_bib <- philo_bib |>
  mutate(journal = str_replace_all(
    journal,
    c(
      "Dialogue-Canadian Philosophical Review" = "Dialogue",
      "Inquiry-An Interdisciplinary Journal Of Philosophy" = "Inquiry",
      "Topoi-An International Review Of Philosophy" = "Topoi",
      "Heythrop Journal-A Quarterly Review Of Philosophy And Theology" = "Heythrop Journal",
      "Phronesis-A Journal For Ancient Philosophy" = "Phronesis",
      "Ratio-New Series" = "Ratio",
      "Hypatia-A Journal Of Feminist Philosophy" = "Hypatia",
      "Episteme-A Journal Of Individual And Social Epistemology" = "Episteme",
      "Russell-The Journal Of The Bertrand Russell Archives" = "Russell",
      "Theoria-A Swedish Journal Of Philosophy" = "Theoria",
      "Ergo-An Open Access Journal Of Philosophy" = "Ergo",
      "Thought-A Journal Of Philosophy" = "Thought",
      "Russell-The Journal Of The Bertrand Russell Studies" = "Russell"
    )
  ))

philo_cite <- master_cite %>%
  filter(id %in% philo_bib$id) %>%
  filter(refs %in% philo_bib$id)



save(philo_bib, file = "philo_bib.RData")
save(philo_cite, file = "philo_cite.RData")

