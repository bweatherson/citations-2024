require(tidyverse)

load("philo_bib_fix.RData")

full_names <- read_csv("full_name.csv", show_col_types = FALSE)

philo_bib_fix_8 <- philo_bib_fix |>
  left_join(full_names, by = "auth") |>
  mutate(full_auth = case_when(
    is.na(full_auth) ~ auth,
    TRUE ~ full_auth
  ))

philo_bib_fix <- philo_bib_fix_8

save(philo_bib_fix, file = "philo_bib_fix.RData")
