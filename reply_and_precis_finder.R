precis <- active_philo_bib |>
  filter(str_sub(art_title, 1, 9) == "Precis of")

precis_cites <- active_philo_cite |>
  filter(refs %in% precis$id)

replies <- active_philo_bib |>
  filter(grepl("Reply To", art_title))

replies_cites <- active_philo_cite |>
  filter(refs %in% replies$id)