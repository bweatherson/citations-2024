jphil_cites_fixed <- jphil_cites |> 
  mutate(id = case_when(
    str_sub(id, 1,3) == "WOS" ~ id,
    TRUE ~ paste0("WOS:",id)
  ))

temp <- jphil_cites_fixed |> inner_join(philo_bib_fix, by = "id") |> arrange(-year)

print(temp |> group_by(year) |> tally(), n = 50)