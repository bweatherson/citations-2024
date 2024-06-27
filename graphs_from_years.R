rolling_spaghetti_graph <- function(x){
  ggplot(filter(rolling_averages, old %in% filter(
    main_bib, old_year == x
    )$old,
    new_year >= x), aes(x = new_year, y = rolling, color = graph_cite)) +
    theme_minimal() +
    geom_line() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.direction = "vertical") +
    labs(x = element_blank(),
         y = element_blank(),
         title =  paste0("Widely cited articles from ", x),
         subtitle = "Rolling five year average of citations in 100 philosophy journals") +
    scale_x_continuous(breaks = 198:202 * 10, minor_breaks = 396:404*5) +
    ylim(0, max(20, max(
      filter(rolling_averages, old %in% filter(
        main_bib, old_year == x
      )$old,
      new_year >= x)$rolling
    )))
}

rolling_facet_graph <- function(x){
  temp <- filter(rolling_averages, old_year == x, new_year >= x) |> ungroup() |> arrange(graph_cite)
  temp$graph_cite <- factor(temp$graph_cite, levels = temp$graph_cite, labels = temp$short_auth)
  background <- temp |> rename(backer = graph_cite)
  ggplot(temp, aes(x = new_year, y = rolling, color = graph_cite)) +
    theme_minimal() +
    geom_line() +
    geom_line(data = background, aes(x = new_year, y = rolling, group = backer), color = "grey80", alpha = 0.5) +
    theme(legend.position="none") +
    labs(x = element_blank(),
         y = element_blank()) +
    scale_x_continuous(breaks = 198:202 * 10, minor_breaks = 396:404*5) +
    facet_wrap(vars(graph_cite)) +
    ylim(0, max(20, max(
      filter(rolling_averages, old %in% filter(
        main_bib, old_year == x
      )$old,
      new_year >= x)$rolling
    )))
}

temp <- main_bib |>
  group_by(short_auth, old_year) |>
  tally()