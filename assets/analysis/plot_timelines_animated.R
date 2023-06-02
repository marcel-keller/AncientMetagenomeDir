## Assumes already run `plot_timelines.R` and objects are in memory

x <- list(raw_environmental, raw_hostmetagenome, raw_hostsinglegenome)

dat <- lapply(x, FUN = function(y) {
  select(
    y,
    List, sample_name, publication_year, latitude, longitude,
    publication_year
  ) %>% distinct()
}) %>%
  bind_rows() %>%
  mutate(List = factor(List, levels = names(dir_colours)))


additional <- tribble(
  ~List, ~publication_year, ~latitude, ~longitude, ~count,
  "Host Associated Metagenome", 1805, -85, 180, 0,
  "Host Associated Metagenome", 1806, -85, 180, 0,
  "Host Associated Metagenome", 1807, -85, 180, 0,
  "Host Associated Metagenome", 1808, -85, 180, 0,
  "Host Associated Metagenome", 1809, -85, 180, 0,
  "Host Associated Metagenome", 2010, -85, 180, 0,
  "Host Associated Metagenome", 2011, -85, 180, 0,
  "Host Associated Metagenome", 2012, -85, 180, 0,
  "Host Associated Metagenome", 2013, -85, 180, 0,
  "Environmental Metagenome", 1805, -85, 180, 0,
  "Environmental Metagenome", 1806, -85, 180, 0,
  "Environmental Metagenome", 1807, -85, 180, 0,
  "Environmental Metagenome", 1808, -85, 180, 0,
  "Environmental Metagenome", 1809, -85, 180, 0,
  "Environmental Metagenome", 2010, -85, 180, 0,
  "Environmental Metagenome", 2011, -85, 180, 0,
  "Environmental Metagenome", 2012, -85, 180, 0,
  "Environmental Metagenome", 2013, -85, 180, 0,
  "Environmental Metagenome", 2014, -85, 180, 0,
)

dat <- dat %>%
  group_by(List, publication_year, latitude, longitude) %>%
  summarise(count = n()) %>%
  bind_rows(additional) %>%
  mutate(List = factor(List, levels = names(dir_colours)))

world_map <- map_data("world")

for (i in seq(2005, 2023, 1)) {
  print(i)
  fig <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", colour = "grey", size = 0.3) +
    geom_point(data = dat |> filter(publication_year <= i), aes(x = longitude, y = latitude, fill = List, size = count), shape = 21, alpha = 0.5) +
    theme_linedraw() +
    facet_wrap(~List, ncol = 1) +
    theme_classic() +
    scale_fill_manual(values = dir_colours) +
    theme(legend.position = "none") +
    labs(
      title = "Geographic locations of samples",
      subtitle = paste("Year:", i),
      x = "Longitude",
      y = "Latitude",
      fill = "Sample Type",
      size = "Sample Count",
      caption = expression(paste(bold("License: "), "CC-BY 4.0. ", bold("Source: "), "AncientMetagenomeDir"))
    ) +
    guides(fill = FALSE)

  ggsave(paste0("AncientMetagenomeDir-Publication_Timeline_cumulative_", i, ".svg"),
    path = out_dir,
    plot = fig,
    device = "svg",
    units = "in",
    width = 5,
    height = 6,
    scale = 0.8
  )
}
