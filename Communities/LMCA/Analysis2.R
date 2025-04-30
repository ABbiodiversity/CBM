library(tidyverse)
library(mapview)
library(sf)

g_drive_new <- "G:/Shared drives/ABMI Mammals/"

species <- c("White-tailed Deer", "Moose", "Gray Wolf")
# EH locations that I think are good "reference" for LMNA
locations <- c("830", "828", "858", "827", "826", "857", "825",
               "794", "793", "761", "730", "792", "760", "729",
               "791", "759", "728", "667", "697")

# Modified from MS summary, to include expansion factor to adjust for greater than expected year (=habitat, weather, timing, etc.) effects
lure <- read.csv(paste0(g_drive_new, "Data/Lure/Lure Effect From Manuscript for OSM May 2022.csv"),stringsAsFactors=FALSE) |>
  # Just gonna call an audible here and make "expansion factor" 1
  mutate(Expansion = 1) |>
  mutate(species_common_name = case_when(
    Species == "Gray.Wolf" ~ "Gray Wolf",
    Species == "White.tailed.Deer" ~ "White-tailed Deer",
    Species == "Moose" ~ "Moose",
    TRUE ~ Species
  )) |>
  filter(species_common_name %in% species) |>
  select(species_common_name, TA)

# Old google drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

df_cam_all <- read_csv(paste0(g_drive, "data/lookup/locations/abmi-cmu-nwsar-bg_public-locations_2022-01-13.csv"))

# New google drive
g_drive_new <- "G:/Shared drives/ABMI Mammals/"

# 2022 EH results
d_22 <- read_csv(paste0(g_drive_new, "Results/Density/Deployments/ABMI Ecosystem Health 2019-2023 Density By Location and Species 2025-04-09.csv")) |>
  filter(str_detect(project, "2022")) |>
  filter(species_common_name %in% species) |>
  mutate(site = str_extract(location, "^[^-]+")) |>
  filter(site %in% locations) |>
  filter(total_season_days > 39) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season_days)) |>
  ungroup() |>
  left_join(lure) |>
  mutate(lure = ifelse(str_detect(location, "NE|SW"), "Yes", "No")) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  mutate(site = str_extract(location, "^[^-]+")) |>
  group_by(project, site, species_common_name) |>
  summarise(density_km2 = mean(density_km2))

# OSM google drive
g_drive_osm <- "G:/Shared drives/OSM BADR Mammals/"

# Older EH results from OSM
d_eh_old <- read_csv(paste0(g_drive_osm, "Results/Densities to use in the summaries.csv")) |>
  filter(str_detect(project, "Ecosystem Health")) |>
  filter(treatment == "reference") |>
  pivot_longer(c(`Gray.Wolf`, Moose, `White.tailed.Deer`), names_to = "species_common_name", values_to = "density_km2") |>
  select(project, location, species_common_name, density_km2, treatment, lure) |>
  mutate(species_common_name = case_when(
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    species_common_name == "White.tailed.Deer" ~ "White-tailed Deer",
    TRUE ~ species_common_name
  )) |>
  #left_join(df_cam_all, by = c("project", "location")) |>
  mutate(site = str_extract(location, "^[^-]+")) |>
  filter(site %in% locations) |>
  left_join(lure) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  #st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  #select(project, location, species_common_name, site) |>
  group_by(project, site, species_common_name) |>
  summarise(density_km2 = mean(density_km2, na.rm = TRUE))

d_eh_all <- bind_rows(d_eh_old, d_22) |>
  mutate(fine_scale = "",
         treatment = "reference")

# BADR JEM results
d_badr <- read_csv(paste0(g_drive_osm, "Results/Densities to use in the summaries.csv")) |>
  filter(str_detect(project, "ABMI OSM")) |>
  filter(landscape_unit < 4) |>
  filter(treatment == "reference" | treatment == "high activity in situ" | treatment == "plant/mine buffer") |>
  pivot_longer(c(`Gray.Wolf`, Moose, `White.tailed.Deer`), names_to = "species_common_name", values_to = "density_km2") |>
  select(project, location, species_common_name, density_km2, treatment, fine_scale, landscape_unit, jem, lure) |>
  mutate(species_common_name = case_when(
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    species_common_name == "White.tailed.Deer" ~ "White-tailed Deer",
    TRUE ~ species_common_name
  )) |>
  mutate(fine_scale = case_when(
    fine_scale == "10-30" ~ "On",
    fine_scale == "100" ~ "Off",
    fine_scale == "300" ~ "Off",
    TRUE ~ fine_scale
  )) |>
  mutate(treatment = ifelse(treatment == "plant/mine buffer", "high activity in situ", treatment)) |>
  left_join(lure) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  group_by(project, jem, landscape_unit, treatment, fine_scale, species_common_name) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(site = paste0(landscape_unit, "_", jem)) |>
  select(project, site, treatment, fine_scale, species_common_name, density_km2)

# All ABMI together
abmi <- bind_rows(d_eh_all, d_badr) |>
  mutate(fine_scale = ifelse(is.na(fine_scale), "", fine_scale))

# Now, finally ... LMNA.

d_lmna <- dens |>
  select(-lure) |>
  rename(site = location)

all <- bind_rows(abmi, d_lmna)

wolf <- all |> filter(species_common_name == "Gray Wolf")

source("Functions/Summarise Density.R")

summary <- all |>
  #filter(species_common_name == "Moose") |>
  mutate(treatment = paste0(treatment, " ", fine_scale)) |>
  filter(!(site == "1_2D1" & treatment == "high activity in situ Off"),
         !(site == "3_1F2" & treatment == "high activity in situ On"),
         #!(site == "1_2F1" & treatment == "high activity in situ Off"),
         #!(site == "1_2F2" & treatment == "high activity in situ On")
  ) |>
  summarise_density(group_id = treatment,
                    agg_samp_per = TRUE,
                    species_col = species_common_name,
                    dens_col = density_km2,
                    conflevel = 0.95)

check <- all |>
  ungroup() |>
  filter(species_common_name == "Gray Wolf") |>
  mutate(treatment = paste0(treatment, " ", fine_scale)) |>
  select(project, site, species_common_name, treatment, density_km2) |>
  #arrange(desc(density_km2)) |>
  filter(!(site == "1_2D1" & treatment == "high activity in situ Off"),
         !(site == "3_1F2" & treatment == "high activity in situ On"),
         #!(site == "1_2F1" & treatment == "high activity in situ Off"),
         #!(site == "1_2F2" & treatment == "high activity in situ On")
         )

print(check, n = 66)

library(tidyverse)
library(boot)

# Clean treatment labels
gw_data <- check %>%
  mutate(treatment = str_trim(treatment))

# Define a function to compute mean difference between two groups
mean_diff <- function(data, indices, group1, group2) {
  d <- data[indices, ]
  mean(d$density_km2[d$treatment == group1]) -
    mean(d$density_km2[d$treatment == group2])
}

# Choose treatments to compare
group_pairs <- combn(unique(gw_data$treatment), 2, simplify = FALSE)

# Perform bootstrap for each pair
set.seed(123)
results <- map(group_pairs, function(pair) {
  boot_res <- boot(data = gw_data,
                   statistic = function(data, i) mean_diff(data, i, pair[1], pair[2]),
                   R = 10000)

  ci <- boot.ci(boot_res, type = "perc")$percent[4:5]

  tibble(
    group1 = pair[1],
    group2 = pair[2],
    mean_diff = boot_res$t0,
    CI_lower = ci[1],
    CI_upper = ci[2]
  )
})

# Combine and view
results_df_wtd <- bind_rows(results) |>
  mutate(species = "White-tailed Deer")


results_df_moose <- bind_rows(results) |>
  mutate(species = "Moose")

results_df_gw <- bind_rows(results) |>
  mutate(species = "Gray Wolf")

# Together
all_diff <- bind_rows(results_df_moose, results_df_wtd, results_df_gw) |>
  mutate(group1 = case_when(
    str_detect(group1, "Off") ~ "SAGD (Off)",
    str_detect(group1, "On") ~ "SAGD (On)",
    TRUE ~ "Low Disturbance"
  )) |>
  mutate(group2 = case_when(
    str_detect(group2, "Off") ~ "SAGD (Off)",
    str_detect(group2, "On") ~ "SAGD (On)",
    TRUE ~ "Low Disturbance"
  )) |>
  mutate(comparison = paste(group1, "vs", group2))

comparison_shapes <- c(
  "SAGD (Off) vs SAGD (On)" = 17,
  "Low Disturbance vs SAGD (On)" = 15,
  "Low Disturbance vs SAGD (Off)" = 18
)

comparison_colors <- c(
  "SAGD (Off) vs SAGD (On)" = "grey20",
  "Low Disturbance vs SAGD (On)" = "grey50",
  "Low Disturbance vs SAGD (Off)" = "grey70"
)

trim_trailing_zeros <- function(x) {
  sub("\\.?0+$", "", formatC(x, format = "f", digits = 3))
}

# Plot
ggplot(all_diff, aes(x = comparison, y = mean_diff, color = comparison, shape = comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), linewidth = 1) +
  geom_point(size = 4.5) +
  scale_y_continuous(labels = trim_trailing_zeros) +
  coord_flip() +
  facet_wrap(~ species, scales = "free_x", nrow = 1) +
  scale_color_manual(values = comparison_colors) +
  scale_shape_manual(values = comparison_shapes) +
  labs(x = NULL,
       y = "Mean Difference in Density (animals per km²)") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.line.y = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, "cm"))

ggsave("LMCA/Differences in Mean_new.png", height = 4, width = 7, dpi = 500, bg = "white")





# Plot

summary |>
  #filter(!species_common_name == "Gray Wolf") |>
  mutate(treatment = case_when(
    str_detect(treatment, "Off") ~ "SAGD\n(Off)",
    str_detect(treatment, "On") ~ "SAGD\n(On)",
    TRUE ~ "Low\nDisturbance"
  )) |>
  #mutate(across(density_avg:density_uci_0.9, ~ .x * 0.9)) |>
  ggplot(aes(x = treatment, y = density_avg)) +
  #geom_col(width = 0.65, color = "black") +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = density_lci_0.95,
                     ymax = density_uci_0.95),
                 linewidth = 0.75) +
  #scale_color_manual(values = c("darkgreen", "orange", "brown")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = expression(Density~(animals~per~km^2)),
       #title = "Animals per square km",
       #subtitle = "Number of Sites: Reference 39, SAGD On 14, SAGD Off 13",
       x = "") +
  facet_wrap(~ species_common_name, nrow = 1, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, margin = margin(0, 0, 0, 0, unit = "cm")),
        strip.text = element_text(size = 12, hjust = 0.5))

ggsave("LMCA/Results_new2.png", height = 4, width = 7, dpi = 500, bg = "white")

#-------------------------------------------------------------------------------------------------

locs <- df_cam_all |>
  mutate(site = str_extract(location, "^[^-]+")) |>
  filter(site %in% locations) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  select(Site = site) |>
  mutate(Treat = "Reference")

# Deployed camera/ARU sites - from Cris, September 2023
g_drive <- "G:/Shared drives/ABMI Camera Mammals/Projects/OSM BADR/"
sf_camaru <- st_read(paste0(g_drive, "Data/Spatial/OSM_TBM_Cam_2021_23.shp")) |>
  st_zm() |>
  st_transform(4326) |>
  filter(str_detect(WildtraxNa, "^1-|^2-|^3-")) |>
  filter(str_detect(Site_ID, "D|F|1A1|1A2|2A1|2A2")) |>
  filter(!str_detect(Site_ID, "-CA10$")) |>
  filter(deployment == "BOTH" | deployment == "CAM")

lmna_locs <- df |>
  select(location, latitude, longitude) |>
  distinct() |>
  #filter(!is.na(Treat)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(Site = case_when(
    str_detect(location, "-D") ~ "D",
    str_detect(location, "-S") ~ "S",
    str_detect(location, "A1|A4B") ~ "A1",
    TRUE ~ "A2"
  )) |>
  group_by(Site) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(geometry = st_centroid(geometry)) |>
  mutate(Treat = case_when(
    str_detect(Site, "A|S") ~ "Reference",
    str_detect(Site, "D") ~ "SAGD"
  ))

mapview(lmna_locs)

library(mapview)
mapview(sf_camaru)

mapview::mapview(locs, col.regions = "darkgreen") + mapview(sf_camaru, col.regions = "blue") + mapview(lmna_locs, col.regions = "red")

# Make layer for Cris

osm <- sf_camaru |>
  st_set_geometry(NULL) |>
  select(Site_ID, Latitude, Longitude) |>
  separate(Site_ID, into = c("lu", "jem", "cam"), sep = "-") |>
  unite(col = "Site", lu, jem, sep = "-", remove = TRUE) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  group_by(Site) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(geometry = st_centroid(geometry)) |>
  mutate(Treat = ifelse(str_detect(Site, "F|D"), "SAGD", "Reference"))

mapview(all_locs)

all_locs <- bind_rows(osm, lmna_locs, locs) |>
  st_transform(3402)

st_write(all_locs, "LMCA/Cluster Points for LMCA Analysis.shp")


