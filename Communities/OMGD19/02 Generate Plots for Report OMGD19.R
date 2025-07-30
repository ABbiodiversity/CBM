# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2023-2024 MNA Region 1 Camera Data Report
# Date:    March 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Load data
load(paste0(g_drive_cbme, "OMGD19/Data/OMGD19 Data Objects.RData"))

species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Canada Lynx", "Woodland Caribou", "Gray Wolf")

species_colours <- c(
  "#4E6E58",  # deep green (forest)
  "#A17C6B",  # muted brown (soil / fur)
  "#79929D",  # soft blue-grey (sky / water)
  "#C2B280",  # dry grass / tan
  "#5B5B5B",  # dark grey (stone / shadow)
  "#7B9E87",  # sage green
  "#A9A9A9",  # light grey (mist)
  "#D6B88A"   # light brown (field / bark)
)

names(species_colours) <- species

#-----------------------------------------------------------------------------------------------------------------------

# Locations summary

location_ranges <- df_od |>
  group_by(project_location) |>
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") |>
  mutate(project_location = forcats::fct_reorder(project_location, start_date)) |>
  separate(project_location, into = c("project", "location"), sep = "_")

ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location),
               linewidth = 2, color = "steelblue") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Camera Location",
       title = "Camera Operation Periods by Location") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank()
  )

# Not going to save this plot, not very interesting for this community. But will keep the code here.


#-----------------------------------------------------------------------------------------------------------------------


# Number of Images

remove <- c("Human", "STAFF/SETUP", "NONE", "Vehicle", "Unidentified")

nimages <- main_report |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name)) |>
  group_by(species_common_name) |>
  tally() |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove,
         n > 40)

fig_nimages <- nimages |>
  filter(species_common_name %in% species) |>
  mutate(species_common_name = fct_reorder(as.factor(species_common_name), n)) |>
  ggplot(mapping = aes(x = species_common_name, y = n, fill = species_common_name)) +
  geom_col(color = "black") +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "",
       y = "Number of Images",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, margin = margin(0.75, 0, 0, 0, unit = "cm")),
        plot.title = element_text(size = 18))

# View the figure
fig_nimages

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Number of Images.png"),
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/OMGD19/Number of Images.png",
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Independent Detections

fig_ind_detect_all <- df_ind_detect |>
  filter(species_common_name %in% species) |>
  ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
  geom_histogram(bins = 50) +
  labs(x = "",
       y = "Number of Detections") +
  facet_wrap(~ species_common_name, scales = "free_y", nrow = 4) +
  scale_y_continuous(
    breaks = function(x) {
      if (max(x, na.rm = TRUE) <= 1.5) {
        c(0, 1)
      } else {
        scales::breaks_extended()(x, 4)
      }
    }
  ) +
  scale_fill_manual(values = species_colours) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        strip.text = element_text(size = 14),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16))

fig_ind_detect_all

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/OMGD19/Independent Detections.png",
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Now we do each species individually

x_range <- range(df_ind_detect$start_time, na.rm = TRUE) + c(-1, 1) * lubridate::days(1)

for (sp in species) {

  p <- df_ind_detect |>
    filter(species_common_name == sp) |>
    ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
    geom_histogram(bins = 50) +
    labs(x = "",
         y = "Number of Detections") +
    scale_y_continuous(
      limits = function(x) {
        c(0, max(5, ceiling(max(x, na.rm = TRUE))))
      },
      breaks = scales::pretty_breaks(n = 5)
    ) +
    scale_x_datetime(
      limits = x_range,
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_fill_manual(values = species_colours[sp]) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 14, margin = margin(0, 0.5, 0, 0, unit = "cm")))

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/OMGD19/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------

# Density at Oilsands Stressors

dens <- df_density_sum |>
  # Put into treatments
  mutate(treatment = case_when(
    str_detect(location, "MNA19-1-") ~ "Roads",
    str_detect(location, "MNA19-2-") ~ "Roads",
    str_detect(location, "MNA19-3-") ~ "Reference",
    str_detect(location, "MNA19-4-") ~ "Reference",
    str_detect(location, "MNA19-5-") ~ "Linear Features",
    str_detect(location, "MNA19-6-") ~ "Reference"
  )) |>
  # 'JEM' codes
  mutate(jem = str_extract(location, ".*(?=-[^-]*$)")) |>
  mutate(density_km2 = ifelse(density_km2 > 5, 5, density_km2)) |>
  group_by(jem, species_common_name, treatment) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Roads")))

for (sp in species) {

  plot <- dens |>
    filter(species_common_name == sp) |>
    ggplot(mapping = aes(x = treatment, y = density_km2)) +
    geom_point(color = species_colours[sp],
               size = 3) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "",
         y = expression(Density~(Animals~per~km^2))) +
    theme_minimal()

  ggsave(paste0("./Communities/MNA Region 1/Figures/", sp, " Density Treatments.png"),
         plot,
         width = 4, height = 4, dpi = 250, bg = "white")

}


# Comparisons to ABMI BADR

g_drive_osm <- "G:/Shared drives/OSM BADR Mammals/"

# BADR JEM results
d_badr <- read_csv(paste0(g_drive_osm, "Results/Densities to use in the summaries.csv")) |>
  filter(str_detect(project, "ABMI OSM")) |>
  filter(treatment == "reference" | treatment == "dense linear features" | treatment == "roads") |>
  pivot_longer(c(`Gray.Wolf`, Moose, `White.tailed.Deer`, `Snowshoe.Hare`, Coyote, `Canada.Lynx`, `Woodland.Caribou`, `Black.Bear`), names_to = "species_common_name", values_to = "density_km2") |>
  select(project, location, species_common_name, density_km2, treatment, fine_scale, landscape_unit, jem, lure) |>
  mutate(species_common_name = case_when(
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    species_common_name == "White.tailed.Deer" ~ "White-tailed Deer",
    species_common_name == "Black.Bear" ~ "Black Bear",
    species_common_name == "Canada.Lynx" ~ "Canada Lynx",
    species_common_name == "Woodland.Caribou" ~ "Woodland Caribou",
    species_common_name == "Snowshoe.Hare" ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  mutate(fine_scale = case_when(
    fine_scale == "10-30" ~ "On",
    fine_scale == "100" ~ "Off",
    fine_scale == "300" ~ "Off",
    TRUE ~ fine_scale
  )) |>
  #left_join(lure) |>
  #mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  group_by(project, jem, landscape_unit, treatment, species_common_name) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(site = paste0(landscape_unit, "_", jem)) |>
  select(project, site, treatment, species_common_name, density_km2) |>
  mutate(treatment = case_when(
    treatment == "roads" ~ "Roads",
    treatment == "dense linear features" ~ "Linear Features",
    treatment == "reference" ~ "Reference"
  )) |>
  mutate(project = "ABMI") |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Roads")))

dens1 <- dens |>
  mutate(project = "MNA Region 1") |>
  rename(site = jem)

sum <- d_badr |>
  #filter(species_common_name == "White-tailed Deer") |>
  #filter(project == "ABMI") |>
  summarise_density(group_id = treatment,
                    agg_samp_per = TRUE,
                    species_col = species_common_name,
                    dens_col = density_km2,
                    conflevel = 0.9) |>
  rename(density_km2 = density_avg)

for (sp in species) {

  sum1 <- sum |>
    filter(species_common_name == sp)

  dens12 <- dens1 |>
    filter(species_common_name == sp)

  p <- d_badr |>
    filter(species_common_name == sp) |>
    mutate(rank = dense_rank(desc(density_km2))) |>
    filter(rank > 3) |>
    ggplot(mapping = aes(x = treatment, y = density_km2)) +
    geom_point(position = position_jitter(width = 0.05),
               alpha = 0.15,
               color = "black",
               size = 1.5) +
    geom_point(data = dens12, aes(x = treatment, y = density_km2),
               color = species_colours[sp],
               size = 3) +
    geom_point(data = sum1,
               aes(x = treatment, y = density_km2),
               color = "black",
               size = 2) +
    geom_linerange(data = sum1,
                   aes(x = treatment, ymin = density_lci_0.9, ymax = density_uci_0.9),
                   color = "black",
                   linewidth = 0.5) +
    labs(x = "",
         y = expression(Density~(Animals~per~km^2))) +
    theme_minimal() +
    theme(axis.title.y = element_text(size = 12))

  ggsave(paste0("Communities/MNA Region 1/Figures/BADR ", sp, ".png"),
         p,
         height = 4, width = 4, dpi = 250, bg = "white")

}





