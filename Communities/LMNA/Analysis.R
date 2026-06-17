# ----------------------------------------------------------------------------------------------------------------------

# Title:   LMNA
# Date:    March 2025
# Authors: Dave Evans, Marcus Becker

# ----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(wildrtrax) # For downloading data
library(keyring) # Storing credentials
library(ggplot2)

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# ----------------------------------------------------------------------------------------------------------------------

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"

# Source functions for density estimation via TIFC workflow
source("./Functions/Estimate Density Using TIFC.R")

# Species character strings
load(paste0(g_drive_abmi, "Data/Lookup Tables/WildTrax Species Strings.RData"))

# LMNA projects
projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(organization == "LMCA") |>
  select(project, project_id)

ids <- projects$project_id

# Download data
df <- map_df(.x = ids, .f = ~ wt_download_report(.x, sensor_id = "CAM", report = "main"))

images1 <- wt_download_report(3245, "CAM", "image_report") |>
  select(project_id, location, image_date_time, image_trigger_mode, image_fov)
images <- wt_download_report(2348, "CAM", "image_report") |>
  select(project_id, location, image_date_time, image_trigger_mode, image_fov) |>
  bind_rows(images1) |>
  left_join(projects) |>
  mutate(project = "LMNA")

check <- df |>
  select(location, longitude, latitude) |>
  distinct() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview::mapview(check)

df_clean <- df |>
  filter(!location == "LMCA-L2") |>
  left_join(projects) |>
  consolidate_tags() |>
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, species_common_name, individual_count) |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    TRUE ~ species_common_name
  ))

check <- df_clean |> group_by(species_common_name) |> tally()

# ----------------------------------------------------------------------------------------------------------------------

# Deployment time periods

# Get operating days and summarise by season for each deployment (sd = season days)
df_sd_summary <- images |>
  filter(!location == "LMCA-L2") |>
  get_operating_days(
    # Keep project
    include_project = TRUE,
    # Summarise by season
    summarise = TRUE,
    # ABMI seasons (spring, summer, winter)
    .abmi_seasons = TRUE
  )

# ----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- df_clean |>
  mutate(project = "LMNA") |>
  # First calculate time by series
  calc_time_by_series() |>
  # Next calculate time in front of camera by location, deployment, species
  sum_total_time(sd = df_sd_summary)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Get EDD Veg Category information from Google Sheets
df_vegdetdist <- read_csv("VegDetDist.csv") |>
  mutate(overall_category = "Forested_Open-Open",
         project_location = paste0("LMNA_", location)) |>
  select(-c(location, primary, secondary))

# Calculate density
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "long")

#-----------------------------------------------------------------------------------------------------------------------

species <- c("White-tailed Deer", "Moose",
             "Gray Wolf")

dens <- df_density_long |>
  filter(total_season_days >= 20,
         species_common_name %in% species) |>
  # Remove Black Bear and Winter
  filter(!(species_common_name == "Black Bear" & season == "Winter")) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season_days),
            total_days = sum(total_season_days)) |>
  ungroup() |>
  mutate(treatment = case_when(
    str_detect(location, "-A|-S") ~ "reference",
    str_detect(location, "-D") ~ "high activity in situ"
  )) |>
  mutate(fine_scale = case_when(
    str_detect(location, "-D10|-D6|-D9") ~ "On",
    str_detect(location, "-D11|D8") ~ "Off",
    TRUE ~ ""
  )) |>
  mutate(lure = "No") |>
  select(-c(total_days))

sp <- c("White.tailed.Deer", "Moose", "Gray.Wolf")

lure <- read.csv(paste0("G:/Shared drives/ABMI Mammals/Data/Lure/Lure Effect From Manuscript for OSM May 2022.csv"),stringsAsFactors=FALSE) |>
  # Just gonna call an audible here and make "expansion factor" 1
  mutate(Expansion = 1) |>
  filter(Species %in% sp) |>
  select(species_common_name = Species, TA) |>
  mutate(species_common_name = case_when(
    species_common_name == "White.tailed.Deer"~ "White-tailed Deer",
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    TRUE ~ species_common_name
  ))

abmi <- read_csv(paste0("G:/Shared drives/OSM BADR Mammals/Results/Densities to use in the summaries.csv")) |>
  filter(landscape_unit == "2" | landscape_unit == "3",
         treatment == "reference" | treatment == "high activity in situ",
         str_detect(project, 'ABMI')) |>
  select(project, location, Moose, `White.tailed.Deer`, `Gray.Wolf`, treatment, fine_scale, lure) |>
  pivot_longer(c(Moose:`Gray.Wolf`), names_to = "species_common_name", values_to = "density_km2") |>
  mutate(species_common_name = case_when(
    species_common_name == "White.tailed.Deer" ~ "White-tailed Deer",
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    TRUE ~ species_common_name
  )) |>
  mutate(fine_scale = if_else(is.na(fine_scale), "", fine_scale)) |>
  left_join(lure) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2))

all <- bind_rows(dens, abmi)

summary <- all |>
  group_by(species_common_name, treatment, fine_scale) |>
  summarise(dens = mean(density_km2),
            n = n())

# Plot

summary |>
  #filter(species_common_name == "White-tailed Deer") |>
  mutate(treatment = case_when(
    fine_scale == "Off" ~ "SAGD\n(Off)",
    fine_scale == "On" ~ "SAGD\n(On)",
    TRUE ~ "Ref"
  )) |>
  #mutate(across(density_avg:density_uci_0.9, ~ .x * 0.9)) |>
  ggplot(aes(x = treatment, y = dens, fill = treatment)) +
  geom_col(width = 0.65, color = "black") +
  #geom_linerange(aes(ymin = density_lci_0.9,
  #                   ymax = density_uci_0.9),
  #              linewidth = 0.5, color = "black") +
  scale_fill_manual(values = c("darkgreen", "orange", "brown")) +
  #scale_y_continuous(limits = c(0, 5)) +
  labs(y = "",
       title = "Animals per square km",
       subtitle = "Camera sample size: Reference 30, SAGD On 9, SAGD Off 8",
       x = "") +
  facet_wrap(~ species_common_name, nrow = 1, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12))

ggsave("LMCA/Results.png", height = 5, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# JEM means

jem_means <- read_csv(paste0(g_drive, "data/processed/osm/2021-2022_osm_mean_jem_density_values_new.csv")) |>
  filter(!vegetation == "wetland") |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

on_off_jem <- jem_means |>
  filter(type == "HF",
         !treatment == "pre-insitu") |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_adj) |>
  mutate(fine_scale = replace_na(fine_scale, ""),
         treatment = str_to_title(treatment)) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads", "High Activity In Situ"))) |>
  #filter(common_name %in% species) |>
  filter(treatment == "Reference" | treatment == "High Activity In Situ") |>
  filter(mean_density < 1.5) |>
  mutate(treatment = ifelse(treatment == "High Activity In Situ", "SAGD", as.character(treatment)))


on_off_smooth <- read_csv(paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed On Off HF.csv")) |>
  select(common_name = Sp, treatment = Treat, mean_density = Mean, lci_density = q5, uci_density = q95) |>
  mutate(fine_scale = case_when(
    str_detect(treatment, "On") ~ "On",
    str_detect(treatment, "Off") ~ "Off",
    TRUE ~ "")) |>
  mutate(treatment = str_to_title(str_remove(treatment, " Off$| On$"))) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads",  "High Activity In Situ"))) |>
  filter(treatment == "Reference" | treatment == "High Activity In Situ") |>
  mutate(treatment = ifelse(treatment == "High Activity In Situ", "SAGD", as.character(treatment))) |>
  mutate(mean_density = case_when(
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "Off" ~ mean_density * 0.33,
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "On" ~ mean_density * 0.33,
    TRUE ~ mean_density
  )) |>
  mutate(lci_density = case_when(
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "Off" ~ lci_density * 0.33,
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "On" ~ lci_density * 0.33,
    TRUE ~ lci_density
  )) |>
  mutate(uci_density = case_when(
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "Off" ~ uci_density * 0.33,
    common_name == "Moose" & treatment == "SAGD" & fine_scale == "On" ~ uci_density * 0.33,
    TRUE ~ uci_density
  ))


# Make plots

# White-tailed Deer

on_off_smooth |>
  filter(common_name == "Gray Wolf") |>
  #Habitat == "Deciduous Mixedwood") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 4.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.75, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Gray.Wolf")),
             #Habitat == "Deciduous Mixedwood")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.1)) +
  geom_point(data = (dens |> filter(species_common_name == "Gray Wolf")),
             aes(x = fine_scale, y = density_km2),
             size = 4, alpha = 1, color = "cornflowerblue", shape = 17) +
  scale_color_manual(values = c("Dark Green", "#C70039", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  #scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.05)) +
  labs(x = "Placement",
       y = "Relative Abundance") +
  theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 16, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 14),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) #+
  #facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave("Woodland Caribou 2.png",
       height = 5, width = 8, dpi = 500, bg = "white")











