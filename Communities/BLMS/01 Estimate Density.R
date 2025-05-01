# ----------------------------------------------------------------------------------------------------------------------

# Title:   BLMS
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
source("./Functions/Summarise Density.R")

# Species character strings
load(paste0(g_drive_abmi, "Data/Lookup Tables/WildTrax Species Strings.RData"))

# BLMS projects
projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(organization == "BLMS") |>
  select(project, project_id)

ids <- projects$project_id

# Download data
df <- map_df(.x = ids, .f = ~ wt_download_report(.x, sensor_id = "CAM", report = "main"))

image_report <- wt_download_report(ids, "CAM", "image_report") |>
  select(project_id, location, image_date_time, image_trigger_mode, image_fov) |>
  left_join(projects)

# Locations
location_report <- wt_download_report(project_id = ids,
                                      sensor_id = "CAM",
                                      report = "location") |>
  select(location, latitude, longitude)

main_report <- df |>
  left_join(projects) |>
  consolidate_tags() |>
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, image_id, species_common_name, individual_count) |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    TRUE ~ species_common_name
  ))

unique(df_clean$species_common_name)

# ----------------------------------------------------------------------------------------------------------------------

# Deployment time periods

# Get operating days and summarise by season for each deployment (sd = season days)
df_sd_summary <- image_report |>
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

df_tt <- main_report |>
  # First calculate time by series
  calc_time_by_series() |>
  # Next calculate time in front of camera by location, deployment, species
  sum_total_time(sd = df_sd_summary)

unique(df_tt$location)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location
proj <- projects$project

# Get EDD Veg Category information from Google Sheets

df_vegdetdist <- read_csv("./Communities/BLMS/edd.csv") |>
  mutate(overall_category = paste0(primary, "_", secondary),
         project_location = paste0(proj, "_", location)) |>
  select(-c(primary, secondary, location))

# Calculate density
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "long")

unique(df_density_long$location)

#-----------------------------------------------------------------------------------------------------------------------

species <- c("Moose", "Coyote", "Black Bear", "White-tailed Deer")

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
    str_detect(location, "^GLR") ~ "Goose Lake",
    str_detect(location, "^RW") ~ "Rubelite West"
  )) |>
  mutate(treatment = factor(treatment, levels = c("Rubelite West", "Goose Lake")))

unique(dens$location)

# Summarise density
dens_sum <- dens |>
  summarise_density(
    group_id = treatment,
    agg_samp_per = TRUE,
    species_col = species_common_name,
    dens_col = density_km2,
    conflevel = 0.9
  ) |>
  arrange(species_common_name)

#-----------------------------------------------------------------------------------------------------------------------

# Prepare data for diel

source("Functions/Date to Radian.R")

species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Canada Lynx", "Woodland Caribou", "Gray Wolf")

df <- main_report |>
  # Turn 'Deer' tags into White-tailed Deer (most likely)
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  filter(species_common_name %in% species) |>
  filter(!individual_count == "VNA") |>
  mutate(individual_count = as.numeric(individual_count),
         rad_time = posix2radian(image_date_time),
         julian = as.numeric(format(image_date_time, "%j")),
         date = as.Date(image_date_time)) |>
  uncount(weights = individual_count) |>
  left_join(location_report)

solar_time <- solartime(dat = df$image_date_time,
                        lat = df$latitude,
                        lon = df$longitude,
                        tz = -6,
                        format = "%Y-%m-%d %H:%M:%S")

rad_time <- df |>
  mutate(solar_time = solar_time$solar) |>
  select(species_common_name, image_date_time, rad_time, solar_time)

#-----------------------------------------------------------------------------------------------------------------------

# Independent Detections

species

df_ind_detect <- main_report |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name)) |>
  mutate(project_id = 3150) |>
  filter(species_common_name %in% species) |>
  wt_ind_detect(threshold = 30, units = "minutes")

#-----------------------------------------------------------------------------------------------------------------------

library(corrplot)

# Make into correct format:

corr <- df_ind_detect |>
  mutate(species_common_name = factor(species_common_name)) |>
  group_by(location, species_common_name, .drop = FALSE) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = location, names_from = species_common_name, values_from = n) |>
  select(-location)

M <- cor(corr)

#-----------------------------------------------------------------------------------------------------------------------

# Step 3. Save results for future scripts

main_report <- main_report |> select(-image_id)

save(dens_sum, dens, location_report, image_report, main_report, rad_time, M, df_ind_detect,
     file = "Communities/BLMS/BLMS Data Objects.RData")










