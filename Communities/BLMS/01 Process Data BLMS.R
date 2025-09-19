# ----------------------------------------------------------------------------------------------------------------------

# Title:   Process Data BLMS Camera Data
# Date:    August 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(wildrtrax)
library(keyring)
library(googledrive)
library(googlesheets4)
library(overlap)
library(activity)
library(corrplot)

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"
# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Species character strings
load(paste0(g_drive_abmi, "Data/Lookup Tables/WildTrax Species Strings.RData"))

# Source functions
files <- list.files("Functions", full.names = TRUE)
for (file in files) {
  source(file)
}

# Authenticate into WildTrax
Sys.setenv(WT_USERNAME = "marcusabecker89",
           WT_PASSWORD = "")

wt_auth()

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Download data

# BLMS Project(s)
projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(organization, "BLMS")) |>
  select(project, project_id) # 3150

project_ids <- projects$project_id

# Main report(s)
# Note: Don't need to loop, or use purrr, since there's only 1 project ID.
main_report <- wt_download_report(project_id = project_ids,
                                  sensor_id = "CAM",
                                  report = "main",
                                  weather_cols = FALSE) |>
  left_join(projects) |>
  # Consolidate tags of same species in the same image into one row
  consolidate_tags() |>
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, species_common_name, individual_count, image_id) |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  ))

# Image report
image_report <- wt_download_report(project_id = project_ids,
                                   sensor_id = "CAM",
                                   report = "image_report",
                                   weather_cols = FALSE) |>
  left_join(projects) |>
  select(project, location, image_id, image_date_time, image_trigger_mode, image_fov)

# Locations
location_report <- wt_download_report(project_id = project_ids,
                                      sensor_id = "CAM",
                                      report = "location") |>
  select(location, latitude, longitude)

# Species of interest
species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Horse")

#-----------------------------------------------------------------------------------------------------------------------

# Step 2. Estimate density

# Deployment time periods
# First, get operating days (od):
df_od_summary <- get_operating_days(
  image_report = image_report,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Also pull the raw od
df_od <- get_operating_days(
  image_report = image_report,
  include_project = TRUE,
  summarise = FALSE,
  .abmi_seasons = TRUE
)

# Calculate time in front of camera (TIFC)

df_tifc <- main_report |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Then sum by time period
  sum_total_time(sd = df_od_summary)

# Calculate density at each location

# EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "BLMS")) |>
  select(id) |>
  pull()

# Read in EDD category data
edd_cat <- map_df(.x = sheet_id,
                  .f = ~ read_sheet(ss = .x) |>
                    mutate(location = as.character(location))) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category)) |>
  mutate(overall_category = paste0(primary_category, "_", secondary_category)) |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  select(project_location, overall_category)

# Calculate density
df_density_long <- calc_density_by_loc(tt = df_tifc,
                                       veg = edd_cat,
                                       cam_fov_angle = 40,
                                       format = "long")
# Summarise density
df_density_sum <- df_density_long |>
  # Remove seasons with less than 20 operating days
  filter(total_season_days >= 20) |>
  # Remove Black Bears in Winter
  filter(!(species_common_name == "Black Bear" & season == "Winter")) |>
  # Summarise density
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season_days),
            total_days = sum(total_season_days)) |>
  ungroup() |>
  select(-total_days)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate densities at each of the two grids
# 1. Rubellite West (RW)
# 2. Goose Lake (GL)

grid_dens <- df_density_sum |>
  mutate(grid = ifelse(str_detect(location, "^GL"), "Goose Lake", "Rubellite West")) |>
  filter(species_common_name %in% species) |>
  summarise_density(group_id = grid,
                    agg_samp_per = TRUE,
                    species_col = species_common_name,
                    dens_col = density_km2,
                    conflevel = 0.9)

#-----------------------------------------------------------------------------------------------------------------------

# Number of images

remove <- c("Human", "STAFF/SETUP", "NONE", "Vehicle", "Unidentified", "All Terrain Vehicle")

nimages <- main_report |>
  group_by(species_common_name) |>
  tally() |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove)

#-----------------------------------------------------------------------------------------------------------------------

# Prepare data for diel modeling

df <- main_report |>
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

# Generate Independent Detections

df_ind_detect <- main_report |>
  mutate(project_id = project_ids) |>
  #filter(species_common_name %in% species) |>
  wt_ind_detect(threshold = 30, units = "minutes")

#-----------------------------------------------------------------------------------------------------------------------

# Species Co-Occurrences

corr <- df_ind_detect |>
  filter(species_common_name %in% species) |>
  mutate(species_common_name = factor(species_common_name)) |>
  group_by(location, species_common_name, .drop = FALSE) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = location, names_from = species_common_name, values_from = n) |>
  select(-location)

M <- cor(corr)

#-----------------------------------------------------------------------------------------------------------------------

# Step 3. Save results for future scripts

save(df_density_sum, # Densities by location and species
     grid_dens, # Densities at each grid
     df_od_summary, # Number of operating days per location
     df_od, # Raw operating days
     location_report, # Locations
     df_ind_detect, # Independent detection
     nimages, # Number of images per species
     image_report, # Image report
     main_report, # Main report
     rad_time, # Radian time
     M, # Species Co-Occurrences
     file = paste0(g_drive_cbme, "BLMS/Data/BLMS Data Objects.RData"))

#-----------------------------------------------------------------------------------------------------------------------













