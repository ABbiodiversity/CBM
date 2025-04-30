# ----------------------------------------------------------------------------------------------------------------------

# Title:   Estimate Density 2023-2024 MNA Region 1 Camera Data
# Date:    March 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(wildrtrax)
library(keyring)
library(googledrive)
library(googlesheets4)
library(overlap)
library(activity)

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"

# Species character strings
load(paste0(g_drive_abmi, "Data/Lookup Tables/WildTrax Species Strings.RData"))

# Authenticate into WildTrax
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

wt_auth()

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Download data

# MNA Region 1 Project(s)
projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(organization, "MNA Region 1")) |>
  select(project, project_id) # 2929

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
  select(project, location, image_date_time, species_common_name, individual_count)

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

# Other reports?

#-----------------------------------------------------------------------------------------------------------------------

# Step 2. Estimate density

# Source functions for density estimation via TIFC workflow
source("./Functions/Estimate Density Using TIFC.R")

# Deployment time periods
# First, get operating days (od):
df_od <- get_operating_days(
  image_report = image_report,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Calculate time in front of camera (TIFC)

df_tifc <- main_report |>
  # Turn 'Deer' tags into White-tailed Deer (most likely)
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Then sum by time period
  sum_total_time(tbd = df_od)

# Calculate density at each location

# EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "MNA Region 1")) |>
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
            total_days = sum(total_season_days))

#-----------------------------------------------------------------------------------------------------------------------

# Prepare data for diel

source("Functions/Date to Radian.R")

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

# Step 3. Save results for future scripts

save(df_density_sum, df_density_long, location_report, image_report, main_report, rad_time,
     file = "Communities/MNA Region 1/MNA Region 1 Data Objects.RData")

#-----------------------------------------------------------------------------------------------------------------------
