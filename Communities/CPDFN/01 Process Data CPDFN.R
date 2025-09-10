# ----------------------------------------------------------------------------------------------------------------------

# Title:   Process Data 2022-2025 CPDFN Camera Data
# Date:    September 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(wildrtrax)
library(googledrive)
library(googlesheets4)
library(overlap)
library(activity)
library(corrplot)

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"
# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"
# Set path to Shared Google Drive (G Drive) - OSM BADR
g_drive_osm <- "G:/Shared drives/OSM BADR Mammals/"

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

# CPDFN Project(s)
projects <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(organization_name, "CPDFN")) |>
  select(project, project_id)

project_ids <- projects$project_id

# Main report(s)
main_reports <- map_df(.x = project_ids,
                       .f = ~ wt_download_report(
                         project_id = .x,
                         sensor_id = "CAM",
                         report = "main",
                         weather_cols = FALSE
                       )) |>
  left_join(projects) |>
  # Consolidate tags of same species in the same image into one row
  consolidate_tags() |>
  # Note: Not sure what happened to this field.
  #filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, species_common_name, individual_count, image_id,
         # Note: Keep tag comments here
         tag_comments)

# Project location combos
proj_loc <- main_reports |>
  select(project, location) |>
  distinct()



# Image reports
image_reports <- map_df(.x = project_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          reports = "image_report",
                          weather_cols = FALSE
                        )) |>
  left_join(projects) |>
  select(project, location, image_id, image_date_time, image_trigger_mode, image_fov,
         # Note: keep image comments here)
         image_comments)

# Locations
location_reports <- map_df(.x = project_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          reports = "location",
                          weather_cols = FALSE
                        )) |>
  select(project_id, location, latitude, longitude) |>
  left_join(projects) |>
  distinct()

# Other reports?

location_reports <- location_reports |>
  distinct()

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
  sum_total_time(sd = df_od_summary)

# Calculate density at each location

# EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "OMGD19")) |>
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

# Prepare data for diel modeling

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

# Generate Independent Detections

df_ind_detect <- main_report |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name)) |>
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

# Number of images

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
  filter(!species_common_name %in% remove)

#-----------------------------------------------------------------------------------------------------------------------

# Densities at OS stressors for comparison with OSM BADR regional monitoring

dens_omgd19 <- df_density_sum |>
  # Categorize into treatments
  mutate(treatment = case_when(
    str_detect(location, "MNA19-1-") ~ "Roads",
    str_detect(location, "MNA19-2-") ~ "Roads",
    str_detect(location, "MNA19-3-") ~ "Reference",
    str_detect(location, "MNA19-4-") ~ "Reference",
    str_detect(location, "MNA19-5-") ~ "Linear Features",
    str_detect(location, "MNA19-6-") ~ "Reference"
  )) |>
  # 'JEM' (i.e., site) codes
  # Extract everything from location except the last digit
  mutate(site = str_extract(location, ".*(?=-[^-]*$)")) |>
  # Pare back some outlier values
  mutate(density_km2 = ifelse(density_km2 > 5, 5, density_km2)) |>
  group_by(site, species_common_name, treatment) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Roads"))) |>
  # Add project
  mutate(project = "OMGD19 Local Monitoring")

# OSM BADR data

lure <- read_csv(paste0(g_drive_abmi, "Data/Lure/ABMI Lure Effect Summary 2024-04-24.csv")) |>
  mutate(species_common_name = str_replace_all(species_common_name, "(?<!^)([A-Z])", " \\1")) |>
  select(species_common_name, TA)

dens_badr <- read_csv(paste0(g_drive_osm, "Results/Densities to use in the summaries.csv")) |>
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
  left_join(lure) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
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
  mutate(project = "OSM Regional Monitoring") |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Roads")))

# Bind together
df_density_os <- bind_rows(dens_badr, dens_omgd19)

#-----------------------------------------------------------------------------------------------------------------------

# Step 3. Save results for future scripts

save(df_density_sum, # Densities by location and species
     df_density_os, # Densities for OS stressors (regional and local)
     df_od_summary, # Number of operating days per location
     df_od, # Raw operating days
     location_report, # Locations
     df_ind_detect, # Independent detections
     nimages, # Number of images per species
     image_report, # Image report
     main_report, # Main report
     rad_time, # Radian time
     M, # Species Co-Occurrences
     file = paste0(g_drive_cbme, "OMGD19/Data/OMGD19 Data Objects.RData"))

#-----------------------------------------------------------------------------------------------------------------------


