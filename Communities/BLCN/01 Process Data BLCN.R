# ----------------------------------------------------------
# title:   Process BLCN camera data from WildTrax
#
# author:  Marcus Becker
# created: 2026-03-18
# inputs:
# outputs:
# notes:

# ----------------------------------------------------------

# 1.0 Initializing environment

# Clear memory
rm(list=ls())
gc()

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"
# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"
# Set path to Shared Google Drive (G Drive) - OSM BADR
g_drive_osm <- "G:/Shared drives/OSM BADR Mammals/"

# Load libraries
library(tidyverse)
library(wildrtrax)
library(sciCentRverse)
library(googledrive)
library(googlesheets4)
library(overlap)
library(activity)
library(corrplot)

# Source functions
files <- list.files("Functions", full.names = TRUE)
for (file in files) {
  source(file)
}

# Source wt credentials for authentication
source("wt_credentials.R")

# Authenticate
wt_auth()

# Google Drive authentication
drive_auth()
gs4_auth()

# Species of interest
sp_uni <- c("White-tailed Deer",
            "Black Bear",
            "Moose",
            "Coyote",
            "Snowshoe Hare",
            "Canada Lynx")

# ----------------------------------------------------------

# 2.0 Download BLCN data

# BLCN projects
blcn_proj <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(project, "BLCN")) |>
  select(project, project_id)

# BLCN project IDs
blcn_proj_ids <- blcn_proj$project_id

# Download main and image reports (as list object)
ls_blcn_reports <- map(.x = blcn_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        reports = c("main",
                                    "image_report",
                                    "location")
                      ))

# Prepare image reports
df_image_reports <-ls_blcn_reports |>
  # Flatten nested list
  list_flatten() |>
  # Keep only image reports
  (\(x) x[str_detect(names(x), "image_report\\.csv$")])() |>
  # Bind multiple elements into single df
  list_rbind() |>
  # Obtain `project` name
  left_join(blcn_proj, by = "project_id") |>
  # Keep only required columns
  select(project, project_id, location, location_id,
         image_id, image_date_time, image_fov,
         image_trigger_mode, equipment_model) |>
  # Note: CAM-497-3619 is BLCN14
  mutate(location = case_when(
    location == "CAM_497_3619" ~ "BLCN14",
    TRUE ~ location
  ))

loc_proj <- df_image_reports |>
  select(project, location) |>
  distinct()

# Prepare main reports
df_main_reports <-ls_blcn_reports |>
  # Flatten nested list
  list_flatten() |>
  # Keep only main reports
  (\(x) x[str_detect(names(x), "main_report\\.csv$")])() |>
  # Bind multiple elements into single df
  list_rbind() |>
  # Obtain `project` name
  left_join(blcn_proj, by = "project_id") |>
  # Keep only required columns
  select(project, project_id, location, location_id,
         image_id, image_date_time, species_common_name,
         individual_count, age_class, sex_class, tag_id) |>
  # Change species_common_name values
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  # Note: CAM-497-3619 is BLCN14
  mutate(location = case_when(
    location == "CAM_497_3619" ~ "BLCN14",
    TRUE ~ location
  ))

# Location reports
df_location_reports <- ls_blcn_reports |>
  list_flatten() |>
  # Keep only main reports
  (\(x) x[str_detect(names(x), "location_report\\.csv$")])() |>
  # Bind multiple elements into single df
  list_rbind() |>
  mutate(location = case_when(
    location == "CAM_497_3619" ~ "BLCN14",
    TRUE ~ location
  )) |>
  select(location, latitude, longitude) |>
  distinct() |>
  right_join(loc_proj) |>
  filter(!is.na(latitude))

# ----------------------------------------------------------

# 3.0 Obtain and summarise camera deployment operating days

df_od <- df_image_reports |>
  # First get operating days
  cam_get_op_days(
    grouping = c("location", "project"),
    missing_as = TRUE,
    span = "data")

df_od_summary <- df_od |>
  # Then summarise number of days by season
  cam_summarise_op_by_season(
    # Standard ABMI definitions of season (Julian day)
    seasons = c(spring = 99L,
                summer = 143L,
                winter = 288L),
    by_year = FALSE,
    wide = TRUE)

# ----------------------------------------------------------

# 4.0 Consolidate species tags in main report

df_main_reports_cons <- cam_consolidate_tags(
  report = df_main_reports
)

# ----------------------------------------------------------

# 5.0 Calculate time in front of camera

# 5.1 Parse tags into series, then calculate time for each

df_series <- df_main_reports_cons |>
  cam_calc_time_by_series(
    # 120 seconds apart delineates new series
    split_gap_secs = 120
  )

# 5.2 Sum total duration by species and location (camera)

df_tifc <- df_series |>
  cam_sum_total_time(
    # Define the same seasonal periods
    season_cutoffs = c(spring = 99L,
                       summer = 143L,
                       winter = 288L),
    # Operational days dataframe
    op_days_df = df_od_summary,
    # Species of interest
    species_universe = sp_uni
  )

# ----------------------------------------------------------

# 6.0 Calculate density

# 6.1 Obtain Effective Detection Distance (EDD) Categories

sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "BLCN")) |>
  select(id) |>
  pull()

df_blcn_edd <- read_sheet(ss = sheet_id) |>
  mutate(overall_category = paste0(primary_category, "_",
                                   secondary_category)) |>
  select(-c(primary_category, secondary_category)) |>
  left_join(loc_proj)

# 5.2 Extract camera model from image reports

df_model <- df_image_reports |>
  cam_extract_model_lookup(
    keys = c("project", "location"),
    model_col = "equipment_model",
    hf2_pattern = "HYPERFIRE 2 COVERT"
  )

# 5.3 Camera heights
df_height <- df_image_reports |>
  # All cameras were deployed at 1m
  mutate(height = "high") |>
  select(project, location, height) |>
  distinct()

# 5.4 Density calculation

df_density_sum <- df_tifc |>
  # Join camera model information
  left_join(df_model) |>
  # Join camera height information
  left_join(df_height) |>
  # Remove cameras with <30 total days of operation
  filter(total_season_days >= 30) |>
  # Calculate density
  cam_calc_density_by_loc(
    # EDD categories for each location
    edd_category_df = df_blcn_edd,
    # Camera field of view angle
    cam_fov_angle = 40,
    format = "long",
    include_project = TRUE,
    # Weighted average across seasons
    aggregate = TRUE,
    # Fill missing EDD values
    use_global_edd = TRUE,
    annotate_edd_source = FALSE) |>
  # Keep required columns only
  select(project, location,
         species_common_name, density_km2)

# ----------------------------------------------------------

# 6.0 Diel modeling

df <- df_main_reports |>
  filter(species_common_name %in% sp_uni) |>
  filter(!individual_count == "VNA") |>
  mutate(individual_count = as.numeric(individual_count),
         rad_time = posix2radian(image_date_time),
         julian = as.numeric(format(image_date_time, "%j")),
         date = as.Date(image_date_time)) |>
  uncount(weights = individual_count) |>
  left_join(df_location_reports) |>
  filter(!is.na(latitude))

solar_time <- solartime(dat = df$image_date_time,
                        lat = df$latitude,
                        lon = df$longitude,
                        tz = -6,
                        format = "%Y-%m-%d %H:%M:%S")

rad_time <- df |>
  mutate(solar_time = solar_time$solar) |>
  select(species_common_name, image_date_time, rad_time, solar_time)

#-----------------------------------------------------------------------------------------------------------------------

# 7.0 Generate Independent Detections

df_ind_detect <- df_main_reports |>
  left_join(blcn_proj) |>
  filter(species_common_name %in% sp_uni) |>
  wt_ind_detect(threshold = 30, units = "minutes")

#-----------------------------------------------------------------------------------------------------------------------

# 8.0 Species Co-Occurrences

corr <- df_ind_detect |>
  mutate(species_common_name = factor(species_common_name)) |>
  group_by(location, species_common_name, .drop = FALSE) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = location, names_from = species_common_name, values_from = n) |>
  select(-location)

M <- cor(corr)

#-----------------------------------------------------------------------------------------------------------------------

# 9.0 Number of images

remove <- c("STAFF/SETUP", "NONE", "Unidentified")

nimages_by_study <- df_main_reports |>
  # Study study variable
  mutate(study = ifelse(str_detect(project, "Winefred"),
                        "Winefred Lake Cameras",
                        "Beaver Lake Cree Nation")) |>
  group_by(study, species_common_name) |>
  tally() |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove) |>
  ungroup()

nimages_total <- df_main_reports |>
  group_by(species_common_name) |>
  tally() |>
  mutate(study = "Total") |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove) |>
  ungroup()

nimages <- bind_rows(nimages_by_study, nimages_total)

#-----------------------------------------------------------------------------------------------------------------------

# 10.0 Densities at OS stressors for comparison with OSM BADR regional monitoring

# From the Google Doc done by Dave E.

dens_blcn <- df_density_sum |>
  filter(project == "BLCN Cameras November 2025 Winefred Lake") |>
  # Categorize into treatments
  mutate(treatment = case_when(
    str_detect(location, "-C") ~ "Linear Features",
    str_detect(location, "-D") ~ "Linear Features",
    str_detect(location, "-E") ~ "Reference")) |>
  # 'JEM' (i.e., site) codes
  mutate(
    site = case_when(
      str_detect(location, "-C") ~ "C",
      str_detect(location, "-D") ~ "D",
      str_detect(location, "-E") ~ "E")) |>
  group_by(species_common_name, site, treatment) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features"))) |>
  # Add project
  mutate(project = "BLCN Winefred Lake Monitoring")

#-----------------------------------------------------------------------------------------------------------------------

# 11.0 Save results for future scripts

save(df_density_sum, # Densities by location and species
     dens_blcn, # Densities for OS stressors (regional and local)
     df_od_summary, # Number of operating days per location
     df_od, # Raw operating days
     df_location_reports, # Locations
     df_ind_detect, # Independent detections
     nimages, # Number of images per species
     df_image_reports, # Image report
     df_main_reports, # Main report
     rad_time, # Radian time
     M, # Species Co-Occurrences
     file = paste0(g_drive_cbme, "BLCN/Data/BLCN Data Objects.RData"))

#-----------------------------------------------------------------------------------------------------------------------



