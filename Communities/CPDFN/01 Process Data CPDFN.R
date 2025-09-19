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

# Locations to remove (duplicated)
remove <- c("CPDFN12", "CPDFN14", "CPDFN19")

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
         tag_comments) |>
  filter(!location %in% remove) |>
  # Change species_common_name values
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    species_common_name == "Wolf" ~ "Gray Wolf",
    TRUE ~ species_common_name
  ))

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
         image_comments) |>
  filter(!location %in% remove)

# Locations
location_reports <- map_df(.x = project_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          reports = "location",
                          weather_cols = FALSE
                        )) |>
  select(location, latitude, longitude) |>
  distinct() |>
  filter(!location %in% remove)

# Other reports?

#-----------------------------------------------------------------------------------------------------------------------

# Step 2. Estimate density

# Deployment time periods
# First, get operating days (od):
df_od_summary <- get_operating_days(
  image_report = image_reports,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Also pull the raw od
df_od <- get_operating_days(
  image_report = image_reports,
  include_project = TRUE,
  summarise = FALSE,
  .abmi_seasons = TRUE
)

# Calculate time in front of camera (TIFC)

df_tifc <- main_reports |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Then sum by time period
  sum_total_time(sd = df_od_summary)

# Calculate density at each location

# EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "CPDFN")) |>
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

df <- main_reports |>
  filter(species_common_name %in% species) |>
  filter(!individual_count == "VNA") |>
  mutate(individual_count = as.numeric(individual_count),
         rad_time = posix2radian(image_date_time),
         julian = as.numeric(format(image_date_time, "%j")),
         date = as.Date(image_date_time)) |>
  uncount(weights = individual_count) |>
  left_join(location_reports) |>
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

# Generate Independent Detections

df_ind_detect <- main_reports |>
  left_join(projects) |>
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

remove <- c("Human", "STAFF/SETUP", "NONE", "Vehicle", "Unidentified", "All Terrain Vehicle",
            "Heavy Equipment", "Unidentified Vehicle")

nimages <- main_reports |>
  group_by(species_common_name) |>
  tally() |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove)

#-----------------------------------------------------------------------------------------------------------------------

# Tag and Image Comments - Non-Resident/Resident Hunter Analysis

tag_comments <- main_reports |>
  select(project, location, image_date_time, tag_comments) |>
  filter(!is.na(tag_comments))

image_comments <- image_reports |>
  select(project, location, image_date_time, image_id, image_comments) |>
  filter(!is.na(image_comments))

comments <- image_comments |>
  full_join(tag_comments, by = c("location", "image_date_time", "project"),
            relationship = "many-to-many") |>
  # If there is both, take the image comment.
  mutate(comment = ifelse(is.na(image_comments), tag_comments, image_comments)) |>
  # Lots of miscellaneous categories, let's focus on those that fit this pattern.
  filter(str_detect(comment, "^NR|^R")) |>
  mutate(species_common_name = case_when(
    str_detect(comment, "^NRH") ~ "Non-Resident Hunter",
    str_detect(comment, "^RH ") ~ "Resident Hunter",
    comment == "R" ~ "Resident",
    .default = "Non-Resident"
  )) |>
  mutate(project_id = project, individual_count = 1) |>
  # 10 minute threshold
  wt_ind_detect(threshold = 10, units = "minutes") |>
  select(project = project_id, location, detection, category = species_common_name, start_time) |>
  # Only the projects set up for this purpose
  filter(!str_detect(project, "CPFN 20th Baseline Cameras"))

#-----------------------------------------------------------------------------------------------------------------------

# Densities at OS stressors for comparison with OSM BADR regional monitoring

jem_raw <- tibble(
  JEM = c(
    # NEW
    "CPDFN-C1","CPDFN-C2","CPDFN-C3",
    # ORIGINAL
    "CPDFN-C4","CPDFN-C5","CPDFN-C6","CPDFN-C7","CPDFN-C8"
  ),
  Cameras = c(
    # NEW
    "C1 (on), C2 (on), C3 (off), C4 (off)",
    "C2 (on)",
    "C1 (on), C2 (off), C3 (off), C4 (on)",
    # ORIGINAL
    "C1 (off), C2 (on), C3 (on), C4 (on)",
    "C1 (on), C2 (off), C3 (off), C4 (on)",
    "C1 (on), C2 (off), C3 (off), C4 (off)",
    "C1 (on), C2 (off), C3 (on), C4 (on)",
    "C1 (off), C2 (off), C3 (on), C4 (on)"
  )
)

fine_scale_cpdfn <- jem_raw |>
  separate_rows(Cameras, sep = ",\\s*") |>
  mutate(
    cam = str_extract(Cameras, "C\\d+"),
    fine_scale = str_extract(Cameras, "(?<=\\().+?(?=\\))") |> str_to_title(),
    location = paste0(JEM, cam),
    jem_num = str_extract(JEM, "\\d+") |> as.integer(),
    cam_num = str_extract(cam, "\\d+") |> as.integer()
  ) |>
  arrange(jem_num, cam_num) |>
  select(location, fine_scale)

dens_cpdfn <- df_density_sum |>
  filter(str_detect(project, "20th"),
         location != "CPDFN-1") |>
  # Categorize into treatments
  mutate(treatment = case_when(
    str_detect(location, "-C1") ~ "Linear Features",
    str_detect(location, "-C3") ~ "Inactive Wellpads",
    str_detect(location, "-C2") ~ "Linear Features",
    TRUE ~ "Reference"
  )) |>
  left_join(fine_scale_cpdfn) |>
  # 'JEM' (i.e., site) codes
  mutate(
    site = case_when(
      str_detect(location, "CPDFN-C1") ~ "C1",
      str_detect(location, "CPDFN-C2") ~ "C2",
      str_detect(location, "CPDFN-C3") ~ "C3",
      TRUE ~ location)) |>
  group_by(site, species_common_name, treatment, fine_scale) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Inactive Wellpads"))) |>
  # Add project
  mutate(project = "CPDFN 20th Baseline Monitoring")

# OSM BADR Regional Monitoring Data

lure <- read_csv(paste0(g_drive_abmi, "Data/Lure/ABMI Lure Effect Summary 2024-04-24.csv")) |>
  mutate(species_common_name = str_replace_all(species_common_name, "(?<!^)([A-Z])", " \\1")) |>
  select(species_common_name, TA)

ref_fine_scale <- read_csv(paste0(g_drive_osm, "Results/ABMI BADR and EH Camera Data 2025-05-06.csv")) |>
  filter(treatment == "Reference") |>
  select(project, location, fine_scale) |>
  mutate(treatment = "reference") |>
  mutate(fine_scale_new = ifelse(fine_scale == "Near", "Off", "On")) |>
  select(-fine_scale)

dens_badr <- read_csv(paste0(g_drive_osm, "Results/Densities to use in the summaries.csv")) |>
  filter(str_detect(project, "ABMI OSM")) |>
  filter(treatment == "reference" | treatment == "dense linear features" | treatment == "low activity well pads") |>
  left_join(ref_fine_scale) |>
  mutate(fine_scale = ifelse(is.na(fine_scale), fine_scale_new, fine_scale)) |>
  select(-fine_scale_new) |>
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
  left_join(lure) |>
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  group_by(project, jem, landscape_unit, treatment, fine_scale, species_common_name) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(site = paste0(landscape_unit, "_", jem)) |>
  select(project, site, treatment, fine_scale, species_common_name, density_km2) |>
  mutate(treatment = case_when(
    treatment == "low activity well pads" ~ "Inactive Well Pads",
    treatment == "dense linear features" ~ "Linear Features",
    treatment == "reference" ~ "Reference"
  )) |>
  mutate(project = "OSM Regional Monitoring") |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Inactive Well Pads")))

# Bind together
df_density_os <- bind_rows(dens_badr, dens_cpdfn)

#-----------------------------------------------------------------------------------------------------------------------

# Step 3. Save results for future scripts

save(df_density_sum, # Densities by location and species
     df_density_os, # Densities for OS stressors (regional and local)
     df_od_summary, # Number of operating days per location
     df_od, # Raw operating days
     location_report, # Locations
     df_ind_detect, # Independent detections
     comments,
     nimages, # Number of images per species
     image_reports, # Image report
     main_reports, # Main report
     rad_time, # Radian time
     M, # Species Co-Occurrences
     file = paste0(g_drive_cbme, "CPDFN/Data/CPDFN Data Objects.RData"))

#-----------------------------------------------------------------------------------------------------------------------


