# ----------------------------------------------------------------------------------------------------------------------

# Title:   Process Data 2023-2024 OMGD19 Camera Data
# Date:    March 2025
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
# Internal SC package
# devtools::install_github("ABbiodiversity/sciCentRverse")
library(sciCentRverse)

# Set path to Shared Google Drive (G Drive) - ABMI Mammals
g_drive_abmi <- "G:/Shared drives/ABMI Mammals/"
# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"
# Set path to Shared Google Drive (G Drive) - OSM BADR
# This is for the comparison to ABMI OSM BADR results
g_drive_osm <- "G:/Shared drives/OSM BADR Mammals/"

# Species character strings
load(paste0(g_drive_abmi, "Data/Lookup Tables/WildTrax Species Strings.RData"))

# Source functions
files <- list.files("Functions", full.names = TRUE)
for (file in files) {
  source(file)
}

# Authenticate into WildTrax
source("wt_credentials.R")
wt_auth()

# Authenticate into Google Drive/Sheets
googledrive::drive_auth()
googlesheets4::gs4_auth()

# Define a list of species we care about for this project
sp_uni <- c("White-tailed Deer",
            "Black Bear",
            "Moose",
            "Coyote",
            "Snowshoe Hare",
            "Canada Lynx",
            "Woodland Caribou",
            "Gray Wolf")

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Download data

# OMGD19 Project(s)
projects <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(organization_name, "MNA Region 1")) |>
  select(project, project_id)

# Vector of project ids
project_ids <- projects$project_id

# There are now 2 projects, but the current report just uses the first.
# To-do: Incorporate the second year of data.
project_id <- 2929

# Main report(s) - this is a dataframe of the tags applied to the images
# Note: Don't need to loop, or use purrr, since there's only 1 project ID (for now)
main_report <- wt_download_report(project_id = project_id,
                                  sensor_id = "CAM",
                                  report = "main") |>
  # I like to keep the project name
  left_join(projects) |>
  # Consolidate tags of same species in the same image into one row
  cam_consolidate_tags() |>
  # Remove images that are out-of-range ("OOR") of the desired field-of-view
  # Note: in this project, there are actually no OOR images. But keep this line in here
  # just in case future data does.
  filter(!(image_fov %in% "OOR")) |>
  # Keep only relevant columns for downstream calculations
  select(project, project_id,
         location, location_id,
         image_date_time, species_common_name, individual_count, image_id)

# Image report - this is a dataframe of information on each image in the project.
image_report <- wt_download_report(project_id = project_id,
                                   sensor_id = "CAM",
                                   report = "image_report") |>
  left_join(projects) |>
  select(project, project_id,
         location, location_id,
         image_id, image_date_time, image_trigger_mode, image_fov, equipment_model)

# Location report - for mapping where the cameras are.
location_report <- wt_download_report(project_id = project_id,
                                      sensor_id = "CAM",
                                      report = "location") |>
  select(location, latitude, longitude)

#-----------------------------------------------------------------------------------------------------------------------

# Step 2. Estimate density

# We will use functions from the sciCentRverse package

# Deployment time periods
# First, get operating days (od):
df_od <- cam_get_op_days(
  df = image_report,
  grouping   = c("project_id", "project", "location_id", "location"),
  missing_as = TRUE,
  span       = "operational"
)

# Define season definitions (Julian day cutoffs)
seasons <- c(spring = 105L,
             summer = 125L,
             winter = 300L)

# Summarise the number of operational days by season
df_od_summary <- df_od |>
  cam_summarise_op_by_season(
    seasons  = seasons,
    by_year  = FALSE,
    wide     = TRUE   # one column per season + total_days
  )

# Calculate time in front of camera (TIFC)

df_tifc <- main_report |>
  # Turn 'Deer' tags into White-tailed Deer (most likely);
  # 'Bear' into Black Bear, 'Foxes' into Red Fox; 'Rabbit' into Snowshoe Hare
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  # First calculate time by series
  cam_calc_time_by_series() |>
  # Then sum by time period
  cam_sum_total_time(
    # Same seasonal cutoffs
    season_cutoffs = seasons,
    # Supply the operational day summary df
    op_days_df = df_od_summary,
    # Filter to only the species we care about
    species_universe = sp_uni
  )

# Obtain the camera models used in this project
df_model <- image_report |>
  cam_extract_model_lookup(
    keys      = c("project", "project_id", "location", "location_id"),
    model_col = "equipment_model"
  )

# Finally, calculate density at each location

# Load the EDD categories - stored on shared Google Drive
# EDD = "Effective Detection Distance"
# Note: Authenticate is done at the beginning of the script

# Obtain the proper sheet ID
# These lookup sheets are in: ABMI Mammals/Data/Detection Distance/EDD Categories By Project/
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "OMGD19")) |>
  select(id) |>
  pull()

# Read in EDD category data from the sheet
edd_cat <- map_df(.x = sheet_id,
                  .f = ~ read_sheet(ss = .x) |>
                  mutate(location = as.character(location))) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category)) |>
  mutate(overall_category = paste0(primary_category, "_", secondary_category)) |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  select(project_location, overall_category)

# Calculate density
df_density_long <- df_tifc |>
  left_join(df_model) |>
  # We have to specify the height of the camera; these were all deployed at 1m,
  # which corresponds to "high" in the internal package lookup table
  mutate(height = "high") |>
  cam_calc_density_by_loc(
    edd_category_df     = edd_cat,        # vegetation category per location
    cam_fov_angle       = 40,             # degrees
    format              = "long",
    aggregate           = TRUE,           # weighted mean across seasons
    use_global_edd      = TRUE,           # fall back to pooled EDD if needed
    annotate_edd_source = FALSE           # label EDD provenance
    )

#-----------------------------------------------------------------------------------------------------------------------

# Prepare data for diel modeling

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
  filter(species_common_name %in% sp_uni) |>
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

# Generate independent detections dataframe

df_ind_detect <- main_report |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name)) |>
  mutate(project_id = project_id) |>
  filter(species_common_name %in% sp_uni) |>
  # Use this function from the wildrtrax package
  wt_ind_detect(threshold = 30,
                units = "minutes")

#-----------------------------------------------------------------------------------------------------------------------

# Species Co-Occurrences

corr <- df_ind_detect |>
  filter(species_common_name %in% sp_uni) |>
  mutate(species_common_name = factor(species_common_name)) |>
  group_by(location, species_common_name, .drop = FALSE) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = location, names_from = species_common_name, values_from = n) |>
  select(-location)

M <- cor(corr)

#-----------------------------------------------------------------------------------------------------------------------

# Generate number of images dataframe

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

dens_omgd19 <- df_density_long |>
  # Categorize into treatments - this information comes from the CBME folks (Dave)
  # Note there are 4 clusters of cameras
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
  # Average across sites with the same treatment
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

# Read in the ABMI OSM BADR results from the other shared drive
dens_badr <- read_csv(
  paste0(g_drive_osm,
         "Data/Processed/ABMI OSM BADR ACME Industry Densities to Use in Summaries.csv")) |>
  # Only compare to ABMI OSM BADR projects
  filter(str_detect(project, "ABMI OSM")) |>
  # Filter to only the treatments present in the OMGD19 data
  filter(treatment == "reference" | treatment == "dense linear features" | treatment == "roads") |>
  # Turn from wide format to long
  pivot_longer(c(`Gray.Wolf`, Moose, `White.tailed.Deer`, `Snowshoe.Hare`, Coyote, `Canada.Lynx`, `Woodland.Caribou`, `Black.Bear`),
               names_to = "species_common_name",
               values_to = "density_km2") |>
  # Reorder columns
  select(project, location, species_common_name, density_km2, treatment,
         fine_scale, landscape_unit, jem, lure) |>
  # Rename species to match OMGD19 data
  mutate(species_common_name = case_when(
    species_common_name == "Gray.Wolf" ~ "Gray Wolf",
    species_common_name == "White.tailed.Deer" ~ "White-tailed Deer",
    species_common_name == "Black.Bear" ~ "Black Bear",
    species_common_name == "Canada.Lynx" ~ "Canada Lynx",
    species_common_name == "Woodland.Caribou" ~ "Woodland Caribou",
    species_common_name == "Snowshoe.Hare" ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  # Adjust fine_scale from metres distance to simple On/Off footprint
  mutate(fine_scale = case_when(
    fine_scale == "10-30" ~ "On",
    fine_scale == "100" ~ "Off",
    fine_scale == "300" ~ "Off",
    TRUE ~ fine_scale
  )) |>
  # Join lure effects
  left_join(lure) |>
  # Make lure adjustment for deployments that are lured
  mutate(density_km2 = ifelse(lure == "Yes", density_km2 / TA, density_km2)) |>
  group_by(project, jem, landscape_unit, treatment, species_common_name) |>
  summarise(density_km2 = mean(density_km2)) |>
  ungroup() |>
  mutate(site = paste0(landscape_unit, "_", jem)) |>
  select(project, site, treatment, species_common_name, density_km2) |>
  # Re-name treatments to match OMGD19
  mutate(treatment = case_when(
    treatment == "roads" ~ "Roads",
    treatment == "dense linear features" ~ "Linear Features",
    treatment == "reference" ~ "Reference",
    TRUE ~ treatment
  )) |>
  mutate(project = "OSM Regional Monitoring") |>
  mutate(treatment = factor(treatment, levels = c("Reference", "Linear Features", "Roads")))

# Bind together ABMI data and OMGD19 data
df_density_os <- bind_rows(dens_badr, dens_omgd19)

#-----------------------------------------------------------------------------------------------------------------------

# Step 3. Save results for future scripts

save(df_density_long, # Densities by location and species
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


