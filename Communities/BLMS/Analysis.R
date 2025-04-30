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

# LMNA projects
projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(organization == "BLMS") |>
  select(project, project_id)

ids <- projects$project_id

# Download data
df <- map_df(.x = ids, .f = ~ wt_download_report(.x, sensor_id = "CAM", report = "main"))

images <- wt_download_report(ids, "CAM", "image_report") |>
  select(project_id, location, image_date_time, image_trigger_mode, image_fov) |>
  left_join(projects)

check <- df |>
  select(location, longitude, latitude) |>
  distinct() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview::mapview(check)

df_clean <- df |>
  left_join(projects) |>
  consolidate_tags() |>
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, species_common_name, individual_count) |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    TRUE ~ species_common_name
  ))

unique(df_clean$species_common_name)

tags <- df_clean |>
  group_by(species_common_name) |>
  tally()

# ----------------------------------------------------------------------------------------------------------------------

# Deployment time periods

# Get operating days and summarise by season for each deployment (sd = season days)
df_sd_summary <- images |>
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
  # First calculate time by series
  calc_time_by_series() |>
  # Next calculate time in front of camera by location, deployment, species
  sum_total_time(sd = df_sd_summary)

unique(df_tt$location)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location
proj <- projects$project

# Get EDD Veg Category information from Google Sheets
df_vegdetdist <- images |>
  select(location) |>
  distinct() |>
  mutate(overall_category = "Forested_Open-Open",
         project_location = paste0(proj, "_", location)) |>
  select(-location)

df_vegdetdist <- read_csv("./BLMS/edd.csv") |>
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

# New plots

dens_sum |>
  filter(species_common_name == "Coyote") |>
  mutate(across(density_avg:density_uci_0.9, ~ .x * 0.9)) |>
  ggplot(aes(x = treatment, y = density_avg, fill = treatment)) +
  geom_col(width = 0.5) +
  #geom_linerange(aes(ymin = density_lci_0.9,
  #                   ymax = density_uci_0.9),
  #              linewidth = 0.5, color = "black") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(y = "",
       title = "Number of Coyote\n(animals per square km)",
       x = "") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))

ggsave("Coyote.png", height = 4, width = 4, dpi = 500, bg = "white")

dens_moose <- dens |>
  filter(species_common_name == "Coyote")

check <- df |>
  select(location, longitude, latitude) |>
  distinct() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  left_join(dens_moose) |>
  mutate(density_km2 = sqrt(density_km2))

# Create a color palette (yellow to red)
pal <- colorNumeric(palette = c("yellow", "red"),
                    domain = check$density_km2, na.color = "transparent")

leaflet(check) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(radius = 25,
                   color = ~ pal(density_km2),
                   stroke = FALSE,
                   fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = ~ density_km2[!is.na(density_km2)], title = "Number of Coyote")


#-----------------------------------------------------------------------------------------------------------------------

# Visualize

library(ggplot2)

dens_sum |>
  ggplot(aes(x = species_common_name, y = density_avg)) +
  geom_point(aes(color = treatment),
             size = 4,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = density_lci_0.9,
                     ymax = density_uci_0.9,
                     color = treatment),
                 linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("darkgreen", "orange")) +
  labs(x = "",
       y = "Abundance") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 14))

ggsave("Plot.png", width = 7, height = 5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

library(leaflet)
library(sf)
library(sp)
library(raster)
library(gstat)
library(dplyr)

loc1 <- df |>
  dplyr::select(location, latitude, longitude) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  filter(str_detect(location, "^GLR")) |>
  left_join(dens) |>
  filter(species_common_name == "Moose")

st_bbox(loc)

utm_zone <- floor((mean(st_coordinates(loc)[,1]) + 180) / 6) + 1
crs_utm <- paste0("EPSG:", 32600 + utm_zone)  # UTM North

points_utm <- st_transform(loc, crs_utm)

# Define grid resolution (in meters)
grid_res <- 100  # Adjust as needed (e.g., 500m x 500m cells)

# Create an empty raster matching the extent of the points
grid_extent <- extent(st_bbox(points_utm))  # Get bounding box of points
grid_utm <- raster(grid_extent, res = grid_res, crs = crs(points_utm))  # Create raster grid

# Convert to SpatialPointsDataFrame for gstat
points_sp <- as(points_utm, "Spatial")

# IDW Interpolation
idw_model <- gstat(formula = density_km2 ~ 1, locations = points_sp, nmax = 10, set = list(idp = 2))
idw_raster_utm <- interpolate(grid_utm, idw_model)

idw_raster_wgs84 <- projectRaster(idw_raster_utm, crs = "EPSG:4326")

#---

loc2 <- df |>
  dplyr::select(location, latitude, longitude) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  filter(!str_detect(location, "^GLR")) |>
  left_join(dens) |>
  filter(species_common_name == "Moose")

st_bbox(loc)

utm_zone <- floor((mean(st_coordinates(loc)[,1]) + 180) / 6) + 1
crs_utm <- paste0("EPSG:", 32600 + utm_zone)  # UTM North

points_utm <- st_transform(loc, crs_utm)

# Define grid resolution (in meters)
grid_res <- 100  # Adjust as needed (e.g., 500m x 500m cells)

# Create an empty raster matching the extent of the points
grid_extent <- extent(st_bbox(points_utm))  # Get bounding box of points
grid_utm <- raster(grid_extent, res = grid_res, crs = crs(points_utm))  # Create raster grid

# Convert to SpatialPointsDataFrame for gstat
points_sp <- as(points_utm, "Spatial")

# IDW Interpolation
idw_model <- gstat(formula = density_km2 ~ 1, locations = points_sp, nmax = 10, set = list(idp = 2))
idw_raster_utm <- interpolate(grid_utm, idw_model)

idw_raster_wgs84_2 <- projectRaster(idw_raster_utm, crs = "EPSG:4326")



pal <- colorNumeric("YlOrRd", domain = values(idw_raster_wgs84), na.color = "transparent")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(idw_raster_wgs84, colors = pal, opacity = 0.7) %>%
  addRasterImage(idw_raster_wgs84_2, colors = pal, opacity = 0.7) %>%
  #addLegend(pal = pal, values = values(idw_raster_wgs84), title = "Moose Density") %>%
  addCircleMarkers(data = loc1, radius = 5, fillColor = ~pal(density_km2),
                   color = "black", weight = 1, fillOpacity = 0.8,
                   popup = ~paste("Value:", loc1$density_km2, "<br>",
                                  "Location:", loc1$location)) %>%
  addCircleMarkers(data = loc2, radius = 5, fillColor = ~pal(density_km2),
                   color = "black", weight = 1, fillOpacity = 0.8,
                   popup = ~paste("Value:", loc2$density_km2, "<br>",
                                  "Location:", loc2$location))









