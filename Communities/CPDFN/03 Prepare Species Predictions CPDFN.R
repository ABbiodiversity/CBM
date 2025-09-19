# ----------------------------------------------------------------------------------------------------------------------

# Title:   Prepare ABMI species predictions for CPDFN AOI
# Date:    September 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(sf)
library(terra)
library(mapview)
library(tidyverse)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"
# S (Science Centre) drive
s_drive <- "S:/samba/abmisc/AB_data_v2023"
# ABMI Mammals Shared Google Drive
g_drive_abmi <- "G:/Shared drives/ABMI Mammals"

# Species
species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Woodland Caribou", "Gray Wolf", "Canada Lynx")

# Load kgrid
load(file.path(s_drive, "kgrid", "kgrid_2.2.Rdata"))

# Load species habitat modeling results
load(file.path(g_drive_abmi, "Results", "Habitat Modeling", "2024", "2024 North Mammal Coefficients and Predictions_2025-07-22.RData"))

#-----------------------------------------------------------------------------------------------------------------------

# Area of Interest (AOI) for CPDFN - Prepared by D. Evans
aoi_cpdfn <- st_read(paste0(g_drive_cbme, "CPDFN/AOI/CPDFN Area of Interest.shp")) |>
  st_transform(3400)

# Dataframe for binding species predictions
kgrid_pred <- kgrid |>
  # Only need LinkID
  select(LinkID)

# Prepare species predictions
for (sp in species) {

  # Pull each species predictions from the results list
  preds <- data.frame(results[[sp]][["Predictions"]]) |>
    select(Landcover) |>
    rename_with(~ sp, .cols = Landcover)

  kgrid_pred <- bind_cols(kgrid_pred, preds)

}

# Prepare kgrid
kgrid_subset <- kgrid |>
  select(NrName, LinkID, X, Y) |>
  left_join(kgrid_pred, by = "LinkID") |>
  filter(!NrName == "Grassland") |>
  select(-NrName) |>
  st_as_sf(coords = c("X", "Y"), crs = st_crs(aoi_cpdfn))

# Spatial subset for the grid cells in the BLMS AOI
kgrid_aoi <- kgrid_subset[st_intersects(kgrid_subset, aoi_cpdfn, sparse = FALSE), ]

st_crs(kgrid_aoi)

# Now turn the data into a raster (for display in leaflet in report)

# First convert sf to SpatVector directly
kgrid_aoi_vect <- vect(kgrid_aoi)

# Create an empty raster grid (1km resolution)
template_rast <- terra::rast(kgrid_aoi_vect, resolution = 1000)

# Rasterize the prediction values and save as tif for each species
for (sp in species) {

  r <- terra::rasterize(kgrid_aoi_vect,
                        template_rast,
                        field = sp)

  # Re-project to WGS84
  r <- terra::project(r, "EPSG:4326")

  # Save raster
  terra::writeRaster(r,
                     filename = paste0(g_drive_cbme, "CPDFN/Predictions/", sp, " CPDFN.tif"),
                     overwrite = TRUE)

}

#-----------------------------------------------------------------------------------------------------------------------
