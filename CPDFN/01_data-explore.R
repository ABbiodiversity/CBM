# -------------------------------------------------

# Title:   Explore CPDFN camera trap data
# Date:    June 2023
# Authors: Dave Evans, Marcus Becker

# -------------------------------------------------

library(wildRtrax)
library(keyring)
library(dplyr)

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# -------------------------------------------------

reports <- wt_get_download_summary(sensor_id = "CAM")

cpdfn_id <- 1312

# Let's download the data!!

# Download tag report
cpdfn_data <- wt_download_report(
  project_id = cpdfn_id,
  sensor_id = "CAM",
  report = "tag",
  weather_cols = FALSE
)

# Download image report
cpdfn_image <- wt_download_report(
  project_id = cpdfn_id,
  sensor_id = "CAM",
  report = "image",
  weather_cols = FALSE
)

image_report <- cpdfn_image[["CPDFN_CPDFN_Wildlife_Cameras_image_report"]]

# Image comments
image_comments <- image_report |>
  filter(!image_comments == "") |>
  select(location, image_date_time, image_comments)

# -------------------------------------------------

# View the data
View(cpdfn_data)

# Remove excess columns
cpdfn_data_slim <- cpdfn_data |> # This is the pipe syntax.
  # Select the columns that we're interested in.
  select(location, image_fov, image_date_time, individual_count,
         age_class, sex_class, species_common_name, tag_comment) |>
  mutate(species_common_name = ifelse(species_common_name == "Deer",
                                      "White-tailed Deer",
                                      species_common_name))

# What are the number of tags by species?
tags <- cpdfn_data_slim |>
  group_by(species_common_name) |>
  summarise(total_tags = n()) |>
  arrange(desc(total_tags))

# -------------------------------------------------

comments <- cpdfn_data_slim |>
  select(tag_comment) |>
  group_by(tag_comment) |>
  tally()






