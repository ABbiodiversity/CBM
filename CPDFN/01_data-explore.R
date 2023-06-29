# ----------------------------------------------------------------------------------------------------------------------

# Title:   Explore CPDFN camera trap data
# Date:    June 2023
# Authors: Dave Evans, Marcus Becker

# ----------------------------------------------------------------------------------------------------------------------

library(wildRtrax)
library(keyring)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Simpler way - directly define variables
Sys.setenv(WT_USERNAME = "davidevans",
           WT_PASSWORD = "mypasword")

# Authenticate into WildTrax
wt_auth()

# ----------------------------------------------------------------------------------------------------------------------

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

# ----------------------------------------------------------------------------------------------------------------------
# View the data
View(cpdfn_data)

# Remove excess columns
cpdfn_data_slim <- cpdfn_data |> # This is the pipe syntax.
  # Kick out the NONES
  filter(!species_common_name == "NONE") |>
  # Select the columns that we're interested in.
  select(location, image_fov, image_date_time, individual_count,
         age_class, sex_class, species_common_name, tag_comments) |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Rabbits and hares" ~ "Snowshoe Hare",
    TRUE ~ species_common_name
  )) |>
  # Let's break tags into human vs wildlife
  mutate(wildlife = case_when(
    str_detect(species_common_name, "Human|Vehicle|Heavy|STAFF") ~ FALSE,
    TRUE ~ TRUE
  ))

# What are the number of tags by category?
tags <- cpdfn_data_slim |>
  group_by(species_common_name) |>
  summarise(total_tags = n()) |>
  arrange(desc(total_tags))

# What about by location?
locations <- cpdfn_data_slim |>
  filter(!species_common_name == "NONE") |>
  group_by(location) |>
  summarise(total_tags = n()) |>
  arrange(desc(total_tags)) |>
  View()

# ----------------------------------------------------------------------------------------------------------------------

comments <- cpdfn_data_slim |>
  select(tag_comment) |>
  group_by(tag_comment) |>
  tally()

# ----------------------------------------------------------------------------------------------------------------------

# First question - Human activity based on month.

df_human <- cpdfn_data_slim |>
  filter(wildlife == FALSE) |>
  select(location, image_date_time, tag = species_common_name, tag_comments) |>
  mutate(month = month(image_date_time, label = TRUE, abbr = FALSE)) |>
  mutate(tag = ifelse(tag == "Unidentified Vehicle", "Vehicle", tag)) |>
  filter(!tag == "STAFF/SETUP")

# Vector of unique locations in the data
locations <- unique(cpdfn_data_slim$location)

# Vector of unique tags in the data
tag <- unique(df_human$tag)[1:4]

month.name

# Dataframe of all combinations (locations, tags, and months)
df <- data.frame(
  location = locations) |>
  crossing(month.name, tag) |>
  rename(month = month.name)

# Summarise
summary <- df_human |>
  # filter(tag == "All Terrain Vehicle") |>
  group_by(location, tag, month) |>
  summarise(total_tags = n()) |>
  arrange(location)

# Find out which combinations of location and month are not represented
missing <- df |>
  anti_join(summary, by = c("location", "month")) |>
  mutate(# tag = "All Terrain Vehicle",
         total_tags = 0)

full_summary <- bind_rows(
  summary, missing) |>
  mutate(month = factor(month, levels = c("January", "February", "March",
                                          "April", "May", "June", "July",
                                          "August", "September", "October",
                                          "November", "December"))) |>
  arrange(location, month)

# ----------------------------------------------------------------------------------------------------------------------

# Visualization!!!!!

library(ggplot2)

# Plot the number of tags by month
ggplot(data = full_summary,
       mapping = aes(x = month, y = total_tags)) +
  geom_col() +
  facet_wrap(~ tag)




