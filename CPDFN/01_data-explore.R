# ----------------------------------------------------------------------------------------------------------------------

# Title:   Explore CPDFN camera trap data
# Date:    June 2023
# Authors: Dave Evans, Marcus Becker

# ----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(wildRtrax) # For downloading data
library(keyring) # Storing credentials
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Simpler way - directly define variables
Sys.setenv(WT_USERNAME = "davidevans",
           WT_PASSWORD = "mypasword")

# Authenticate into WildTrax
wt_auth()

# ----------------------------------------------------------------------------------------------------------------------

# Obtain vector of CPDFN report ids
reports <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(organization == "CPDFN") |>
  select(project_id) |>
  pull() # 1312 and 2259

# Stick to the first year of data for now
report <- 1312

# Download data

# Tag report:
cpdfn_tags <- wt_download_report(project_id = report,
                                 sensor_id = "CAM",
                                 report = "main",
                                 weather_cols = FALSE)

# What are the tag comments?
check <- cpdfn_tags |>
  group_by(tag_comments) |>
  tally()

# Download image report
cpdfn_image <- wt_download_report(project_id = report,
                                  sensor_id = "CAM",
                                  report = "image_report",
                                  weather_cols = FALSE)

check1 <- cpdfn_image |>
  group_by(image_comments) |>
  tally()

# Image comments
image_comments <- cpdfn_image |>
  filter(!image_comments == "") |>
  select(location, image_date_time, image_id, image_comments)

# ----------------------------------------------------------------------------------------------------------------------

# Remove excess columns
cpdfn_data_slim <- cpdfn_tags |> # This is the pipe syntax.
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
  arrange(desc(total_tags))

# ----------------------------------------------------------------------------------------------------------------------

tag_comments <- cpdfn_data_slim |>
  select(location, image_date_time, tag_comments) |>
  filter(!is.na(tag_comments))

# Are the tag and image comments related?
detections <- image_comments |>
  full_join(tag_comments, by = c("location", "image_date_time")) |>
  mutate(comment = ifelse(is.na(image_comments), tag_comments, image_comments)) |>
  filter(str_detect(comment, "^NR|^R")) |>
  mutate(species_common_name = case_when(
    str_detect(comment, "^NRH") ~ "Non-Resident Hunter",
    str_detect(comment, "^RH ") ~ "Resident Hunter",
    comment == "R" ~ "Resident",
    .default = "Non-Resident"
  )) |>
  mutate(project_id = "CPDFN", individual_count = 1) |>
  wt_ind_detect(threshold = 10, units = "minutes") |>
  select(detection, location, category = species_common_name, start_time)

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

# Visualization

library(ggplot2)
library(abmi.themes)
add_abmi_fonts()
library(ggtext)

lims <- as.POSIXct(strptime(c("2022-02-01", "2022-12-01"), format = "%Y-%m-%d"))

cool_text <- '<span style="color:#4682b4;">**European**</span> countries<br>account for 49.8%
of all runners<br>among the top 10 countries.<br><br>There have been<br>1,427 ultra runners from
<span style="color:#FF0000;">**Canada**</span>.'

text <- '<span style ="color:darkgreen;">Residents </span>vs <span style="color:#7570b3;">Non-Residents</span>'


# Plot the number of tags by month
detections |>
  mutate(status = str_remove(category, " Hunter$")) |>
  ggplot(mapping = aes(x = start_time, fill = category)) +
  geom_histogram(position = "stack", bins = 50) +
  facet_wrap(~ status, nrow = 2) +
  labs(y = "Number of Detections",
       title = "Detections at CPDFN Cameras in 2022") +
  scale_y_continuous(breaks = seq(0, 60, 20), limits = c(0, 60)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", limits = lims) +
  #annotate(geom = "richtext", y = 42, x = as.POSIXct("2022-03-15", format = "%Y-%m-%d"), label = text,
  #         label.colour = NA, fill = NA, fontface = "bold", color = "grey20", size = 4) +
  scale_fill_manual(
    values = c(`Non-Resident` = "grey", `Non-Resident Hunter` = "darkgreen",
               Resident = "grey", `Resident Hunter` = "#7570b3"),
    breaks = c("Non-Resident Hunter", "Resident Hunter")) +
  #scale_fill_manual(values = c("#7570b3", "darkgreen")) +
  #guides(fill = guide_legend(override.aes = list(size = 11))) +
  theme_abmi() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0)),
        #axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        #panel.spacing.y = unit(0, "lines"),
        strip.text = element_text(size = 11, color = "black", face = "bold", hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(size = 15))

ggsave("image.png")

# ----------------------------------------------------------------------------------------------------------------------

sites <- unique(cpdfn_data_slim$location)
category <- c("Non-Resident", "Resident", "Non-Resident Hunter", "Resident Hunter")

d <- data.frame(
  location = sites) |>
  crossing(category)

d1 <- detections |>
  group_by(location, category) |>
  tally()

d2 <- d |>
  anti_join(d1, by = c("location", "category")) |>
  mutate(n = 0) |>
  bind_rows(d1)

d2 |>
  mutate(location = factor(location, levels = c("CPDFN-1", "CPDFN-2", "CPDFN-3",
                                                "CPDFN-4", "CPDFN-5", "CPDFN-6", "CPDFN-7",
                                                "CPDFN-8", "CPDFN-9", "CPDFN-10",
                                                "CPDFN-11", "CPDFN-12", "CPDFN-13", "CPDFN-14",
                                                "CPDFN-15", "CPDFN-16", "CPDFN-17", "CPDFN-19"))) |>
  mutate(status = str_remove(category, " Hunter$")) |>
  #filter(str_detect(category, "Hunter")) |>
  ggplot(mapping = aes(x = location, y = n, fill = status)) +
  geom_col(position = "stack") +
  labs(y = "Number of Detections",
       title = "Total Detections at CPDFN Cameras in 2022") +
  scale_y_continuous(breaks = seq(0, 60, 20), limits = c(0, 60)) +
  annotate(geom = "richtext", y = 55, x = 4, label = text,
           label.colour = NA, fill = NA, fontface = "bold", color = "grey20", size = 4.75) +
  scale_fill_manual(values = c("#7570b3", "darkgreen")) +
  theme_abmi() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0)),
        #axis.text.y = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        #panel.spacing.y = unit(0, "lines"),
        strip.text = element_text(size = 11, color = "black", face = "bold", hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(size = 15))

# ----------------------------------------------------------------------------------------------------------------------

# Animals

cpdfn_data_slim |>
  filter(str_detect(species_common_name, "White-tailed Deer|Black Bear|Moose|Gray Wolf")) |>
  mutate(project_id = "CPDFN") |>
  wt_ind_detect(threshold = 30, unit = "minutes") |>
  ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ species_common_name, nrow = 4, scales = "free_y") +
  labs(y = "Number of Detections",
       title = "Wildlife Detections at CPDFN Cameras in 2022") +
  #scale_y_continuous(breaks = seq(0, 60, 20), limits = c(0, 60)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", limits = lims) +
  #annotate(geom = "richtext", y = 42, x = as.POSIXct("2022-03-15", format = "%Y-%m-%d"), label = text,
  #         label.colour = NA, fill = NA, fontface = "bold", color = "grey20", size = 4) +
  scale_fill_viridis_d() +
  #guides(fill = guide_legend(override.aes = list(size = 11))) +
  theme_abmi() +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        #axis.text.y = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        #panel.spacing.y = unit(0, "lines"),
        strip.text = element_text(size = 11, color = "black", face = "bold", hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(size = 16))








