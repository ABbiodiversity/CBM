# ----------------------------------------------------------------------------------------------------------------------

# Title:   Process Data GLM
# Date:    November 2025
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
source("wt_credentials.R")

wt_auth()

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Download data

# CPDFN Project(s)
projects <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(organization_name, "GLMS")) |>
  select(project, project_id)

# Let's deal with the latest one for now
project_ids <- 3291

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
  select(project, location, image_date_time, species_common_name, individual_count, image_id) |>
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
  select(project, location, image_id, image_date_time, image_trigger_mode, image_fov)

# Locations
location_reports <- map_df(.x = project_ids,
                           .f = ~ wt_download_report(
                             project_id = .x,
                             sensor_id = "CAM",
                             reports = "location",
                             weather_cols = FALSE
                           )) |>
  select(location, latitude, longitude) |>
  distinct()

# Other reports?

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

species_colours <- c(
  "White-tailed Deer"  = "#3A86FF",
  "Snowshoe Hare"      = "#E63946",
  "Moose"              = "#06D6A0",
  "Canada Lynx"        = "#FFBE0B",
  "Black Bear"         = "#8338EC",
  "Coyote"             = "#3366CC",
  "Squirrels" = "#FF006E",
  "Elk (wapiti)" = "#FB5607",
  "Gray Wolf" = "darkgreen"
)

species <- c("White-tailed Deer", "Moose", "Elk (wapiti)",
             "Black Bear", "Gray Wolf")

remove <- c("Human", "STAFF/SETUP", "NONE", "Vehicle", "Unidentified")

nimages <- main_reports |>
  group_by(species_common_name) |>
  tally() |>
  arrange(desc(n)) |>
  filter(!species_common_name %in% remove,
         n > 20)

fig_nimages <- nimages |>
  mutate(species_common_name = ifelse(
    str_detect(species_common_name, "Squirrels"),
    "Squirrels",
    species_common_name
  )) |>
  #filter(species_common_name %in% species) |>
  mutate(species_common_name = fct_reorder(as.factor(species_common_name), n)) |>
  ggplot(mapping = aes(x = species_common_name, y = n, fill = species_common_name)) +
  geom_col(color = "black") +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "",
       y = "Number of Images",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, margin = margin(0.75, 0, 0, 0, unit = "cm")),
        plot.title = element_text(size = 18))

# View the figure
fig_nimages

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, "GLMS/Figures/Number of Images.png"),
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/GLMS/Number of Images.png",
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Independent detections

df_ind_detect <- main_reports |>
  mutate(project_id = project_ids) |>
  #filter(species_common_name %in% species) |>
  wt_ind_detect(threshold = 30, units = "minutes")

fig_ind_detect_all <- df_ind_detect |>
  filter(species_common_name %in% species) |>
  ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
  geom_histogram(bins = 30) +
  labs(x = "",
       y = "Number of Detections") +
  facet_wrap(~ species_common_name, scales = "free_y", nrow = 4) +
  scale_y_continuous(
    breaks = function(x) {
      if (max(x, na.rm = TRUE) <= 1.5) {
        c(0, 1)
      } else {
        scales::breaks_extended()(x, 4)
      }
    }
  ) +
  scale_fill_manual(values = species_colours) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        strip.text = element_text(size = 14),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16))

fig_ind_detect_all

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, "GLMS/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/GLMS/Independent Detections.png",
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Now we do each species individually

x_range <- range(df_ind_detect$start_time, na.rm = TRUE) + c(-1, 1) * lubridate::days(1)

for (sp in species) {

  p <- df_ind_detect |>
    filter(species_common_name == sp) |>
    ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
    geom_histogram(bins = 30) +
    labs(x = "",
         y = "Number of Detections") +
    scale_y_continuous(
      limits = function(x) {
        c(0, max(5, ceiling(max(x, na.rm = TRUE))))
      },
      breaks = scales::pretty_breaks(n = 5)
    ) +
    scale_x_datetime(
      limits = x_range,
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_fill_manual(values = species_colours[sp]) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 14, margin = margin(0, 0.5, 0, 0, unit = "cm")))

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, "GLMS/Figures/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/GLMS/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------

# Deployment date ranges

location_ranges <- image_reports |>
  group_by(location) |>
  summarise(start_date = as.Date(min(image_date_time)),
            end_date = as.Date(max(image_date_time)), .groups = "drop") |>
  mutate(location = forcats::fct_reorder(location, start_date, .desc = TRUE))

fig_ranges <- ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location),
               linewidth = 1.5, lineend = "round",
               color = "cornflowerblue") +
  geom_point(aes(x = start_date), size = 3, color = "cornflowerblue") +
  geom_point(aes(x = end_date), size = 3, color = "cornflowerblue") +
  #scale_color_manual(values = c("cornflowerblue")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 13)
  )

# View the figure
fig_ranges

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, "GLMS/Figures/Deployment Ranges.png"),
       fig_ranges,
       width = 6, height = 5.5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/GLMS/Deployment Ranges.png",
       fig_ranges,
       width = 6, height = 5.5, dpi = 500, bg = "white")




