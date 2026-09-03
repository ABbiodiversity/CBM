# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2023-2024 OMGD19 Camera Data Report
# Date:    March 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(overlap)
library(fs)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Load data
load(paste0(g_drive_cbme, "OMGD19/Data/OMGD19 Data Objects.RData"))

species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Canada Lynx", "Woodland Caribou", "Gray Wolf")

species_colours <- c(
  "White-tailed Deer"  = "#3A86FF",
  "Snowshoe Hare"      = "#E63946",
  "Moose"              = "#06D6A0",
  "Canada Lynx"        = "#FFBE0B",
  "Black Bear"         = "#8338EC",
  "Woodland Caribou"   = "#FB5607",
  "Coyote"             = "#3366CC",
  "Gray Wolf"          = "#FF006E"
)

#-----------------------------------------------------------------------------------------------------------------------

# Locations summary

location_ranges <- df_od |>
  group_by(location) |>
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") |>
  mutate(location = forcats::fct_reorder(location, start_date))

ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location),
               linewidth = 2, color = "steelblue") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Camera Location",
       title = "Camera Operation Periods by Location") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank()
  )

# Not going to save this plot, not very interesting for this community, since all deployments
# were virtually identical.
# But will keep the code here.

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

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
  filter(!species_common_name %in% remove,
         n > 40)

fig_nimages <- nimages |>
  filter(species_common_name %in% species) |>
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
ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Number of Images.png"),
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/OMGD19/Number of Images.png",
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Independent Detections

fig_ind_detect_all <- df_ind_detect |>
  filter(species_common_name %in% species) |>
  ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
  geom_histogram(bins = 50) +
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
ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/OMGD19/Independent Detections.png",
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Now we do each species individually

x_range <- range(df_ind_detect$start_time, na.rm = TRUE) + c(-1, 1) * lubridate::days(1)

for (sp in species) {

  p <- df_ind_detect |>
    filter(species_common_name == sp) |>
    ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
    geom_histogram(bins = 50) +
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
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_fill_manual(values = species_colours[sp]) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 14, margin = margin(0, 0.5, 0, 0, unit = "cm")))

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, "OMGD19/Figures/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/OMGD19/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------

# Temporal activity

output_dir <- "Figures/OMGD19"

output_dir_drive <- paste0(g_drive_cbme, "OMGD19/Figures")

# Loop through each species
for (sp in species) {

  # Extract rad_time for the species
  times <- rad_time |>
    filter(species_common_name == sp) |>
    pull(rad_time)

  # Define output file path
  file_name <- paste0("Activity ", sp,  ".png")

  # File paths
  file_paths <- c(
    file.path(output_dir, file_name),
    file.path(output_dir_drive, file_name))

  # Save the plot in both locations
  for (path in file_paths) {
    png(path, width = 800, height = 600, res = 120)
    densityPlot(times,
                col = species_colours[[sp]],
                lty = 1,
                lwd = 3,
                main = sp)
    dev.off()
  }

}

#-----------------------------------------------------------------------------------------------------------------------
