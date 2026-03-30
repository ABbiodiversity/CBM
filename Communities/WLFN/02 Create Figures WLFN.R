# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2026 WLFN Camera Data Report
# Date:    March 2026
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Clear memory
rm(list=ls())
gc()

# Attach packages
library(tidyverse)
library(overlap)
library(fs)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Community
com_acr <- "WFL128"

# Load data
load(paste0(g_drive_cbme, "WFL128/Data/WFL128 Data Objects.RData"))

# Species of interest
sp_uni <- c("White-tailed Deer",
            "Black Bear",
            "Moose",
            "Coyote",
            "Snowshoe Hare",
            "Horse",
            "Red Fox")

species_colours <- c(
  "White-tailed Deer" = "#3A86FF",  # blue
  "Black Bear"        = "#6A4C93",  # deep purple
  "Moose"             = "#8D6E63",  # brown
  "Coyote"            = "#2A9D8F",  # teal
  "Snowshoe Hare"     = "#E63946",  # red
  "Horse"             = "#F4A261",  # sandy/orange
  "Red Fox"           = "#E76F51"   # fox orange-red
)

#-----------------------------------------------------------------------------------------------------------------------

# Deployment date ranges

location_ranges <- df_od |>
  mutate(project_location = paste0(project, "_", location)) |>
  group_by(project_location) |>
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  mutate(location = forcats::fct_reorder(location, start_date, .desc = TRUE)) |>
  mutate(analysis = "2024 WFL128 Wildlife Cameras")

fig_ranges <- ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location, color = analysis),
               linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = start_date, color = analysis), size = 4) +
  geom_point(aes(x = end_date, color = analysis), size = 4) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(color = "Project: ") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 15, hjust = 1, angle = 45),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

# View the figure
fig_ranges

h <- 8
w <- 7.5

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Deployment Ranges.png"),
       fig_ranges,
       width = w, height = h, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = paste0("Figures/", com_acr, "/Deployment Ranges.png"),
       fig_ranges,
       width = w, height = h, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

  fig_nimages <- nimages |>
    filter(species_common_name %in% sp_uni,
           #study == s,
           n > 50) |>
    mutate(species_common_name = fct_reorder(as.factor(species_common_name), n)) |>
    ggplot(mapping = aes(x = species_common_name, y = n, fill = species_common_name)) +
    geom_col(color = "black") +
    scale_fill_manual(values = species_colours) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma,
                      breaks = seq(0, 12000, 2000),
                      expand = expansion(mult = c(0, 0.03))) +
    labs(title = "",
         y = "Number of Images",
         x = "") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 16, margin = margin(0.75, 0, 0, 0, unit = "cm")),
          plot.title = element_text(size = 18))

fig_nimages

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Number of Images.png"),
         fig_nimages,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/", com_acr, "/Number of Images.png"),
         fig_nimages,
         width = 7, height = 5, dpi = 500, bg = "white")


#-----------------------------------------------------------------------------------------------------------------------

# Independent Detections

fig_ind_detect_all <- df_ind_detect |>
  filter(species_common_name %in% sp_uni) |>
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
ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = paste0("Figures/", com_acr, "/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Now we do each species individually

df_ind_detect_wc <- df_ind_detect

x_range <- range(df_ind_detect_wc$start_time, na.rm = TRUE) + c(-1, 1) * lubridate::days(1)

for (sp in sp_uni) {

  p <- df_ind_detect_wc |>
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
          axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 14, margin = margin(0, 0.5, 0, 0, unit = "cm")))

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/", com_acr, "/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------

# Temporal activity

output_dir <- paste0("Figures/", com_acr)

output_dir_drive <- paste0(g_drive_cbme, com_acr, "/Figures")

species <- sp_uni

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
                rug = FALSE,
                ylab = "Activity Level",
                main = sp)
    dev.off()
  }

}

# ----------------------------------------------------------------------------------------------------------------------

