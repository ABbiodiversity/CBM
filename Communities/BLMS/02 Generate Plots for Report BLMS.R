# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2024 BLMS Camera Data Report
# Date:    September 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(overlap)
library(fs)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Load data
load(paste0(g_drive_cbme, "BLMS/Data/BLMS Data Objects.RData"))

species <- c(
  "White-tailed Deer",
  "Black Bear",
  "Moose",
  "Coyote",
  "Snowshoe Hare",
  "Feral Horse")

species_colours <- c(
  "White-tailed Deer"  = "#3A86FF",
  "Snowshoe Hare"      = "#E63946",
  "Moose"              = "#06D6A0",
  "Black Bear"         = "#8338EC",
  "Feral Horse"              = "#FB5607",
  "Coyote"             = "#3366CC"
)

#-----------------------------------------------------------------------------------------------------------------------

# Deployment date ranges

location_ranges <- df_od |>
  group_by(project_location) |>
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  mutate(location = forcats::fct_reorder(location, start_date, .desc = TRUE)) |>
  mutate(grid = ifelse(str_detect(location, "^GL"), "Goose Lake", "Rubellite West"))

fig_ranges <- ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location, color = grid),
               linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = start_date, color = grid), size = 3) +
  geom_point(aes(x = end_date, color = grid), size = 3) +
  scale_color_manual(values = c("orange", "darkgreen")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
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
ggsave(filename = paste0(g_drive_cbme, "BLMS/Figures/Deployment Ranges.png"),
       fig_ranges,
       width = 5, height = 5.5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/BLMS/Deployment Ranges.png",
       fig_ranges,
       width = 5, height = 5.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

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
ggsave(filename = paste0(g_drive_cbme, "BLMS/Figures/Number of Images.png"),
       fig_nimages,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/BLMS/Number of Images.png",
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
ggsave(filename = paste0(g_drive_cbme, "BLMS/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = "Figures/BLMS/Independent Detections.png",
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Now we do each species individually

# Note: Filtering out the data in those two cameras that ran long.
# (Just for this visualization)
df_ind_detect <- df_ind_detect |>
  filter(start_time < as.Date("2024-12-01"))

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
  ggsave(filename = paste0(g_drive_cbme, "BLMS/Figures/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/BLMS/Independent Detections ", sp, ".png"),
         p,
         width = 7, height = 5, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------

# Temporal activity

output_dir <- "Figures/BLMS"

output_dir_drive <- paste0(g_drive_cbme, "BLMS/Figures")

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

# Grid Comparisons

species <- c("White-tailed Deer", "Moose", "Coyote",
             "Black Bear", "Snowshoe Hare", "Horse")

for (sp in species) {

  grid_dens |>
    filter(species_common_name == sp) |>
    mutate(grid = factor(grid, levels = c("Rubellite West", "Goose Lake"))) |>
    #mutate(across(density_avg:density_uci_0.9, ~ .x * 0.9)) |>
    ggplot(aes(x = grid, y = density_avg, fill = grid)) +
    geom_col(width = 0.5, color = "black") +
    #geom_linerange(aes(ymin = density_lci_0.9,
    #                   ymax = density_uci_0.9,
    #                   color = grid),
    #              linewidth = 0.5) +
    scale_fill_manual(values = c("orange", "darkgreen")) +
    #scale_color_manual(values = c("darkgreen", "orange")) +
    #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
    labs(y = expression(Density~(Animals~per~km^2)),
         #title = sp,
         x = "") +
    theme_minimal() +
    theme(legend.position = "none",
          #plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10))

  ggsave(paste0("Figures/BLMS/", sp, " Grid Comparison.png"),
         height = 4, width = 4, dpi = 500, bg = "white")

  ggsave(paste0(g_drive_cbme, "BLMS/Figures/", sp, " Grid Comparison.png"),
         height = 4, width = 4, dpi = 500, bg = "white")

}

# ----------------------------------------------------------------------------------------------------------------------








