# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2025 CPDFN Camera Data Report
# Date:    September 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(overlap)
library(fs)

# Set path to Shared Google Drive (G Drive) - CBME Community Camera Results
g_drive_cbme <- "G:/Shared drives/CBME Community Camera Results/"

# Community
com_acr <- "CPDFN"

# Load data
load(paste0(g_drive_cbme, "CPDFN/Data/CPDFN Data Objects.RData"))

species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Canada Lynx", "Woodland Caribou", "Gray Wolf")

species_plus_human <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
                        "Canada Lynx", "Woodland Caribou", "Gray Wolf",
                        "Human", "All Terrain Vehicle", "Heavy Equipment", "Vehicle")

species_colours <- c(
  "White-tailed Deer"  = "#3A86FF",
  "Snowshoe Hare"      = "#E63946",
  "Moose"              = "#06D6A0",
  "Canada Lynx"        = "#FFBE0B",
  "Black Bear"         = "#8338EC",
  "Woodland Caribou"   = "#FB5607",
  "Coyote"             = "#3366CC",
  "Gray Wolf"          = "#FF006E",
  "Human"              = "gray50",
  "All Terrain Vehicle"   = "gray50",
  "Heavy Equipment"             = "gray50",
  "Vehicle"          = "gray50"
)

#-----------------------------------------------------------------------------------------------------------------------

# Deployment date ranges

location_ranges <- df_od |>
  group_by(project_location) |>
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  mutate(location = forcats::fct_reorder(location, start_date, .desc = TRUE)) |>
  mutate(analysis = ifelse(str_detect(project, "20th"), "Wildlife Abundance", "Human Land Use")) |>
  mutate(analysis = ifelse(location == "CPDFN-1", "Human Land Use", analysis))

fig_ranges <- ggplot(location_ranges, aes(y = location)) +
  geom_segment(aes(x = start_date, xend = end_date, yend = location, color = analysis),
               linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = start_date, color = analysis), size = 3) +
  geom_point(aes(x = end_date, color = analysis), size = 3) +
  scale_color_manual(values = c("orange", "darkgreen")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(color = "Project: ") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12, hjust = 1, angle = 45),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

# View the figure
fig_ranges

h <- 8.5
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

studies <- c("Total", "Human Land Use", "Wildlife Abundance")

for (s in studies) {

  fig_nimages <- nimages |>
    filter(species_common_name %in% species_plus_human,
           study == s,
           n > 100) |>
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

  # Save the figure to Google Drive
  ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Number of Images ", s, ".png"),
         fig_nimages,
         width = 7, height = 5, dpi = 500, bg = "white")

  # Save the figure to the Figures folder in the CBM repository
  ggsave(filename = paste0("Figures/", com_acr, "/Number of Images ", s, ".png"),
         fig_nimages,
         width = 7, height = 5, dpi = 500, bg = "white")

}

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
ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Independent Detections.png"),
       fig_ind_detect_all,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = paste0("Figures/", com_acr, "/Independent Detections.png"),
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
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_fill_manual(values = species_colours[sp]) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
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

# Resident vs Non-Resident Land Use (Hunters)

lims <- as.POSIXct(strptime(c("2022-02-01", "2023-12-01"), format = "%Y-%m-%d"))

text <- '<span style ="color:darkgreen;">Residents </span>vs <span style="color:#7570b3;">Non-Residents</span>'

# Plot the number of tags by month
fig_ind_detect_rh <- comments |>
  mutate(category = case_when(
    category == "Resident Hunter" ~ "CPDFN Hunter",
    category == "Resident" ~ "CPDFN Resident",
    TRUE ~ category)) |>
  mutate(category = factor(category, levels = c("CPDFN Resident", "CPDFN Hunter",
                                                 "Non-Resident", "Non-Resident Hunter"))) |>
  mutate(status = ifelse(str_detect(category, "CPDFN"), "CPDFN Residents", "Non-Residents")) |>
  ggplot(mapping = aes(x = start_time, fill = category)) +
  geom_histogram(position = "stack", bins = 80) +
  facet_wrap(~ status, nrow = 2) +
  labs(y = "Number of Detections") +
  scale_y_continuous(breaks = seq(0, 40, 10)) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_x_datetime(date_breaks = "4 months", date_labels = "%b %Y") +
  scale_fill_manual(
    values = c(`Non-Resident` = "grey", `Non-Resident Hunter` = "darkgreen",
               `CPDFN Resident` = "grey", `CPDFN Hunter` = "#7570b3"),
    breaks = c("Non-Resident Hunter", "CPDFN Hunter")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
        #axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        #legend.key.size = unit(0.4, "cm"),
        strip.text = element_text(size = 14, hjust = 0, margin = margin(b = 0.5, unit = "cm")),
        strip.background = element_blank())

# View the figure
fig_ind_detect_rh

# Save the figure to Google Drive
ggsave(filename = paste0(g_drive_cbme, com_acr, "/Figures/Category Detections.png"),
       fig_ind_detect_rh,
       width = 7, height = 5, dpi = 500, bg = "white")

# Save the figure to the Figures folder in the CBM repository
ggsave(filename = paste0("Figures/", com_acr, "/Category Detections.png"),
       fig_ind_detect_rh,
       width = 7, height = 5, dpi = 500, bg = "white")

# ----------------------------------------------------------------------------------------------------------------------

# Number of comments for each category



# ----------------------------------------------------------------------------------------------------------------------








