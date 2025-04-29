# ----------------------------------------------------------------------------------------------------------------------

# Title:   Generate Plots for 2023-2024 MNA Region 1 Camera Data Report
# Date:    March 2025
# Authors: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(wildrtrax)

# Load data
load("Communities/MNA Region 1/MNA Region 1 Data Objects.RData")

species <- c("White-tailed Deer", "Black Bear", "Moose", "Coyote", "Snowshoe Hare",
             "Canada Lynx", "Woodland Caribou", "Gray Wolf")

species_colours <- c(
  "#4E6E58",  # deep green (forest)
  "#A17C6B",  # muted brown (soil / fur)
  "#79929D",  # soft blue-grey (sky / water)
  "#C2B280",  # dry grass / tan
  "#5B5B5B",  # dark grey (stone / shadow)
  "#7B9E87",  # sage green
  "#A9A9A9",  # light grey (mist)
  "#D6B88A"   # light brown (field / bark)
)

names(species_colours) <- species

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

remove <- c("Human", "STAFF/SETUP", "NONE", "Vehicle")

plot1 <- main_report |>
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
         n > 40) |>
  mutate(species_common_name = fct_reorder(as.factor(species_common_name), n)) |>
  ggplot(mapping = aes(x = species_common_name, y = n, fill = species_common_name)) +
  geom_col(color = "black") +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "",
       y = "Images",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, margin = margin(0.25, 0, 0, 0, unit = "cm")),
        plot.title = element_text(size = 18))

plot1

ggsave(filename = "Communities/MNA Region 1/Figures/Number of Images.png", plot1,
       width = 7, height = 5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Independent Detections

df_ind_detect <- main_report |>
  mutate(species_common_name = case_when(
    species_common_name == "Deer" ~ "White-tailed Deer",
    species_common_name == "Mule Deer" ~ "White-tailed Deer",
    species_common_name == "Bear" ~ "Black Bear",
    species_common_name == "Foxes" ~ "Red Fox",
    str_detect(species_common_name, "Rabbit") ~ "Snowshoe Hare",
    TRUE ~ species_common_name)) |>
  mutate(project_id = 2929) |>
  filter(species_common_name %in% species) |>
  wt_ind_detect(threshold = 30, units = "minutes")

plot2 <- df_ind_detect |>
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

plot2

ggsave(filename = "Communities/MNA Region 1/Figures/Independent Detections.png", plot2,
       width = 7, height = 7, dpi = 500, bg = "white")

# Now we do each species individually

for (sp in species) {

  plot3 <- df_ind_detect |>
    filter(species_common_name == sp) |>
    ggplot(mapping = aes(x = start_time, fill = species_common_name)) +
    geom_histogram(bins = 50) +
    labs(x = "",
         y = "Number of Detections") +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    scale_fill_manual(values = species_colours[sp]) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 13))

  ggsave(filename = paste0("Communities/MNA Region 1/Figures/Independent Detections ", sp, ".png"), plot3,
         width = 5, height = 3, dpi = 500, bg = "white")

}

#-----------------------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------------------





