# ----------------------------------------------------------------------------------------------------------------------

# Title:   Analysis for the LMNA camera trap paper
# Date:    March 2025
# Authors: Marcus Becker

# ----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildrtrax)
library(tidyverse)
library(sciCentRverse)

source("wt_credentials.R")

# Authenticate
wt_auth()

# Species of interest
sp <- c("White-tailed Deer", "Moose", "Wolf")

# ----------------------------------------------------------------------------------------------------------------------

# Download data

lmna <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(project, "LMNA")) |>
  dplyr::select(project, project_id)

lmna_main <- map_df(.x = lmna$project_id,
                    .f = ~ wt_download_report(project_id = .x,
                                              sensor_id = "CAM",
                                              reports = "main")) |>
  # Fold the 'Bs' and other letters into their parent location
  mutate(location = case_when(
    str_detect(location, "-A1") ~ "LMCA-A1",
    str_detect(location, "-A4") ~ "LMCA-A4",
    str_detect(location, "-L20") ~ "LMCA-L20",
    TRUE ~ location
  )) |>
  filter(species_common_name %in% sp)

lmna_image <- map_df(.x = lmna$project_id,
                     .f = ~ wt_download_report(project_id = .x,
                                               sensor_id = "CAM",
                                               reports = "image_report") |>
                       dplyr::select(-image_snow)) |>
  # Fold the 'Bs' and other letters into their parent location
  mutate(location = case_when(
    str_detect(location, "-A1") ~ "LMCA-A1",
    str_detect(location, "-A4") ~ "LMCA-A4",
    str_detect(location, "-L20") ~ "LMCA-L20",
    TRUE ~ location
  ))

lmna_loc <- map_df(.x = lmna$project_id,
                   .f = ~ wt_download_report(project_id = .x,
                                             sensor_id = "CAM",
                                             reports = "location"))

# ----------------------------------------------------------------------------------------------------------------------

# Treatment by location

loc <- lmna_main |>
  left_join(lmna) |>
  dplyr::select(project, location) |>
  distinct() |>
  arrange(location, project) |>
  # Assign "treatments"
  mutate(treatment = case_when(
    str_detect(location, "-A|-S") ~ "reference",
    str_detect(location, "-D|-L") ~ "high activity in situ"
  )) |>
  # Assign "vegetation"
  mutate(vegetation = case_when(
    str_detect(location, "-A|-L") ~ "decidmix40",
    str_detect(location, "-S|-D") ~ "treedlow20"
  )) |>
  dplyr::select(location, treatment, vegetation) |>
  distinct()

# ----------------------------------------------------------------------------------------------------------------------

# Effort (i.e., number of days of operation)

days <- lmna_image |>
  cam_get_op_days(
    grouping   = c("location"),
    missing_as = TRUE,
    span       = "data"
  ) |>
  group_by(location) |>
  summarise(total_days = sum(operating))

# ----------------------------------------------------------------------------------------------------------------------

# Calculate metrics of interest

# 1. Independent detections

lmna_detections <- wt_ind_detect(
  x = lmna_main,
  threshold = 30,
  units = "minutes"
)

# Detections per unit of effort (detections per 100 camera-days)
det_per_effort <- lmna_detections |>
  group_by(location, species_common_name) |>
  summarise(n_detections = n(), .groups = "drop") |>
  complete(location, species_common_name, fill = list(n_detections = 0)) |>
  left_join(days, by = "location") |>
  mutate(detections_per_100_days = (n_detections / total_days) * 100)

# 2. Proportional monthly detections

# Operating months per location
operating_months_by_loc <- lmna_image |>
  mutate(month = floor_date(image_date_time, "month")) |>
  group_by(location) |>
  summarise(total_months = n_distinct(month), .groups = "drop")

# Proportion of months each species was detected at each location
prop_monthly_detections <- lmna_detections |>
  mutate(month = floor_date(start_time, "month")) |>
  group_by(location, species_common_name) |>
  summarise(months_detected = n_distinct(month), .groups = "drop") |>
  complete(location, species_common_name = sp, fill = list(months_detected = 0)) |>
  left_join(operating_months_by_loc, by = "location") |>
  mutate(prop_monthly_detections = months_detected / total_months)

# 3. Total number of images

lmna_images <- lmna_main |>
  group_by(location, species_common_name) |>
  summarise(total_images = n(), .groups = "drop") |>
  complete(location, species_common_name, fill = list(total_images = 0)) |>
  left_join(days, by = "location") |>
  mutate(images_per_100_days = (total_images / total_days) * 100)

# ----------------------------------------------------------------------------------------------------------------------

# Models: variation in metrics by treatment, controlling for vegetation
#
# NOTE on metric labeling: det_per_effort and lmna_images report "per 100 days" for
# descriptive/reporting purposes only. Models use raw counts (n_detections, total_images)
# with log(total_days) as an offset — this is statistically preferable to modelling the
# pre-normalised rate as a continuous response, because it correctly treats the data as
# counts and handles the effort adjustment on the log scale (matching the NB log link).

library(glmmTMB)
library(broom.mixed)

# --- Year-level data preparation ---
# Splitting each location into annual units increases effective N (15 locations -> 38
# location-years) and allows a year_num covariate to absorb any temporal trend.

loc_year <- lmna_image |>
  mutate(year = year(image_date_time)) |>
  distinct(location, year)

# Effort per location per year
days_year <- lmna_image |>
  cam_get_op_days(grouping = "location", missing_as = TRUE, span = "data") |>
  mutate(year = year(date)) |>
  group_by(location, year) |>
  summarise(total_days = sum(operating), .groups = "drop")

# Detections per location × year × species (zeros filled for undetected species)
det_year <- loc_year |>
  crossing(species_common_name = sp) |>
  left_join(
    lmna_detections |>
      mutate(year = year(start_time)) |>
      group_by(location, species_common_name, year) |>
      summarise(n_detections = n(), .groups = "drop"),
    by = c("location", "year", "species_common_name")
  ) |>
  replace_na(list(n_detections = 0))

# Images per location × year × species
img_year <- loc_year |>
  crossing(species_common_name = sp) |>
  left_join(
    lmna_main |>
      mutate(year = year(image_date_time)) |>
      group_by(location, species_common_name, year) |>
      summarise(total_images = n(), .groups = "drop"),
    by = c("location", "year", "species_common_name")
  ) |>
  replace_na(list(total_images = 0))

# Proportional monthly detections per location × year × species
op_months_year <- lmna_image |>
  mutate(year = year(image_date_time), month = floor_date(image_date_time, "month")) |>
  group_by(location, year) |>
  summarise(total_months = n_distinct(month), .groups = "drop")

prop_year <- loc_year |>
  crossing(species_common_name = sp) |>
  left_join(
    lmna_detections |>
      mutate(year = year(start_time), month = floor_date(start_time, "month")) |>
      group_by(location, species_common_name, year) |>
      summarise(months_detected = n_distinct(month), .groups = "drop"),
    by = c("location", "year", "species_common_name")
  ) |>
  replace_na(list(months_detected = 0)) |>
  left_join(op_months_year, by = c("location", "year"))

# --- Wolf presence covariate ---
# Wolf is too sparse to model directly (11/15 locations have zero detections overall),
# but its presence at a location-year can serve as a covariate for prey species models.
wolf_year <- det_year |>
  filter(species_common_name == "Wolf") |>
  mutate(wolf_present = as.integer(n_detections > 0)) |>
  dplyr::select(location, year, wolf_present)

# --- Combined year-level model data (prey species only) ---
model_data_year <- det_year |>
  filter(species_common_name != "Wolf") |>
  left_join(
    prop_year |> dplyr::select(location, species_common_name, year, months_detected, total_months),
    by = c("location", "species_common_name", "year")
  ) |>
  left_join(
    img_year |> dplyr::select(location, species_common_name, year, total_images),
    by = c("location", "species_common_name", "year")
  ) |>
  left_join(days_year, by = c("location", "year")) |>
  left_join(loc, by = "location") |>
  left_join(wolf_year, by = c("location", "year")) |>
  group_by(location) |>
  mutate(year_num = year - min(year)) |>
  ungroup() |>
  mutate(
    treatment           = factor(treatment, levels = c("reference", "high activity in situ")),
    vegetation          = factor(vegetation),
    species_common_name = factor(species_common_name, levels = c("Moose", "White-tailed Deer"))
  )

# Split into per-species data frames
md_moose <- model_data_year |> filter(species_common_name == "Moose")
md_wtd   <- model_data_year |> filter(species_common_name == "White-tailed Deer")

# --- Per-species GLMMs with random intercept for location ---
# Random intercept for location accounts for the repeated-measures structure
# (same location contributes multiple years). glmmTMB is used throughout for
# consistency across NB and binomial families.
# Formula: metric ~ treatment + vegetation + year_num + wolf_present + (1|location) [+ offset]

# Moose
moose_det  <- glmmTMB(
  n_detections ~ treatment + vegetation + year_num + wolf_present + offset(log(total_days)) +
    (1 | location),
  family = nbinom2, data = md_moose
)

moose_prop <- glmmTMB(
  cbind(months_detected, total_months - months_detected) ~
    treatment + vegetation + year_num + wolf_present + (1 | location),
  family = binomial, data = md_moose
)

moose_img  <- glmmTMB(
  total_images ~ treatment + vegetation + year_num + wolf_present + offset(log(total_days)) +
    (1 | location),
  family = nbinom2, data = md_moose
)

# White-tailed Deer
wtd_det  <- glmmTMB(
  n_detections ~ treatment + vegetation + year_num + wolf_present + offset(log(total_days)) +
    (1 | location),
  family = nbinom2, data = md_wtd
)

wtd_prop <- glmmTMB(
  cbind(months_detected, total_months - months_detected) ~
    treatment + vegetation + year_num + wolf_present + (1 | location),
  family = binomial, data = md_wtd
)

wtd_img  <- glmmTMB(
  total_images ~ treatment + vegetation + year_num + wolf_present + offset(log(total_days)) +
    (1 | location),
  family = nbinom2, data = md_wtd
)

# --- Extract fixed-effect results: exponentiated coefficients with Wald CIs ---
extract_glmm <- function(model, metric, species) {
  tidy(model, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(metric = metric, species = species) |>
    dplyr::select(species, metric, term, estimate, conf.low, conf.high, p.value)
}

term_labels <- c(
  "treatmenthigh activity in situ" = "Treatment: high activity",
  "vegetationtreedlow20"           = "Vegetation: treedlow20",
  "year_num"                       = "Year (numeric)",
  "wolf_present"                   = "Wolf present"
)

model_results <- bind_rows(
  extract_glmm(moose_det,  "Detections (NB + offset)",          "Moose"),
  extract_glmm(moose_prop, "Prop. monthly detections (Binomial)", "Moose"),
  extract_glmm(moose_img,  "Images (NB + offset)",               "Moose"),
  extract_glmm(wtd_det,    "Detections (NB + offset)",          "White-tailed Deer"),
  extract_glmm(wtd_prop,   "Prop. monthly detections (Binomial)", "White-tailed Deer"),
  extract_glmm(wtd_img,    "Images (NB + offset)",               "White-tailed Deer")
) |>
  mutate(term = recode(term, !!!term_labels))

# ----------------------------------------------------------------------------------------------------------------------

# Data visualization

# Forest plot: treatment effect (high activity in situ vs. reference) for Moose and WTD

plot_data <- model_results |>
  filter(term == "Treatment: high activity") |>
  mutate(
    metric_label = recode(metric,
      "Detections (NB + offset)"            = "Independent\ndetections",
      "Prop. monthly detections (Binomial)" = "Prop. monthly\ndetections",
      "Images (NB + offset)"                = "Images"
    ),
    metric_label = factor(metric_label, levels = c(
      "Images",
      "Prop. monthly\ndetections",
      "Independent\ndetections"
    )),
    # Significance label for annotation
    sig = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ paste0("p = ", round(p.value, 3)),
      p.value < 0.05  ~ paste0("p = ", round(p.value, 3)),
      TRUE            ~ paste0("p = ", round(p.value, 2))
    )
  )

ggplot(plot_data, aes(x = estimate, y = metric_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(label = sig), x = log10(0.13), hjust = 0, size = 2.9, colour = "grey30") +
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4, 8),
    labels = c("0.25", "0.5", "1", "2", "4", "8"),
    limits = c(0.13, 10)
  ) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    x = "Rate ratio (high activity vs. reference)",
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 10)
  )

