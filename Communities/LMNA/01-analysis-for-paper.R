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

wt_auth()

# Species of interest
sp <- c("White-tailed Deer", "Moose", "Wolf")

# ----------------------------------------------------------------------------------------------------------------------

# Download data

lmna <- wt_get_projects(sensor = "CAM") |>
  filter(str_detect(project, "LMNA")) |>
  select(project, project_id)

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
                                               reports = "image_report")) |>
  # Fold the 'Bs' and other letters into their parent location
  mutate(location = case_when(
    str_detect(location, "-A1") ~ "LMCA-A1",
    str_detect(location, "-A4") ~ "LMCA-A4",
    str_detect(location, "-L20") ~ "LMCA-L20",
    TRUE ~ location
  ))

# ----------------------------------------------------------------------------------------------------------------------

# Treatment by location

loc <- lmna_main |>
  left_join(lmna) |>
  select(project, location) |>
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
  select(location, treatment, vegetation) |>
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

library(MASS)
library(broom)

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

# --- Pooled models (species × treatment interaction) ---
# Fitting one model per metric across both prey species is more efficient than
# separate per-species models: it pools variance estimation and directly tests
# whether the treatment effect differs between Moose and WTD via the interaction term.
# Model formula: species * treatment + vegetation + year_num + wolf_present + effort offset

pool_det <- glm.nb(
  n_detections ~ species_common_name * treatment + vegetation + year_num + wolf_present +
    offset(log(total_days)),
  data = model_data_year
)

pool_prop <- glm(
  cbind(months_detected, total_months - months_detected) ~
    species_common_name * treatment + vegetation + year_num + wolf_present,
  family = binomial,
  data = model_data_year
)

pool_img <- glm.nb(
  total_images ~ species_common_name * treatment + vegetation + year_num + wolf_present +
    offset(log(total_days)),
  data = model_data_year
)

# Extract results: exponentiated coefficients with Wald CIs
extract_pooled <- function(model, metric) {
  tidy(model, exponentiate = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(
      conf.low  = exp(log(estimate) - 1.96 * std.error),
      conf.high = exp(log(estimate) + 1.96 * std.error),
      metric    = metric
    ) |>
    dplyr::select(metric, term, estimate, conf.low, conf.high, p.value)
}

model_results <- bind_rows(
  extract_pooled(pool_det,  "Detections (NB + offset)"),
  extract_pooled(pool_prop, "Prop. monthly detections (Binomial)"),
  extract_pooled(pool_img,  "Images (NB + offset)")
) |>
  mutate(
    term = recode(term,
      "species_common_nameWhite-tailed Deer"                                = "Species: WTD",
      "treatmenthigh activity in situ"                                      = "Treatment: high activity",
      "vegetationtreedlow20"                                                = "Vegetation: treedlow20",
      "year_num"                                                            = "Year (numeric)",
      "wolf_present"                                                        = "Wolf present",
      "species_common_nameWhite-tailed Deer:treatmenthigh activity in situ" = "WTD × high activity"
    )
  )





